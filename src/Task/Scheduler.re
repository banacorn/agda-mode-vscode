open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  module ViewHandler = Handle__View.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  module Request = Request.Impl(Editor);

  type t = Runner.t(Command.t);

  // open! Task;

  // Task Runner
  let dispatchCommand = (self, command) => Runner.push(self, command);

  let rec sendRequest =
          (state, request: Task.request): Promise.t(array(Task.request)) => {
    // Task.SendRequest will be deferred and executed until the current request is handled
    let derivedRequests = ref([||]);

    let runner = Runner.make(task => {runTask(state, task)});

    // handle of the connection response listener
    let handle = ref(None);
    let handler =
      fun
      | Error(error) => {
          let tasks = ErrorHandler.handle(Error.Connection(error));
          Runner.pushMany(runner, List.toArray(tasks))
          ->Promise.get(() => {Runner.terminate(runner)});
        }
      | Ok(Parser.Incr.Event.Yield(Error(error))) => {
          let tasks = ErrorHandler.handle(Error.Parser(error));
          Runner.pushMany(runner, List.toArray(tasks))
          ->Promise.get(() => {Runner.terminate(runner)});
        }
      | Ok(Yield(Ok(response))) => {
          // Task.SendRequest are filtered out
          let otherTasks =
            List.toArray(ResponseHandler.handle(response))
            ->Array.keep(task =>
                switch (Task.classify(task)) {
                | Some(req) =>
                  Js.Array.push(req, derivedRequests^)->ignore;
                  false;
                | None => true
                }
              );
          Runner.pushMany(runner, otherTasks)->ignore;
        }
      | Ok(Stop) => Runner.terminate(runner);

    let promise = runner.terminationPromise;

    switch (request) {
    | Task.Agda(req) =>
      state
      ->State.sendRequestToAgda(req)
      ->Promise.flatMap(
          fun
          | Ok(connection) => {
              handle := Some(connection.Connection.emitter.on(handler));
              promise;
            }
          | Error(error) => {
              let tasks = ErrorHandler.handle(error);
              runTasks(state, tasks)->Promise.flatMap(() => promise);
            },
        )
      ->Promise.tap(() => (handle^)->Option.forEach(f => f()))
      ->Promise.map(() => derivedRequests^)
    | View(req) =>
      state->State.sendRequestToView(req)->Promise.map(() => derivedRequests^)
    };
  }
  and sendRequests = (state, requests: list(Task.request)): Promise.t(unit) =>
    switch (requests) {
    | [] => Promise.resolved()
    | [x, ...xs] =>
      sendRequest(state, x)
      ->Promise.flatMap(xs' =>
          sendRequests(state, List.concat(List.fromArray(xs'), xs))
        )
    }
  and runTask = (state, task: Task.t): Promise.t(unit) => {
    switch (task) {
    | Task.Terminate =>
      Js.log("[ task ][ terminate ] ");
      State.destroy(state);
    | WithState(callback) =>
      Js.log("[ task ][ with state ] ");
      callback(state)->Promise.flatMap(runTasks(state));
    | Goal(action) =>
      Js.log("[ task ][ goal ] ");
      let tasks = GoalHandler.handle(action);
      runTasks(state, tasks);
    | SendRequest(request) =>
      Js.log("[ task ][ agda request ]");
      sendRequests(state, [Agda(request)]);
    | ViewReq(request) =>
      Js.log("[ task ][ view request ] ");
      sendRequests(state, [View(request)]);
    | ViewRes(response) =>
      Js.log("[ task ][ view response ] ");
      switch (response) {
      | View.Response.InquiryResult(result) =>
        Js.log("!!!");
        state.onViewInquiryResponse.emit(result);
      | _ => ()
      };
      let tasks = ViewHandler.handle(response);
      runTasks(state, tasks);
    | ViewListener(callback) =>
      Js.log("[ task ][ view inquiry result listener ] ");
      state.onViewInquiryResponse.once()
      ->Promise.tap(Js.log)
      ->Promise.flatMap(callback)
      ->Promise.flatMap(runTasks(state));
    | Error(error) =>
      Js.log("[ task ][ view error ] ");
      let tasks = ErrorHandler.handle(error);
      runTasks(state, tasks);
    | Debug(message) =>
      Js.log("[ debug ] " ++ message);
      runTasks(state, [Task.displayWarning("Debug", Some(message))]);
    };
  }
  and runTasks = (state, tasks: list(Task.t)): Promise.t(unit) =>
    switch (tasks) {
    | [] => Promise.resolved()
    | [x, ...xs] =>
      runTask(state, x)->Promise.flatMap(() => runTasks(state, xs))
    };

  let make = state => {
    Runner.make(command => {
      let tasks = CommandHandler.handle(command);
      runTasks(state, tasks);
    });
  };

  // destroy only after all tasks have been executed
  let destroy = (runner: Runner.t(Command.t)): Promise.t(unit) =>
    runner.terminationPromise;
};