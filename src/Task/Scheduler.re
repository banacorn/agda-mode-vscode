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
  let interrupt = (self, command) => {
    Js.log("[ scheduler ][ interrupt! ] " ++ Command.toString(command));
    Runner.interrupt(self, command);
  };

  let rec runTask = (state, task: Task.t): Promise.t(list(Task.request)) => {
    switch (task) {
    | Task.Terminate =>
      Js.log("[ task ][ terminate ] ");
      State.destroy(state)->Promise.map(() => []);
    | DispatchCommand(_) => Promise.resolved([])
    | WithState(callback) =>
      callback(state)->Promise.flatMap(runTasks(state))
    | Goal(action) =>
      Js.log("[ task ][ goal ] ");
      let tasks = GoalHandler.handle(action);
      runTasks(state, tasks);
    | SendRequest(request) =>
      Js.log("[ task ][ agda request ]");
      Promise.resolved([Task.Agda(request)]);
    // sendRequests(state, [Agda(request)]);
    | ViewReq(request, callback) =>
      Js.log("[ task ][ view request ] ");
      // sendRequests(state, [View(request, callback)]);
      Promise.resolved([Task.View(request, callback)]);
    | ViewEvent(event) =>
      Js.log("[ task ][ view event ] ");
      let tasks =
        switch (event) {
        | Initialized => []
        | Destroyed => [Task.Terminate]
        };
      runTasks(state, tasks);
    | Error(error) =>
      Js.log("[ task ][ view error ] ");
      let tasks = ErrorHandler.handle(error);
      runTasks(state, tasks);
    | Debug(message) =>
      Js.log("[ debug ] " ++ message);
      runTasks(state, [Task.displayWarning("Debug", Some(message))]);
    };
  }
  and runTasks =
      (state, tasks: list(Task.t)): Promise.t(list(Task.request)) =>
    switch (tasks) {
    | [] => Promise.resolved([])
    | [x, ...xs] =>
      runTask(state, x)
      ->Promise.flatMap(reqs =>
          runTasks(state, xs)
          ->Promise.map(moreReqs => List.concat(reqs, moreReqs))
        )
    };

  let rec sendRequest =
          (state, request: Task.request): Promise.t(list(Task.request)) => {
    // Task.SendRequest will be deferred and executed until the current request is handled
    let derivedRequests = ref([||]);

    let runner =
      Runner.make(task => {
        runTask(state, task)
        ->Promise.map(requests => {
            derivedRequests :=
              Js.Array.concat(List.toArray(requests), derivedRequests^);
            ();
          })
      });

    let filterReqeusts = tasks =>
      tasks->Array.keep(task =>
        switch (Task.classify(task)) {
        | Some(req) =>
          Js.Array.push(req, derivedRequests^)->ignore;
          false;
        | None => true
        }
      );

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
            List.toArray(ResponseHandler.handle(response))->filterReqeusts;
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
              runTasks(state, tasks)
              ->Promise.flatMap(requests => {
                  derivedRequests :=
                    Js.Array.concat(List.toArray(requests), derivedRequests^);
                  promise;
                });
            },
        )
      ->Promise.tap(() => (handle^)->Option.forEach(f => f()))
      ->Promise.map(() => List.fromArray(derivedRequests^))
    | View(req, callback) =>
      state
      ->State.sendRequestToView(req)
      ->Promise.flatMap(response => {
          Runner.pushMany(
            runner,
            filterReqeusts(List.toArray(callback(response))),
          )
        })
      ->Promise.map(() => List.fromArray(derivedRequests^))
    };
  }
  and sendRequests = (state, requests: list(Task.request)): Promise.t(unit) =>
    switch (requests) {
    | [] => Promise.resolved()
    | [x, ...xs] =>
      sendRequest(state, x)
      ->Promise.flatMap(xs' => sendRequests(state, List.concat(xs', xs)))
    };

  let make = state => {
    Runner.make(command => {
      let tasks = CommandHandler.handle(command);
      runTasks(state, tasks)->Promise.flatMap(sendRequests(state));
    });
  };

  // destroy only after all tasks have been executed
  let destroy = (runner: Runner.t(Command.t)): Promise.t(unit) =>
    runner.terminationPromise;
};