module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Task__Error.Impl(Editor);
  module ViewHandler = Task__View.Impl(Editor);
  module CommandHandler = Task__Command.Impl(Editor);
  module ResponseHandler = Task__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;

  // Busy if there are Tasks currently being executed, Idle otherwise
  type status =
    | Busy
    | Idle;

  // Task Runner
  type t = {
    // channel for adding Commands to the back of the queue
    onAddCommand: Event.t(Command.t),
    // channel for receiving responses from Agda
    onResponse: Event.t(option(result(Response.t, Error.t))),
    // edge triggered Status emitter
    onChangeStatus: Event.t(status),
    mutable status,
  };

  let dispatchCommand = (self, command) => self.onAddCommand.emit(command);

  let rec sendRequest =
          (state, request: Request.t): Promise.t(array(Request.t)) => {
    // Task.SendRequest will be deferred and executed until the current request is handled
    let derivedRequests = ref([||]);
    // Task queue
    let queue = ref([||]);

    let (promise, resolve) = Promise.pending();
    // when Agda has stopped responding, `stoppedResponding` will be set as `Some(resolve)`
    let stoppedResponding = ref(None);

    let status = ref(Idle);
    let rec runTasksInQueue = () => {
      switch (status^) {
      | Busy => Promise.resolved()
      | Idle =>
        let nextTasks = Js.Array.shift(queue^);
        (
          switch (nextTasks) {
          | None => Promise.resolved()
          | Some(task) =>
            status := Busy;
            runTask(state, task)
            ->Promise.flatMap(runTasksInQueue)
            ->Promise.tap(() => {status := Idle});
          }
        )
        // when finished executing all tasks in the queue
        // see if the Agda has stopped responding
        ->Promise.tap(() => {
            (stoppedResponding^)
            ->Option.forEach(resolve => resolve(derivedRequests^))
          });
      };
    };

    let stop = () =>
      switch (status^) {
      | Idle => resolve(derivedRequests^)
      | Busy => stoppedResponding := Some(resolve)
      };
    // handle of the connection response listener
    let handle = ref(None);
    let handler =
      fun
      | Error(error) => {
          let tasks = ErrorHandler.handle(Error.Connection(error));
          queue := Js.Array.concat(queue^, List.toArray(tasks));
          runTasksInQueue()->ignore;
          stop();
        }
      | Ok(Parser.Incr.Event.Yield(Error(error))) => {
          let tasks = ErrorHandler.handle(Error.Parser(error));
          queue := Js.Array.concat(queue^, List.toArray(tasks));
          runTasksInQueue()->ignore;
          stop();
        }
      | Ok(Yield(Ok(response))) => {
          let otherTasks =
            List.toArray(ResponseHandler.handle(response))
            ->Array.keep(
                fun
                | Task.SendRequest(req) => {
                    Js.Array.push(req, derivedRequests^)->ignore;
                    false;
                  }
                | _ => true,
              );
          queue := Js.Array.concat(queue^, otherTasks);
          runTasksInQueue()->ignore;
        }
      | Ok(Stop) => stop();

    state
    ->State.sendRequest(request)
    ->Promise.flatMap(
        fun
        | Ok(connection) => {
            handle := Some(connection.Connection.emitter.on(handler));
            promise;
          }
        | Error(error) => {
            let tasks = ErrorHandler.handle(error);
            resolve([||]);
            runTasks(state, tasks)->Promise.flatMap(() => promise);
          },
      );
  }
  and sendRequests = (state, requests: list(Request.t)): Promise.t(unit) =>
    switch (requests) {
    | [] => Promise.resolved()
    | [x, ...xs] =>
      sendRequest(state, x)
      ->Promise.flatMap(xs' =>
          sendRequests(state, List.concat(List.fromArray(xs'), xs))
        )
    }
  and runTask = (state, task: Task.t): Promise.t(unit) =>
    switch (task) {
    | Task.Terminate =>
      Js.log("[ task ][ terminate ] ");
      State.destroy(state);
    | WithState(callback) =>
      callback(state)->Promise.flatMap(runTasks(state))
    | SendRequest(request) =>
      Js.log("[ task ][ send request ]");
      sendRequests(state, [request]);
    | ViewReq(request) =>
      Js.log("< >");
      state->State.sendRequestToView(request);
    | ViewRes(response) =>
      let tasks = ViewHandler.handle(response);
      runTasks(state, tasks);
    }
  and runTasks = (state, tasks: list(Task.t)): Promise.t(unit) =>
    switch (tasks) {
    | [] => Promise.resolved()
    | [x, ...xs] =>
      runTask(state, x)->Promise.flatMap(() => runTasks(state, xs))
    };

  let make = state => {
    // emitters
    let onAddCommand = Event.make();
    let onResponse = Event.make();
    let onChangeStatus = Event.make();
    // statess
    let queue = [||];

    let self = {onAddCommand, onResponse, onChangeStatus, status: Idle};

    let getNextCommand = () => Js.Array.shift(queue);

    let rec runCommandsInQueue = () => {
      let nextCommand = getNextCommand();
      switch (nextCommand) {
      | None =>
        self.status = Idle;
        self.onChangeStatus.emit(Idle);
      | Some(command) =>
        let tasks = CommandHandler.handle(command);
        runTasks(state, tasks)->Promise.get(() => {runCommandsInQueue()});
      };
    };

    let _ =
      onAddCommand.on(command => {
        // add it to the back of the queue
        Js.Array.push(command, queue)->ignore;
        // kick start `runCommandsInQueue` if it's not already running
        if (self.status == Idle) {
          self.status = Busy;
          self.onChangeStatus.emit(Busy);
          runCommandsInQueue();
        };
      });

    self;
  };

  // destroy only after all tasks have been executed
  let destroy = (self: t): Promise.t(unit) => {
    let (promise, resolve) = Promise.pending();
    let destroy' = () => {
      self.onChangeStatus.destroy();
      self.onAddCommand.destroy();
      resolve();
    };

    switch (self.status) {
    | Idle => destroy'()
    | Busy =>
      let _ =
        self.onChangeStatus.on(
          fun
          | Idle => destroy'()
          | Busy => (),
        );
      ();
    };
    promise;
  };
};