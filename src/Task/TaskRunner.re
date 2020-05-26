open Belt;

module Runner = {
  type status =
    | Busy
    | Idle;

  type t('a) = {
    mutable queue: array('a),
    mutable status,
    execute: 'a => Promise.t(unit),
    mutable terminator: (Promise.t(unit), unit => unit),
    mutable shouldTerminate: bool,
  };

  let make = execute => {
    let (promise, resolve) = Promise.pending();
    {
      queue: [||],
      status: Idle,
      execute,
      terminator: (promise, resolve),
      shouldTerminate: false,
    };
  };

  let rec run = (self: t('a)): Promise.t(unit) => {
    switch (self.status) {
    | Busy => Promise.resolved()
    | Idle =>
      let nextTasks = Js.Array.shift(self.queue);
      (
        switch (nextTasks) {
        | None => Promise.resolved()
        | Some(task) =>
          self.status = Busy;
          self.execute(task)
          ->Promise.flatMap(() => {run(self)})
          ->Promise.tap(_ => {self.status = Idle});
        }
      )
      // see if the runner need to be terminated after finished executing all tasks in the queue
      ->Promise.tap(() =>
          if (self.shouldTerminate) {
            let (_, resolve) = self.terminator;
            resolve();
          }
        );
    };
  };

  let push = (self, x: 'a) => {
    // push a new task to the queue
    Js.Array.push(x, self.queue)->ignore;
    // kick start the runner
    run(self);
  };

  let pushMany = (self: t('a), xs: array('a)): Promise.t(unit) => {
    // concat tasks to the back of the queue
    self.queue = Js.Array.concat(self.queue, xs);
    // kick start the runner
    run(self);
  };

  //
  let terminate = self => {
    let (_, resolve) = self.terminator;
    switch (self.status) {
    | Idle => resolve()
    | Busy => self.shouldTerminate = true
    };
  };
};

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Task__Error.Impl(Editor);
  module ViewHandler = Task__View.Impl(Editor);
  module CommandHandler = Task__Command.Impl(Editor);
  module ResponseHandler = Task__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);

  // Task Runner
  type t = {
    // channel for adding Commands to the back of the queue
    onAddCommand: Event.t(Command.t),
    // channel for receiving responses from Agda
    onResponse: Event.t(option(result(Response.t, Error.t))),
    // edge triggered Status emitter
    onChangeStatus: Event.t(Runner.status),
    mutable status: Runner.status,
  };

  let dispatchCommand = (self, command) => self.onAddCommand.emit(command);

  let rec sendRequest =
          (state, request: Request.t): Promise.t(array(Request.t)) => {
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
          Js.log(Response.toString(response));
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
          Runner.pushMany(runner, otherTasks)->ignore;
        }
      | Ok(Stop) => Runner.terminate(runner);

    let (promise, _) = runner.terminator;

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
            runTasks(state, tasks)->Promise.flatMap(() => promise);
          },
      )
    ->Promise.tap(() => (handle^)->Option.forEach(f => f()))
    ->Promise.map(() => derivedRequests^);
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