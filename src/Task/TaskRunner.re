module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Task__Error.Impl(Editor);
  module ViewHandler = Task__View.Impl(Editor);
  module TaskCommand = Task__Command.Impl(Editor);
  module ResponseHandler = Task__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open Belt;

  type outcome =
    | Derived(list(Task.t))
    | Pending
    | Concluded;

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
    mutable pendingResponse: bool,
    // edge triggered Status emitter
    onChangeStatus: Event.t(status),
    mutable status,
  };

  let dispatchCommand = (self, command) => self.onAddCommand.emit(command);

  let runTask = (self, state, task: Task.t): Promise.t(outcome) =>
    switch (task) {
    | Task.Terminate =>
      Js.log("[ task ][ terminate ] ");
      State.destroy(state)->ignore;
      Promise.resolved(Concluded);
    | WithState(callback) =>
      callback(state)->Promise.map(tasks => Derived(tasks))
    | DispatchCommand(command) =>
      Js.log("[ task ][ command ] " ++ Command.toString(command));
      Promise.resolved(Derived(TaskCommand.dispatch(command)));
    | SendRequest(request) =>
      Js.log("[ task ][ send request ]");
      let destructor = ref(None);

      let stop = () => {
        self.pendingResponse = false;
        // invoke the destructor
        // to stop receiving events regarding this request
        (destructor^)->Option.forEach(f => f());
      };

      let handler = (
        fun
        | Error(error) => {
            stop();
            self.onResponse.emit(Some(Error(Error.Connection(error))));
            self.onResponse.emit(None);
          }
        | Ok(Parser.Incr.Event.Yield(Error(error))) => {
            stop();
            self.onResponse.emit(Some(Error(Error.Parser(error))));
            self.onResponse.emit(None);
          }
        | Ok(Yield(Ok(response))) =>
          self.onResponse.emit(Some(Ok(response)))
        | Ok(Stop) => {
            stop();
            self.onResponse.emit(None);
          }
      );

      state
      ->State.sendRequest(request)
      ->Promise.flatMap(
          fun
          | Ok(connection) => {
              destructor := Some(connection.Connection.emitter.on(handler));
              Promise.resolved(Pending);
            }
          | Error(error) => {
              let tasks = ErrorHandler.handle(error);
              Promise.resolved(Derived(tasks));
            },
        );
    | ViewReq(request) =>
      Js.log("< >");
      state->State.sendRequestToView(request)->Promise.map(() => Concluded);
    | ViewRes(response) =>
      let tasks = ViewHandler.handle(response);
      Promise.resolved(Derived(tasks));
    };

  let make = state => {
    // emitters
    let onAddCommand = Event.make();
    let onResponse = Event.make();
    let onChangeStatus = Event.make();
    // statess
    let queue = ref([||]);

    let self = {
      onAddCommand,
      onResponse,
      pendingResponse: false,
      onChangeStatus,
      status: Idle,
    };

    let getNextTask = () =>
      if (self.pendingResponse) {
        None;
      } else {
        Js.Array.shift(queue^);
      };

    let pushDerivedTasks = tasks =>
      queue := Js.Array.concat(List.toArray(tasks), queue^);

    let rec runTasksInQueues = () =>
      if (self.pendingResponse) {
        let destructor = ref(None);
        destructor :=
          Some(
            self.onResponse.on(
              fun
              | Some(Ok(response)) => {
                  Js.log("Response " ++ Response.toString(response));
                  let tasks = ResponseHandler.handle(response);
                  pushDerivedTasks(tasks);
                }
              | Some(Error(error)) => {
                  let tasks = ErrorHandler.handle(error);
                  pushDerivedTasks(tasks);
                }
              | None => {
                  Js.log("status: " ++ string_of_bool(self.pendingResponse));
                  runTasksInQueues();
                  (destructor^)->Option.forEach(f => f());
                },
            ),
          );
        ();
      } else {
        let nextTask = getNextTask();
        switch (nextTask) {
        | None =>
          self.status = Idle;
          self.onChangeStatus.emit(Idle);
        | Some(task) =>
          runTask(self, state, task)
          ->Promise.get(
              fun
              | Derived(tasks) => {
                  Js.log("Derived");
                  pushDerivedTasks(tasks);
                  runTasksInQueues();
                }
              | Pending => {
                  Js.log("Pending");
                  self.pendingResponse = true;
                  runTasksInQueues();
                }
              | Concluded => {
                  Js.log("Concluded");
                  runTasksInQueues();
                },
            )
        };
      };

    let _ =
      onAddCommand.on(command => {
        // add it to the back of the queue
        Js.Array.push(Task.DispatchCommand(command), queue^)->ignore;
        // kick start `runTasksInQueues` if it's not already running
        if (self.status == Idle) {
          self.status = Busy;
          self.onChangeStatus.emit(Busy);
          runTasksInQueues();
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