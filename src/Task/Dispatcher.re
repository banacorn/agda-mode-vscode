open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  let sendAgdaRequest = (runTasks, state, req) => {
    // this promise get resolved after the request to Agda is completed
    let (promise, resolve) = Promise.pending();
    let handle = ref(None);
    let handler: result(Connection.response, Connection.Error.t) => unit =
      fun
      | Error(error) => {
          let tasks = ErrorHandler.handle(Error.Connection(error));
          runTasks(tasks);
        }
      | Ok(Parser.Incr.Event.Yield(Error(error))) => {
          let tasks = ErrorHandler.handle(Error.Parser(error));
          runTasks(tasks);
        }
      | Ok(Yield(Ok(response))) => {
          Js.log(">>> " ++ Response.toString(response));
          let tasks = ResponseHandler.handle(response);
          runTasks(tasks);
        }
      | Ok(Stop) => {
          Js.log(">>| ");
          resolve();
        };

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
            runTasks(tasks);
            promise;
          },
      )
    ->Promise.tap(() => (handle^)->Option.forEach(f => f()));
  };

  let printLog = true;
  let log =
    if (printLog) {
      Js.log;
    } else {
      _ => ();
    };

  type source =
    | Agda
    | View
    | Command
    | Misc;

  module Runner = {
    type t = {
      mutable queues: MultiQueue.t(Task.t),
      // `busy` will be set to `true` if there are Tasks being executed
      // A semaphore to make sure that only one `kickStart` is running at a time
      mutable busy: bool,
    };

    let make = () => {queues: MultiQueue.make(), busy: false};

    module Queues = {
      let spawn = (self, target) =>
        self.queues = MultiQueue.spawn(self.queues, target);
      let remove = (self, target) =>
        self.queues = MultiQueue.remove(self.queues, target);
      let addTasks = (self, target, tasks) =>
        self.queues = MultiQueue.addTasks(self.queues, target, tasks);
      let countBySource = (self, target) =>
        MultiQueue.countBySource(self.queues, target);
      let getNextTask = self =>
        MultiQueue.getNextTask(true, self.queues)
        ->Option.map(((task, queue)) => {
            self.queues = queue;
            task;
          });

      let addMiscTasks = (self, tasks) => {
        spawn(self, Misc);
        addTasks(self, Misc, tasks);
        remove(self, Misc);
        Promise.resolved(true);
      };
    };

    let rec executeTask = (self, state: State.t, task) => {
      switch (task) {
      | DispatchCommand(command) =>
        let tasks = CommandHandler.handle(command);
        Queues.addTasks(self, Command, tasks);
        Promise.resolved(true);
      | SendRequest(request) =>
        if (Queues.countBySource(self, Agda) > 0) {
          // there can only be 1 Agda request at a time
          Promise.resolved(
            false,
          );
        } else {
          Queues.spawn(self, Agda);
          sendAgdaRequest(
            tasks => {
              Queues.addTasks(self, Agda, tasks);
              kickStart(self, state);
            },
            state,
            request,
          )
          ->Promise.get(() => {Queues.remove(self, Agda)});
          // NOTE: return early before `sendAgdaRequest` resolved
          Promise.resolved(true);
        }
      | SendEventToView(event) =>
        Queues.spawn(self, View);
        state
        ->State.sendEventToView(event)
        ->Promise.map(_ => {
            Queues.remove(self, View);
            true;
          });
      | SendRequestToView(request, callback) =>
        if (Queues.countBySource(self, View) > 0) {
          // there can only be 1 View request at a time
          Promise.resolved(
            false,
          );
        } else {
          Queues.spawn(self, View);
          state
          ->State.sendRequestToView(request)
          ->Promise.map(
              fun
              | None => ()
              | Some(response) => {
                  Queues.addTasks(self, View, callback(response));
                },
            )
          ->Promise.map(() => {
              Queues.remove(self, View);
              true;
            });
        }
      | AddHighlightings(annotations) =>
        annotations
        ->Array.keep(annotation =>
            annotation.aspects->Array.some(Response.Aspect.shouldHighlight)
          )
        ->Array.forEach(annotation =>
            Js.log(Response.Highlighting.toString(annotation))
          );
        Promise.resolved(true);
      | WithState(callback) =>
        callback(state);
        Promise.resolved(true);
      | WithStateP(callback) =>
        Queues.spawn(self, Misc);
        callback(state)
        ->Promise.map(Queues.addTasks(self, Misc))
        ->Promise.tap(() => {Queues.remove(self, Misc)})
        ->Promise.map(() => true);
      | Terminate => State.destroy(state)->Promise.map(() => false)
      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        Queues.addMiscTasks(self, tasks);
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        Queues.addMiscTasks(self, tasks);
      | Debug(message) =>
        Js.log("DEBUG " ++ message);
        Promise.resolved(true);
      };
    }
    // consuming Tasks in the `queues`
    and kickStart = (self, state) =>
      if (!self.busy) {
        switch (Queues.getNextTask(self)) {
        | None => ()
        | Some(task) =>
          self.busy = true;
          executeTask(self, state, task) // and start executing tasks
          ->Promise.get(keepRunning => {
              self.busy = false; // flip the semaphore back
              if (keepRunning) {
                // and keep running
                kickStart(self, state);
              };
            });
        };
      };
  };

  type t = {
    blocking: Runner.t,
    critical: Runner.t,
  };

  let make = () => {blocking: Runner.make(), critical: Runner.make()};

  let dispatchCommand = (self, state: State.t, command) => {
    log(
      "\n\n"
      ++ MultiQueue.toString(Task.toString, self.critical.queues)
      ++ "\n----------------------------\n"
      ++ MultiQueue.toString(Task.toString, self.blocking.queues),
    );

    switch (command) {
    | Command.NextGoal
    | PreviousGoal
    | InputMethod(_)
    | EventFromView(_)
    | Escape =>
      Runner.Queues.addTasks(
        self.critical,
        Command,
        [DispatchCommand(command)],
      );
      Runner.kickStart(self.critical, state);
    | _ =>
      Runner.Queues.addTasks(
        self.blocking,
        Command,
        [DispatchCommand(command)],
      );
      Runner.kickStart(self.blocking, state);
    };
  };

  // do nothing
  let destroy = _ => ();
};