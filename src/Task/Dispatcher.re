open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Decoration = Decoration.Impl(Editor);
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  let sendAgdaRequest = (runTasks, runTasksLater, state, request) => {
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
      | Ok(Yield(Ok(NonLast(response)))) => {
          Js.log(">>> " ++ Response.toString(response));
          let tasks = ResponseHandler.handle(response);
          runTasks(tasks);
        }
      | Ok(Yield(Ok(Last(priority, response)))) => {
          Js.log(
            ">>> "
            ++ string_of_int(priority)
            ++ " "
            ++ Response.toString(response),
          );
          let tasks = ResponseHandler.handle(response);
          runTasksLater(priority, tasks);
        }
      | Ok(Stop) => {
          Js.log(">>| ");
          resolve();
        };

    state
    ->State.connect
    ->Promise.mapOk(connection => {
        let version = connection.metadata.version;
        let filepath =
          Editor.getFileName(state.editor)->Option.getWithDefault("");
        let libraryPath = Editor.Config.getLibraryPath();
        let highlightingMethod = Editor.Config.getHighlightingMethod();
        let backend = Editor.Config.getBackend();
        let encoded =
          Request.encode(
            state.editor,
            version,
            filepath,
            backend,
            libraryPath,
            highlightingMethod,
            request,
          );
        Js.log2("<<<", encoded);
        Connection.send(encoded, connection);
        connection;
      })
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

  let printLog = false;
  let log =
    if (printLog) {
      Js.log;
    } else {
      _ => ();
    };

  module Runner = {
    type t = {
      mutable queues: TaskQueue.t(Task.t),
      executeTask: (t, t => unit, Task.t) => Promise.t(bool),
      // `busy` will be set to `true` if there are Tasks being executed
      // A semaphore to make sure that only one `kickStart` is running at a time
      mutable busy: bool,
      mutable shouldDestroy: option(unit => unit),
    };

    let make = executeTask => {
      queues: TaskQueue.make(),
      busy: false,
      shouldDestroy: None,
      executeTask,
    };

    // consuming Tasks in the `queues`
    let rec kickStart = self =>
      if (!self.busy) {
        let (nextTask, queue) = TaskQueue.getNextTask(self.queues);
        self.queues = queue;
        switch (nextTask) {
        | None =>
          // if there are no more tasks, and .shouldDestroy is set, then resolve it
          switch (self.shouldDestroy) {
          | None => ()
          | Some(resolve) => resolve()
          }
        | Some(task) =>
          self.busy = true;
          self.executeTask(self, kickStart, task) // and start executing tasks
          ->Promise.get(keepRunning => {
              self.busy = false; // flip the semaphore back
              if (keepRunning) {
                // and keep running
                kickStart(self);
              };
            });
        };
      };
    // returns a promise that resolves when all tasks have been executed
    let destroy = self =>
      if (self.busy) {
        let (promise, resolve) = Promise.pending();
        self.shouldDestroy = Some(resolve);
        promise;
      } else {
        Promise.resolved();
      };

    // clear the queue, doesn't wait
    let forceDestroy = self => {
      self.queues = TaskQueue.make();
      Promise.resolved();
    };
  };

  let executeTask =
      (
        state: State.t,
        self: Runner.t,
        kickStart: Runner.t => unit,
        task: Task.t,
      )
      : Promise.t(bool) => {
    switch (task) {
    | DispatchCommand(command) =>
      let tasks = CommandHandler.handle(command);
      self.queues = TaskQueue.addTasksToMain(self.queues, tasks);
      Promise.resolved(true);
    | SendRequest(request) =>
      // there can only be 1 Agda request at a time
      if (TaskQueue.agdaIsOccupied(self.queues)) {
        Js.log("[ panic ] There can only be 1 Agda request at a time!");
        Promise.resolved(false);
      } else {
        self.queues = TaskQueue.acquireAgda(self.queues);

        let lastTasks = [||];

        sendAgdaRequest(
          tasks => {
            self.queues = TaskQueue.addTasksToAgda(self.queues, tasks);
            kickStart(self);
          },
          (priority, tasks) => {
            Js.Array.push((priority, tasks), lastTasks)->ignore
          },
          state,
          request,
        )
        ->Promise.map(() => {
            let tasks =
              Js.Array.sortInPlaceWith(
                (x, y) => compare(fst(x), fst(y)),
                lastTasks,
              )
              ->Array.map(snd)
              ->List.concatMany;
            self.queues = TaskQueue.addTasksToAgda(self.queues, tasks);
            self.queues = TaskQueue.releaseAgda(self.queues);
            kickStart(self);
          })
        ->Promise.get(() => {
            self.queues = TaskQueue.releaseAgda(self.queues)
          });
        // NOTE: return early before `sendAgdaRequest` resolved
        Promise.resolved(true);
      }
    | SendEventToView(event) =>
      state->State.sendEventToView(event)->Promise.map(_ => {true})
    | SendRequestToView(request, callback) =>
      // there can only be 1 View request at a time
      if (TaskQueue.viewIsOccupied(self.queues)) {
        Promise.resolved(false);
      } else {
        self.queues = TaskQueue.acquireView(self.queues);
        state
        ->State.sendRequestToView(request)
        ->Promise.map(
            fun
            | None => true
            | Some(response) => {
                self.queues =
                  TaskQueue.addTasksToView(self.queues, callback(response));
                self.queues = TaskQueue.releaseView(self.queues);
                true;
              },
          );
      }
    | AddHighlightings(annotations) =>
      annotations->Array.forEach(highlighting => {
        let decorations =
          Decoration.decorateHighlighting(state.editor, highlighting);
        state.decorations = Array.concat(state.decorations, decorations);
      });
      Promise.resolved(true);
    | RemoveAllHighlightings =>
      state.decorations
      ->Array.map(fst)
      ->Array.forEach(Editor.Decoration.destroy);
      state.decorations = [||];
      Promise.resolved(true);
    | RefreshAllHighlightings =>
      state.decorations
      ->Array.forEach(((decoration, range)) => {
          Editor.Decoration.decorate(state.editor, decoration, [|range|])
        });
      state.goals
      ->Array.forEach(goal => goal->Goal.refreshDecoration(state.editor));
      Promise.resolved(true);
    | WithState(callback) =>
      callback(state);
      Promise.resolved(true);

    | WithStateP(callback) =>
      callback(state)
      ->Promise.map(tasks => {
          self.queues = TaskQueue.addTasksToMain(self.queues, tasks)
        })
      ->Promise.map(() => true)
    | SuicideByCop =>
      state->State.emitKillMePlz;
      Promise.resolved(false);
    | Goal(action) =>
      let tasks = GoalHandler.handle(action);
      self.queues = TaskQueue.addTasksToMain(self.queues, tasks);
      Promise.resolved(true);
    | Error(error) =>
      let tasks = ErrorHandler.handle(error);
      self.queues = TaskQueue.addTasksToMain(self.queues, tasks);
      Promise.resolved(true);
    | Debug(message) =>
      Js.log("DEBUG " ++ message);
      Promise.resolved(true);
    };
  };

  type t = {
    blocking: Runner.t,
    critical: Runner.t,
  };

  let make = (state: State.t) => {
    blocking: Runner.make(executeTask(state)),
    critical: Runner.make(executeTask(state)),
  };

  let dispatchCommand = (self, command) => {
    log(
      "\n\n"
      ++ TaskQueue.toString(Task.toString, self.critical.queues)
      ++ "\n----------------------------\n"
      ++ TaskQueue.toString(Task.toString, self.blocking.queues),
    );

    switch (command) {
    | Command.NextGoal
    | PreviousGoal
    | InputMethod(_)
    | EventFromView(_)
    | Escape =>
      self.critical.queues =
        TaskQueue.addTasksToMain(
          self.critical.queues,
          [DispatchCommand(command)],
        );
      Runner.kickStart(self.critical);
    | _ =>
      self.blocking.queues =
        TaskQueue.addTasksToMain(
          self.blocking.queues,
          [DispatchCommand(command)],
        );
      Runner.kickStart(self.blocking);
    };
  };

  // wait until all tasks have finished executing
  let destroy = self => {
    self.critical
    ->Runner.destroy
    ->Promise.flatMap(() => self.blocking->Runner.destroy);
  };

  // destroy everything in a rather violent way
  let forceDestroy = self => {
    self.critical
    ->Runner.forceDestroy
    ->Promise.flatMap(() => self.blocking->Runner.forceDestroy);
  };
};
