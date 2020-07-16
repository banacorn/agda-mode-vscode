open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Decoration = Decoration.Impl(Editor);
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module DecorationHandler = Handle__Decoration.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  let sendAgdaRequest = (runTasks, runTasksLater, state, request) => {
    let printLog = false;
    let (log, log2) =
      if (printLog) {
        (Js.log, Js.log2);
      } else {
        (_ => (), (_, _) => ());
      };

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
          log(">>> " ++ Response.toString(response));
          let tasks = ResponseHandler.handle(response);
          runTasks(tasks);
        }
      | Ok(Yield(Ok(Last(priority, response)))) => {
          log(
            ">>> "
            ++ string_of_int(priority)
            ++ " "
            ++ Response.toString(response),
          );
          let tasks = ResponseHandler.handle(response);
          runTasksLater(priority, tasks);
        }
      | Ok(Stop) => {
          log(">>| ");
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
        log2("<<<", encoded);
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

  let executeTask =
      (state: State.t, queue: TaskQueue.t(Task.t), task: Task.t)
      : Promise.t(bool) => {
    let keepRunning =
      switch (task) {
      | DispatchCommand(command) =>
        let tasks = CommandHandler.handle(command);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | SendRequest(request) =>
        // there can only be 1 Agda request at a time
        if (TaskQueue.Agda.isOccupied(queue)) {
          Js.log("[ panic ] There can only be 1 Agda request at a time!");
          Js.log(TaskQueue.toString(Task.toString, queue));
          Promise.resolved(false);
        } else {
          let lastTasks = [||];

          sendAgdaRequest(
            tasks => {TaskQueue.Agda.addToTheBack(queue, tasks)},
            (priority, tasks) => {
              Js.Array.push((priority, tasks), lastTasks)->ignore
            },
            state,
            request,
          )
          ->Promise.flatMap(() => TaskQueue.Agda.close(queue))
          ->Promise.map(() => {
              let tasks =
                Js.Array.sortInPlaceWith(
                  (x, y) => compare(fst(x), fst(y)),
                  lastTasks,
                )
                ->Array.map(snd)
                ->List.concatMany;
              TaskQueue.addToTheFront(queue, tasks);
            })
          ->ignore;
          // NOTE: return early before `sendAgdaRequest` resolved
          Promise.resolved(true);
        }
      | SendEventToView(event) =>
        state->State.sendEventToView(event)->Promise.map(_ => {true})
      | SendRequestToView(request, callback) =>
        // there can only be 1 View request at a time
        if (TaskQueue.View.isOccupied(queue)) {
          Promise.resolved(false);
        } else {
          state
          ->State.sendRequestToView(request)
          ->Promise.flatMap(
              fun
              | None => Promise.resolved()
              | Some(response) => {
                  TaskQueue.View.addToTheBack(queue, callback(response));
                  TaskQueue.View.close(queue);
                },
            )
          ->Promise.map(_ => {true})
          ->ignore;
          // NOTE: return early before `sendRequestToView` resolved
          Promise.resolved(true);
        }
      | Decoration(action) =>
        let tasks = DecorationHandler.handle(action);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | WithState(callback) =>
        callback(state);
        Promise.resolved(true);

      | WithStateP(callback) =>
        callback(state)
        ->Promise.map(TaskQueue.addToTheFront(queue))
        ->Promise.map(() => true)
      | SuicideByCop =>
        state->State.emitKillMePlz;
        Promise.resolved(false);
      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | Debug(message) =>
        Js.log("DEBUG " ++ message);
        Promise.resolved(true);
      };

    log(
      "### "
      ++ Task.toString(task)
      ++ "\n"
      ++ TaskQueue.toString(Task.toString, queue),
    );

    keepRunning;
  };

  type t = {
    blocking: TaskQueue.t(Task.t),
    critical: TaskQueue.t(Task.t),
  };

  let make = (state: State.t) => {
    blocking: TaskQueue.make(executeTask(state)),
    critical: TaskQueue.make(executeTask(state)),
  };

  let dispatchCommand = (self, command) => {
    // log(
    //   "\n\n"
    //   ++ TaskQueue.toString(Task.toString, self.critical)
    //   ++ "\n----------------------------\n"
    //   ++ TaskQueue.toString(Task.toString, self.blocking),
    // );
    switch (command) {
    | Command.NextGoal
    | PreviousGoal
    | InputMethod(_)
    | EventFromView(_)
    | Escape =>
      TaskQueue.addToTheBack(self.critical, [DispatchCommand(command)])
    | _ => TaskQueue.addToTheBack(self.blocking, [DispatchCommand(command)])
    };
  };

  // wait until all tasks have finished executing
  let destroy = self => {
    self.critical
    ->TaskQueue.destroy
    ->Promise.flatMap(() => self.blocking->TaskQueue.destroy);
  };

  // destroy everything in a rather violent way
  let forceDestroy = self => {
    self.critical
    ->TaskQueue.forceDestroy
    ->Promise.flatMap(() => self.blocking->TaskQueue.forceDestroy);
  };
};
