open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Decoration = Decoration.Impl(Editor);
  module ErrorHandler = Handle__Error.Impl(Editor);
  module GoalHandler = Handle__Goal.Impl(Editor);
  module CommandHandler = Handle__Command.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module DecorationHandler = Handle__Decoration.Impl(Editor);
  module EditorIM = EditorIM.Impl(Editor);
  module TaskQueue = TaskQueue.Impl(Editor);
  module Task = Task.Impl(Editor);
  open! Task;

  // helper function of `executeTask`
  let sendAgdaRequest = (addToAgdaQueue, addToDeferredQueue, state, request) => {
    let printLog = true;
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
          addToAgdaQueue(tasks);
        }
      | Ok(Parser.Incr.Event.Yield(Error(error))) => {
          let tasks = ErrorHandler.handle(Error.Parser(error));
          addToAgdaQueue(tasks);
        }
      | Ok(Yield(Ok(NonLast(response)))) => {
          switch (response) {
          | Response.HighlightingInfoDirect(_, _)
          | HighlightingInfoIndirect(_) =>
            log("~~~ " ++ Response.toString(response));
            let tasks = ResponseHandler.handle(response);
            addToDeferredQueue(-1, tasks);
          | _ =>
            log(">>> " ++ Response.toString(response));
            let tasks = ResponseHandler.handle(response);
            addToAgdaQueue(tasks);
          };
        }
      | Ok(Yield(Ok(Last(priority, response)))) => {
          log(
            ">>> "
            ++ string_of_int(priority)
            ++ " "
            ++ Response.toString(response),
          );
          let tasks = ResponseHandler.handle(response);
          addToDeferredQueue(priority, tasks);
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
            addToAgdaQueue(tasks);
            promise;
          },
      )
    ->Promise.tap(() => (handle^)->Option.forEach(f => f()));
  };

  // the working horse
  let executeTask =
      (state: State.t, queue: TaskQueue.t, task: Task.t): Promise.t(bool) => {
    let keepRunning =
      switch (task) {
      | DispatchCommand(command) =>
        let tasks = CommandHandler.handle(command);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | AgdaRequest(request) =>
        // there can only be 1 Agda request at a time
        if (TaskQueue.Agda.isOccupied(queue)) {
          Js.log("[ panic ] There can only be 1 Agda request at a time!");
          Js.log(TaskQueue.toString(Task.toString, queue));
          Promise.resolved(false);
        } else {
          let deferredTasks = [||];

          sendAgdaRequest(
            tasks => TaskQueue.Agda.addToTheBack(queue, tasks),
            (priority, tasks) =>
              Js.Array.push((priority, tasks), deferredTasks)->ignore,
            state,
            request,
          )
          ->Promise.flatMap(() => TaskQueue.Agda.close(queue))
          ->Promise.map(() => {
              // sort the deferred task by priority (ascending order)
              let deferredTasks =
                Js.Array.sortInPlaceWith(
                  (x, y) => compare(fst(x), fst(y)),
                  deferredTasks,
                )
                ->Array.map(snd)
                ->List.concatMany;
              TaskQueue.addToTheFront(queue, deferredTasks);

              true;
            });
          // ->ignore;
          // // NOTE: return early before `sendAgdaRequest` resolved
          // Promise.resolved(true);
        }
      | ViewEvent(event) =>
        state->State.sendEventToView(event)->Promise.map(_ => {true})
      | ViewRequest(request, callback) =>
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
      | Goal(action) =>
        let tasks = GoalHandler.handle(action);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | Error(error) =>
        let tasks = ErrorHandler.handle(error);
        TaskQueue.addToTheFront(queue, tasks);
        Promise.resolved(true);
      | Destroy =>
        state->State.emitRemoveFromRegistry;
        Promise.resolved(false);
      | Debug(message) =>
        Js.log("DEBUG " ++ message);
        Promise.resolved(true);
      };

    let printLog = false;
    if (printLog) {
      Js.log(
        "### "
        ++ Task.toString(task)
        ++ "\n"
        ++ TaskQueue.toString(Task.toString, queue),
      );
    };

    keepRunning;
  };

  type t = {
    state: State.t,
    // may contain Tasks like `AgdaRequest` or `ViewRequest`
    blocking: TaskQueue.t,
    // mainly for UI-related commands
    critical: TaskQueue.t,
  };

  let addToTheBackCritical = (self, tasks) => {
    TaskQueue.addToTheBack(self.critical, tasks);
    TaskQueue.onEmptied(self.critical);
  };

  let addToTheBackBlocking = (self, tasks) => {
    TaskQueue.addToTheBack(self.blocking, tasks);
    TaskQueue.onEmptied(self.blocking);
  };

  let dispatchCommand = (self, command) => {
    open Command;
    // a command is non-blocking if it doesn't generate any Agda request/View query tasks
    let nonBlocking =
      fun
      | NextGoal
      | PreviousGoal
      | InputMethod(_)
      | EventFromView(_)
      | Escape => true
      | _ => false;

    if (nonBlocking(command)) {
      addToTheBackCritical(self, [DispatchCommand(command)]);
    } else {
      // `SaveCursor` before and `RestoreCursor` after `DispatchCommand(command)`
      addToTheBackBlocking(
        self,
        [Goal(SaveCursor), DispatchCommand(command), Goal(RestoreCursor)],
      );
    };
  };

  let make =
      (
        extentionPath: string,
        editor: Editor.editor,
        removeFromRegistry: unit => unit,
        eventEmitter: Event.t(EditorIM.event),
      ) => {
    let state = State.make(extentionPath, eventEmitter, editor);
    let dispatcher = {
      state,
      blocking: TaskQueue.make(executeTask(state)),
      critical: TaskQueue.make(executeTask(state)),
    };

    // listens to events from the view
    state.view
    ->Editor.View.onEvent(event =>
        dispatchCommand(dispatcher, EventFromView(event))->ignore
      )
    ->Js.Array.push(state.subscriptions)
    ->ignore;

    // listens to events from the editor input method
    state.editorIM.eventEmitter.on(action => {
      dispatchCommand(dispatcher, Command.InputMethod(action))->ignore
    })
    ->Editor.Disposable.make
    ->Js.Array.push(state.subscriptions)
    ->ignore;

    // remove it from the Registry if it requests to be destroyed
    state->State.onRemoveFromRegistry->Promise.get(removeFromRegistry);

    // return the dispatcher
    dispatcher;
  };

  // wait until all tasks have finished executing
  let destroy = self => {
    self.critical
    ->TaskQueue.onEmptied
    ->Promise.flatMap(() => self.blocking->TaskQueue.onEmptied)
    ->Promise.flatMap(() => self.state->State.destroy);
  };

  // destroy everything in a rather violent way
  let forceDestroy = self => {
    self.critical
    ->TaskQueue.forceDestroy
    ->Promise.flatMap(() => self.blocking->TaskQueue.forceDestroy)
    ->Promise.flatMap(() => self.state->State.destroy);
  };
};
