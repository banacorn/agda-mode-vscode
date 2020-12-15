open Belt

open! Task

// helper function of `executeTask`
let sendAgdaRequest = (addToAgdaQueue, addToDeferredQueue, state, request) => {
  let printLog = false
  let (log, log2) = if printLog {
    (Js.log, Js.log2)
  } else {
    (_ => (), (_, _) => ())
  }

  // this promise get resolved after the request to Agda is completed
  let (promise, resolve) = Promise.pending()
  let handle = ref(None)
  let handler: result<Connection.response, Connection.Error.t> => unit = x =>
    switch x {
    | Error(error) =>
      let tasks = Handle__Error.handle(Error.Connection(error))
      addToAgdaQueue(tasks)
    | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
      let tasks = Handle__Error.handle(Error.Parser(error))
      addToAgdaQueue(tasks)
    | Ok(Yield(Ok(NonLast(response)))) =>
      let tasks = Handle__Response.handle(response)
      addToAgdaQueue(tasks)
    | Ok(Yield(Ok(Last(priority, response)))) =>
      log(">>> " ++ (string_of_int(priority) ++ (" " ++ Response.toString(response))))
      let tasks = Handle__Response.handle(response)
      addToDeferredQueue(priority, tasks)
    | Ok(Stop) =>
      log(">>| ")
      resolve()
    }

  state->State.connect->Promise.mapOk(connection => {
    let document = VSCode.TextEditor.document(state.editor)
    let version = connection.metadata.version
    let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
    let libraryPath = Config.getLibraryPath()
    let highlightingMethod = Config.getHighlightingMethod()
    let backend = Config.getBackend()
    let encoded = Request.encode(
      document,
      version,
      filepath,
      backend,
      libraryPath,
      highlightingMethod,
      request,
    )
    log2("<<<", encoded)
    Connection.send(encoded, connection)
    connection
  })->Promise.flatMap(x =>
    switch x {
    | Ok(connection) =>
      handle := Some(connection.Connection.chan->Chan.on(handler))
      promise
    | Error(error) =>
      let tasks = Handle__Error.handle(error)
      addToAgdaQueue(tasks)
      promise
    }
  )->Promise.tap(() => handle.contents->Option.forEach(f => f()))
}

// the working horse
let executeTask = (state: State.t, queue: TaskQueue.t, task: Task.t): Promise.t<bool> => {
  let keepRunning = switch task {
  | DispatchCommand(command) =>
    let tasks = Handle__Command.handle(command)
    TaskQueue.addToTheFront(queue, tasks)
    Promise.resolved(true)
  | AgdaRequest(request) =>
    // there can only be 1 Agda request at a time
    if TaskQueue.Agda.isOccupied(queue) {
      Js.log("[ panic ] There can only be 1 Agda request at a time!")
      Js.log(TaskQueue.toString(Task.toString, queue))
      Promise.resolved(false)
    } else {
      let deferredTasks = []

      Js.Console.timeStart("$$$ Agda Request/Response")
      sendAgdaRequest(
        // when received a "NonLast" Agda Response
        tasks => TaskQueue.Agda.addToTheBack(queue, tasks),
        // when received a "Last" Agda Response
        (priority, tasks) => Js.Array.push((priority, tasks), deferredTasks)->ignore,
        state,
        request,
      )
      ->Promise.flatMap(() => TaskQueue.Agda.close(queue))
      ->Promise.map(() => {
        Js.Console.timeEnd("$$$ Agda Request/Response")
        // sort the deferred task by priority (ascending order)
        let deferredTasks =
          Js.Array.sortInPlaceWith((x, y) => compare(fst(x), fst(y)), deferredTasks)
          ->Array.map(snd)
          ->List.concatMany
        // apply decorations after all "NonLast" tasks before all "Last" tasks

        // Only dispatch `Decoration(Apply)` when SemanticHighlighting is not enabled
        let deferredTasks = Config.getSemanticHighlighting()
          ? deferredTasks
          : list{Task.Decoration(Apply), ...deferredTasks}

        TaskQueue.addToTheFront(queue, deferredTasks)
        true
      })
    }
  | ViewEvent(event) => state->State.sendEventToView(event)->Promise.map(_ => true)
  | ViewRequest(request, callback) =>
    // there can only be 1 View request at a time
    if TaskQueue.View.isOccupied(queue) {
      Promise.resolved(false)
    } else {
      state->State.sendRequestToView(request)->Promise.flatMap(x =>
        switch x {
        | None => Promise.resolved()
        | Some(response) =>
          TaskQueue.View.addToTheBack(queue, callback(response))
          TaskQueue.View.close(queue)
        }
      )->Promise.map(_ => true)->ignore
      // NOTE: return early before `sendRequestToView` resolved
      Promise.resolved(true)
    }
  | Decoration(action) =>
    let tasks = Handle__Decoration.handle(action)
    TaskQueue.addToTheFront(queue, tasks)
    Promise.resolved(true)
  | WithState(callback) =>
    callback(state)
    Promise.resolved(true)

  | WithStateP(callback) =>
    callback(state)->Promise.map(TaskQueue.addToTheFront(queue))->Promise.map(() => true)
  | Goal(action) =>
    let tasks = Handle__Goal.handle(action)
    TaskQueue.addToTheFront(queue, tasks)
    Promise.resolved(true)
  | Error(error) =>
    let tasks = Handle__Error.handle(error)
    TaskQueue.addToTheFront(queue, tasks)
    Promise.resolved(true)
  | Destroy =>
    state->State.emitRemoveFromRegistry
    Promise.resolved(false)
  | BenchStart(id) =>
    Js.Console.timeStart(id)
    Promise.resolved(true)
  | BenchEnd(id) =>
    Js.Console.timeEnd(id)
    Promise.resolved(true)
  | Debug(message) =>
    Js.log("DEBUG " ++ message)
    Promise.resolved(true)
  }

  let printLog = false
  if printLog {
    Js.log("### " ++ (Task.toString(task) ++ ("\n" ++ TaskQueue.toString(Task.toString, queue))))
  }

  keepRunning
}

type t = {
  state: State.t,
  // may contain Tasks like `AgdaRequest` or `ViewRequest`
  blocking: TaskQueue.t,
  // mainly for UI-related commands
  critical: TaskQueue.t,
}

let addToTheBackCritical = (self, tasks) => {
  TaskQueue.addToTheBack(self.critical, tasks)
  TaskQueue.onEmptied(self.critical)
}

let addToTheBackBlocking = (self, tasks) => {
  TaskQueue.addToTheBack(self.blocking, tasks)
  TaskQueue.onEmptied(self.blocking)
}

let dispatchCommand = (self, command) => {
  open Command
  // a command is non-blocking if it doesn't generate any Agda request/View prompt tasks
  let nonBlocking = x =>
    switch x {
    | NextGoal
    | PreviousGoal
    | InputMethod(_)
    | EventFromView(_)
    | Escape => true
    | _ => false
    }

  if nonBlocking(command) {
    addToTheBackCritical(self, list{DispatchCommand(command)})
  } else {
    // `SaveCursor` before and `RestoreCursor` after `DispatchCommand(command)`
    addToTheBackBlocking(
      self,
      list{Goal(SaveCursor), DispatchCommand(command), Goal(RestoreCursor)},
    )
  }
}

let make = (
  extentionPath: string,
  editor: VSCode.TextEditor.t,
  removeFromRegistry: unit => unit,
  chan: Chan.t<IM.Log.t>,
) => {
  let state = State.make(extentionPath, chan, editor)
  let dispatcher = {
    state: state,
    blocking: TaskQueue.make(executeTask(state)),
    critical: TaskQueue.make(executeTask(state)),
  }

  let subscribe = disposable => disposable->Js.Array.push(state.subscriptions)->ignore

  // listens to events from the view
  state.view
  ->View__Controller.onEvent(event => dispatchCommand(dispatcher, EventFromView(event))->ignore)
  ->Js.Array.push(state.subscriptions)
  ->ignore

  // register event listeners for the input method
  VSCode.Window.onDidChangeTextEditorSelection(.event => {
    let document = VSCode.TextEditor.document(editor)
    let intervals =
      event
      ->VSCode.TextEditorSelectionChangeEvent.selections
      ->Array.map(selection => (
        IM.toOffset(document, VSCode.Selection.start(selection)),
        IM.toOffset(document, VSCode.Selection.end_(selection)),
      ))

    Handle__InputMethod.select(state, intervals)->Promise.get(
      TaskQueue.addToTheFront(dispatcher.critical),
    )
  })->subscribe
  VSCode.Workspace.onDidChangeTextDocument(.event => {
    let input = IM.Input.fromTextDocumentChangeEvent(editor, event)
    Handle__InputMethod.EditorIM.runAndHandle(state, input)->Promise.get(
      TaskQueue.addToTheFront(dispatcher.critical),
    )
  })->subscribe

  // remove it from the Registry if it requests to be destroyed
  state->State.onRemoveFromRegistry->Promise.get(removeFromRegistry)

  // definition provider for go-to-definition
  let definitionProvider = (fileName, point) => {
    // only provide source location, when the filenames are matched
    let currentFileName =
      state.editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath

    if fileName == currentFileName {
      Decoration.lookupSrcLoc(state.decorations, point)
    } else {
      None
    }
  }

  // hover provider
  let hoverProvider = (fileName, point) => {
    // only provide source location, when the filenames are matched
    let currentFileName =
      state.editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath

    if fileName == currentFileName {
      let range = VSCode.Range.make(point, point)
      Some(Promise.resolved(([""], range)))
    } else {
      None
    }
  }

  // registering feature providers
  let disposables = Editor.Provider.registerProvider(definitionProvider, hoverProvider)
  state.subscriptions = Js.Array.concat(state.subscriptions, disposables)

  // these two arrays are called "legends"
  let tokenTypes = Highlighting.Aspect.TokenType.enumurate
  let tokenModifiers = Highlighting.Aspect.TokenModifier.enumurate

  let documentSemanticTokensProvider = (fileName, push) => {
    let useSemanticHighlighting = Config.getSemanticHighlighting()
    // Js.log("useSemanticHighlighting")
    let document = VSCode.TextEditor.document(editor)
    let currentFileName = document->VSCode.TextDocument.fileName->Parser.filepath
    if useSemanticHighlighting && fileName == currentFileName {
      Some(Decoration.generateSemanticTokens(editor, state.decorations.highlightings, push))
    } else {
      None
    }
  }
  let disposables = Editor.Provider.registerTestingProvider(
    documentSemanticTokensProvider,
    (tokenTypes, tokenModifiers),
  )
  state.subscriptions = Js.Array.concat(state.subscriptions, disposables)

  // return the dispatcher
  dispatcher
}

// wait until all tasks have finished executing
let destroy = self =>
  self.critical
  ->TaskQueue.onEmptied
  ->Promise.flatMap(() => self.blocking->TaskQueue.onEmptied)
  ->Promise.flatMap(() => self.state->State.destroy)

// destroy everything in a rather violent way
let forceDestroy = self =>
  self.critical
  ->TaskQueue.forceDestroy
  ->Promise.flatMap(() => self.blocking->TaskQueue.forceDestroy)
  ->Promise.flatMap(() => self.state->State.destroy)
