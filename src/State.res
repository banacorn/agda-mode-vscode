open Belt

// type editor = TextEditor.t;
// type context = Editor.context;
type t = {
  mutable editor: VSCode.TextEditor.t,
  view: View__Controller.t,
  mutable connection: option<Connection.t>,
  mutable goals: array<Goal.t>,
  mutable decorations: Decoration.t,
  mutable cursor: option<VSCode.Position.t>,
  editorIM: EditorIM.t,
  promptIM: PromptIM.t,
  mutable subscriptions: array<VSCode.Disposable.t>,
  // for self destruction
  onRemoveFromRegistry: Event.t<unit>,
}

// getters

let getEditor = (state: t) => state.editor

// events to the FileName-Dispatch Registry

let onRemoveFromRegistry = state => state.onRemoveFromRegistry.once()
let emitRemoveFromRegistry = state => state.onRemoveFromRegistry.emit()

// Agda connection/disconnection

// connect if not connected yet
let connect = state =>
  switch state.connection {
  | None =>
    Connection.make(Config.getAgdaPath, Config.setAgdaPath)
    ->Promise.mapError(e => Error.Connection(e))
    ->Promise.tapOk(conn => state.connection = Some(conn))
  | Some(connection) => Promise.resolved(Ok(connection))
  }
let disconnect = state =>
  switch state.connection {
  | None => Promise.resolved()
  | Some(connection) =>
    Connection.destroy(connection)
    Promise.resolved()
  }

// construction/destruction

// set context so that only certain key bindings only work
// when there's a active text editor
let setLoaded = value => VSCode.Commands.setContext("agdaMode", value)->ignore

let destroy = state => {
  state.view->View__Controller.destroy
  state.onRemoveFromRegistry.destroy()
  state.goals->Array.forEach(Goal.destroy)
  state.decorations->Decoration.destroy
  setLoaded(false)
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state->disconnect
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (extentionPath, eventEmitter, editor) => {
  setLoaded(true)
  // view initialization
  let view = View__Controller.make(extentionPath, editor)

  let state = {
    editor: editor,
    view: view,
    connection: None,
    goals: [],
    decorations: Decoration.make(),
    cursor: None,
    editorIM: EditorIM.make(eventEmitter),
    promptIM: PromptIM.make(),
    subscriptions: [],
    onRemoveFromRegistry: Event.make(),
  }

  state
}

// View-related

let show = state => {
  state.view->View__Controller.show
  setLoaded(true)
}
let hide = state => {
  state.view->View__Controller.hide
  setLoaded(false)
}
let sendRequestToView = (state, request) =>
  View__Controller.send(state.view, View.RequestOrEventToView.Request(request))
let sendEventToView = (state, event) =>
  View__Controller.send(state.view, View.RequestOrEventToView.Event(event))
