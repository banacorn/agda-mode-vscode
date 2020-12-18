open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  view: View__Controller.t,
  mutable connection: option<Connection.t>,
  mutable goals: array<Goal.t>,
  mutable decorations: Decoration.t,
  mutable cursor: option<VSCode.Position.t>,
  editorIM: IM.t,
  promptIM: IM.t,
  mutable subscriptions: array<VSCode.Disposable.t>,
  // for self destruction
  onRemoveFromRegistry: Chan.t<unit>,
}

module Decoration = {
  let make = Decoration.make

  let addViaPipe = (state, highlightings) =>
    state.decorations->Decoration.addDirectly(highlightings)

  let addViaFile = (state, filepath) => state.decorations->Decoration.addIndirectly(filepath)

  let clear = state => Decoration.removeAppliedDecorations(state.decorations)

  let apply = state => Decoration.readTempFiles(state.decorations)->Promise.map(() => {
      Decoration.applyHighlightings(state.decorations, state.editor)
    })

  let refresh = state => {
    // highlightings
    Decoration.refresh(state.decorations, state.editor)
    // goal decorations
    state.goals->Array.forEach(goal => goal->Goal.refreshDecoration(state.editor))
  }

  let destroy = state => Decoration.destroy(state.decorations)
}

// events to the FileName-Dispatch Registry
let onRemoveFromRegistry = state => state.onRemoveFromRegistry->Chan.once
let emitRemoveFromRegistry = state => state.onRemoveFromRegistry->Chan.emit()

// connect if not connected yet
let connect = state =>
  switch state.connection {
  | None =>
    Connection.make(Config.getAgdaPath, Config.setAgdaPath)->Promise.tapOk(conn =>
      state.connection = Some(conn)
    )
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
  state->emitRemoveFromRegistry
  state.onRemoveFromRegistry->Chan.destroy
  state.goals->Array.forEach(Goal.destroy)
  state->Decoration.destroy
  setLoaded(false)
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state->disconnect
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (extentionPath, chan, editor) => {
  setLoaded(true)
  // view initialization
  let view = View__Controller.make(extentionPath, editor)

  {
    editor: editor,
    document: VSCode.TextEditor.document(editor),
    view: view,
    connection: None,
    goals: [],
    decorations: Decoration.make(),
    cursor: None,
    editorIM: IM.make(chan),
    promptIM: IM.make(chan),
    subscriptions: [],
    onRemoveFromRegistry: Chan.make(),
  }
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
