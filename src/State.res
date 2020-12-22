open Belt

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  view: ViewController.t,
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

type state = t

module Decoration = {
  let make = Decoration.make

  let addViaPipe = (state, highlightings) =>
    state.decorations->Decoration.addDirectly(highlightings)

  let addViaFile = (state, filepath) => state.decorations->Decoration.addIndirectly(filepath)

  let clear = state => Decoration.removeAppliedDecorations(state.decorations)

  let apply = state =>
    Decoration.readTempFiles(state.decorations)->Promise.map(() => {
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
  state.view->ViewController.destroy
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
  let view = ViewController.make(extentionPath, editor)

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
module type View = {
  let show: state => unit
  let hide: state => unit
  // display stuff
  let display: (state, View.Header.t, View.Body.t) => Promise.t<unit>
  let displayEmacs: (state, View.Body.Emacs.t, View.Header.t, string) => Promise.t<unit>
  let displayOutOfGoalError: state => Promise.t<unit>
  // Input Method
  let updateIM: (state, View.EventToView.InputMethod.t) => Promise.t<unit>
  let updatePromptIM: (state, string) => Promise.t<unit>
  // Prompt
  let prompt: (state, View.Header.t, View.Prompt.t, string => Promise.t<unit>) => Promise.t<unit>
  let interruptPrompt: state => Promise.t<unit>
}
module View: View = {
  let show = state => {
    state.view->ViewController.show
    setLoaded(true)
  }
  let hide = state => {
    state.view->ViewController.hide
    setLoaded(false)
  }

  // display stuff
  let display = (state, header, body) => ViewController.sendEvent(state.view, Display(header, body))
  let displayEmacs = (state, kind, header, body) =>
    ViewController.sendEvent(
      state.view,
      Display(header, Emacs(kind, View.Header.toString(header), body)),
    )
  let displayOutOfGoalError = state =>
    display(state, Error("Out of goal"), Plain("Please place the cursor in a goal"))

  // update the Input Method
  let updateIM = (state, event) => ViewController.sendEvent(state.view, InputMethod(event))
  let updatePromptIM = (state, content) =>
    ViewController.sendEvent(state.view, PromptIMUpdate(content))

  // Header + Prompt
  let prompt = (
    state,
    header,
    prompt,
    callbackOnPromptSuccess: string => Promise.t<unit>,
  ): Promise.t<unit> => {
    // focus on the panel before prompting
    VSCode.Commands.setContext("agdaModePrompting", true)->ignore
    state.view->ViewController.focus

    // send request to view
    ViewController.sendRequest(state.view, Prompt(header, prompt), response =>
      switch response {
      | PromptSuccess(result) =>
        callbackOnPromptSuccess(result)->Promise.map(() => {
          // put the focus back to the editor after prompting
          VSCode.Commands.setContext("agdaModePrompting", false)->ignore
          state.document->Editor.focus
        })
      | PromptInterrupted => Promise.resolved()
      }
    )
  }
  let interruptPrompt = state => ViewController.sendEvent(state.view, PromptInterrupt)
}
