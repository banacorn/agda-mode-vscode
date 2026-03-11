// cache the stuff previously displayed in the view, so that we can restore them later
module ViewCache = {
  type t = {
    mutable display: option<(View.Header.t, View.Body.t)>,
    mutable prompt: option<(View.Header.t, View.Prompt.t, View.Response.t => promise<unit>)>,
  }

  let make = () => {
    display: None,
    prompt: None,
  }

  let cacheEvent = (self, event: View.EventToView.t) =>
    switch event {
    // cache the event only when it's a "Display"
    | Display(header, body) => self.display = Some((header, body))
    | _ => ()
    }

  let cacheRequest = (self, event: View.Request.t, callback) =>
    switch event {
    | Prompt(header, prompt) => self.prompt = Some(header, prompt, callback)
    }

  let clearPrompt = self => self.prompt = None

  // if there's no Prompt, then restore Display instead
  let restore = (self, view) =>
    switch self.prompt {
    | Some((header, prompt, callback)) =>
      view->WebviewPanel.sendRequest(Prompt(header, prompt), callback)->ignore
    | None =>
      self.display->Option.forEach(((header, body)) =>
        view->WebviewPanel.sendEvent(Display(header, body))->ignore
      )
    }
}

type channels = {
  inputMethod: Chan.t<IM.Log.t>,
  // emits when a Response has been handled
  responseHandled: Chan.t<Response.t>,
  // emits when a Command has been handled
  commandHandled: Chan.t<Command.t>,
  // for debugging
  log: Chan.t<Log.t>,
}

type t = {
  // unique ID for identifying this state instance in the connection registry
  id: string,
  // platform dependencies for dependency injection
  platformDeps: Platform.t,
  // Agda version is set when connection is established
  mutable agdaVersion: option<string>,
  // editor and document
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  // view
  panelCache: ViewCache.t,
  mutable runningInfoLog: array<(int, string)>,
  // mutable goals: array<Goal.t>,
  tokens: Tokens.t,
  goals: Goals.t,
  mutable cursor: option<VSCode.Position.t>,
  editorIM: IM.t,
  promptIM: IM.t,
  mutable subscriptions: array<VSCode.Disposable.t>,
  // for self destruction
  onRemoveFromRegistry: Chan.t<unit>,
  globalStorageUri: VSCode.Uri.t,
  extensionUri: VSCode.Uri.t,
  memento: Memento.t,
  // for logging and testing
  channels: channels,
  // Skip HighlightingInfo during refine & give operations because Agda sends faulty token positions
  // that conflict with the extension's correctly calculated positions.
  mutable isInRefineOrGiveOperation: bool,
}

let make = (
  id,
  platformDeps,
  channels,
  globalStorageUri,
  extensionUri,
  memento,
  editor,
  semanticTokens: option<Resource.t<array<Highlighting__SemanticToken.t>>>,
) => {
  id,
  platformDeps,
  agdaVersion: None,
  editor,
  document: VSCode.TextEditor.document(editor),
  panelCache: ViewCache.make(),
  runningInfoLog: [],
  // goals: [],
  goals: Goals.make(),
  tokens: Tokens.make(semanticTokens),
  cursor: None,
  editorIM: IM.make(channels.inputMethod),
  promptIM: IM.make(channels.inputMethod),
  subscriptions: [],
  onRemoveFromRegistry: Chan.make(),
  globalStorageUri,
  extensionUri,
  memento,
  channels,
  isInRefineOrGiveOperation: false,
}

// construction/destruction
let destroy = async (state, alsoRemoveFromRegistry) => {
  state.channels.log->Chan.emit(Others("State.destroy: Starting destruction"))
  await state.goals->Goals.waitUntilNotBusy
  state.channels.log->Chan.emit(Others("State.destroy: Goals are not busy"))
  if alsoRemoveFromRegistry {
    state.channels.log->Chan.emit(Others("State.destroy: Emitting remove from registry"))
    state.onRemoveFromRegistry->Chan.emit()
  }
  state.onRemoveFromRegistry->Chan.destroy
  state.channels.log->Chan.emit(Others("State.destroy: Destroyed onRemoveFromRegistry channel"))
  state.goals->Goals.destroy
  state.channels.log->Chan.emit(Others("State.destroy: Destroyed goals"))
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state.channels.log->Chan.emit(Others("State.destroy: Disposed subscriptions"))
  state.channels.log->Chan.emit(TokensReset("State.destroy"))
  state.tokens->Tokens.reset
  state.tokens->Tokens.destroyUpdateChannel
  state.channels.log->Chan.emit(Others("State.destroy: Tokens reset completed"))
  await Registry__Connection.release(state.id)
  state.channels.log->Chan.emit(Others("State.destroy: Connection released, destruction complete"))
  Ok()
}

// control the scope of command key-binding
module Context = {
  // input method related key-bindings
  let setPrompt = value => VSCode.Commands.setContext("agdaModePrompting", value)->ignore
  let setIM = value => VSCode.Commands.setContext("agdaModeTyping", value)->ignore
}
