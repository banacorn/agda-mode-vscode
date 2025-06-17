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

// datatype for logging
module Log = {
  type t =
    | CommandDispatched(Command.t)
    | CommandHandled(Command.t)
    | RequestSent(Request.t)
    | ResponseHandled(Response.t)
    | Others(string) // generic string

  let toString = log =>
    switch log {
    | CommandDispatched(command) => " <=== " ++ Command.toString(command)
    | RequestSent(request) => "   <- " ++ Request.toString(request)
    | ResponseHandled(response) => "    > " ++ Response.toString(response)
    | CommandHandled(command) => " ===> " ++ Command.toString(command)
    | Others(str) => str
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
  // connection
  mutable connection: option<Connection.t>,
  mutable agdaVersion: option<string>, // Agda version is set when connection is established
  // editor and document
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  // view
  panelCache: ViewCache.t,
  mutable runningInfoLog: array<(int, string)>,
  // mutable goals: array<Goal.t>,
  tokens: Tokens.t,
  goals2: Goals.t,
  mutable cursor: option<VSCode.Position.t>,
  editorIM: IM.t,
  promptIM: IM.t,
  mutable subscriptions: array<VSCode.Disposable.t>,
  // for self destruction
  onRemoveFromRegistry: Chan.t<unit>,
  globalStorageUri: VSCode.Uri.t,
  extensionPath: string,
  memento: State__Memento.t,
  // for logging and testing
  channels: channels,
}

let make = (
  channels,
  globalStorageUri,
  extensionPath,
  memento,
  editor,
  semanticTokens: option<Resource.t<array<Highlighting__SemanticToken.t>>>,
) => {
  connection: None,
  agdaVersion: None,
  editor,
  document: VSCode.TextEditor.document(editor),
  panelCache: ViewCache.make(),
  runningInfoLog: [],
  // goals: [],
  goals2: Goals.make(),
  tokens: Tokens.make(semanticTokens),
  cursor: None,
  editorIM: IM.make(channels.inputMethod),
  promptIM: IM.make(channels.inputMethod),
  subscriptions: [],
  onRemoveFromRegistry: Chan.make(),
  globalStorageUri,
  extensionPath,
  memento: State__Memento.make(memento),
  channels,
}

// construction/destruction
let destroy = async (state, alsoRemoveFromRegistry) => {
  await state.goals2->Goals.waitUntilNotBusy
  if alsoRemoveFromRegistry {
    state.onRemoveFromRegistry->Chan.emit()
  }
  state.onRemoveFromRegistry->Chan.destroy
  state.goals2->Goals.destroy
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  await state.connection->Connection.destroy
  // TODO: delete files in `.indirectHighlightingFileNames`
}

// control the scope of command key-binding
module Context = {
  // input method related key-bindings
  let setPrompt = value => VSCode.Commands.setContext("agdaModePrompting", value)->ignore
  let setIM = value => VSCode.Commands.setContext("agdaModeTyping", value)->ignore
}
