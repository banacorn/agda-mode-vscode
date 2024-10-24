open State__Type
module View = State__View
module Context = State__Type.Context
type t = t

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  State
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let onDownload = (state, event) => {
  open LanguageServerMule.Source.GitHub.Download.Event
  switch event {
  | Start => View.Panel.displayStatus(state, "Start downloading")->ignore
  | Progress(accum, total) =>
    // if the file is larger than 10MB than we use MB as the unit
    let message =
      total > 10485760
        ? "Downloading ( " ++
          string_of_int(accum / 1048576) ++
          " MB / " ++
          string_of_int(total / 1048576) ++ " MB )"
        : "Downloading ( " ++
          string_of_int(accum / 1024) ++
          " KB / " ++
          string_of_int(total / 1024) ++ " MB )"
    View.Panel.displayStatus(state, message)->ignore
  | Finish => View.Panel.displayStatus(state, "Downloaded")->ignore
  }
}

let sendRequest = (
  state: state,
  handleResponse: Response.t => promise<unit>,
  request: Request.t,
): promise<unit> => {
  let sendRequestAndHandleResponses = (
    state,
    request,
    handler: Response.t => promise<unit>,
  ): promise<unit> => {
    let onResponse = async result =>
      switch result {
      | Error(error) => await View.Panel.displayConnectionError(state, error)
      | Ok(response) =>
        await handler(response)
        // switch await handler(response) {
        // | Error(error) => await View.Panel.displayConnectionError(state, error)
        // | Ok() => ()
        // }
      }
    Connection.sendRequest(
      state.globalStoragePath,
      onDownload(state, ...),
      Config.Connection.getUseAgdaLanguageServer(),
      state.document,
      request,
      onResponse
    )->Promise.then(async result =>
      switch result {
      | Error(error) =>
        await View.Panel.displayConnectionError(state, error)
      | Ok(status) =>
        await View.Panel.displayConnectionStatus(state, status)
      }
    )
  }

  state.agdaRequestQueue->RequestQueue.push(
    request => sendRequestAndHandleResponses(state, request, handleResponse),
    request,
  )
}

// construction/destruction
let destroy = (state, alsoRemoveFromRegistry) => {
  if alsoRemoveFromRegistry {
    state.onRemoveFromRegistry->Chan.emit()
  }
  state.onRemoveFromRegistry->Chan.destroy
  state.goals->Array.forEach(Goal.destroyDecoration)
  state.highlighting->Highlighting.destroy
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  Connection.stop()
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (channels, globalStoragePath, extensionPath, editor) => {
  editor,
  document: VSCode.TextEditor.document(editor),
  panelCache: ViewCache.make(),
  runningInfoLog: [],
  goals: [],
  tokens: Tokens.make(),
  highlighting: Highlighting.make(),
  cursor: None,
  editorIM: IM.make(channels.inputMethod),
  promptIM: IM.make(channels.inputMethod),
  subscriptions: [],
  onRemoveFromRegistry: Chan.make(),
  agdaRequestQueue: RequestQueue.make(),
  globalStoragePath,
  extensionPath,
  channels,
}
