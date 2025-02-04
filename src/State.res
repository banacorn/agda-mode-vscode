open State__Type
module View = State__View
module Context = State__Type.Context
type t = t

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  State
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let onDownload = (state, event) => {
  open Connection__Download__GitHub.Download.Event
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

let handleDownloadPolicy = async (state, policy) => {
  switch policy {
  | Config.Connection.Download.YesKeepUpToDate => await View.Panel.display(state, Plain("Trying to download and install the latest Agda Language Server and keep it up-to-date"), [])
  | YesButDontUpdate => await View.Panel.display(state, Plain("Trying to download and install the latest Agda Language Server"), [])
  | NoDontAskAgain => await View.Panel.displayConnectionError(state, CannotFindALSorAgda)
  | Undecided =>
    // ask the user
    let messageOptions = {
      VSCode.MessageOptions.modal: true,
      detail: "Do you want to download and install the latest Agda Language Server?",
    }
    let result = await VSCode.Window.showWarningMessageWithOptions(
      "Cannot find Agda or Agda Language Server",
      messageOptions,
      [
        "Yes, and keep it up-to-date",
        "Yes, but don't update afterwards",
        "No, and don't ask again",
      ],
    )

    // update the policy
    let newPolicy = switch result {
    | Some("Yes, and keep it up-to-date") => Config.Connection.Download.YesKeepUpToDate
    | Some("Yes, but don't update afterwards") => Config.Connection.Download.YesButDontUpdate
    | Some("No, and don't ask again") => Config.Connection.Download.NoDontAskAgain
    | _ => Config.Connection.Download.Undecided
    }

    await Config.Connection.Download.setDownloadPolicy(newPolicy)
  }
}

let connectionErrorHandler = async (state, error) => {
  switch error {
  | Connection__Error.CannotFindALSorAgda =>
    let policy = Config.Connection.Download.getDownloadPolicy()
    await handleDownloadPolicy(state, policy)

  | _ => await View.Panel.displayConnectionError(state, error)
  }
}

let sendRequest = async (
  state: state,
  handleResponse: Response.t => promise<unit>,
  request: Request.t,
): unit => {
  let sendRequestAndHandleResponses = async (
    connection,
    state,
    request,
    handler: Response.t => promise<unit>,
  ) => {
    let onResponse = async response => {
      await handler(response)
      state.channels.log->Chan.emit(ResponseHandled(response))
    }

    state.channels.log->Chan.emit(RequestSent(request))
    // only resolve the promise after:
    //  1. the result of connection has been displayed
    //  2. all responses have been handled
    switch await Connection.sendRequest(connection, state.document, request, onResponse) {
    | Error(error) => await connectionErrorHandler(state, error)
    | Ok(status) =>
      // display the connection status
      await View.Panel.displayConnectionStatus(state, status)
      // update the Agda version
      switch status {
      | Agda(version, _) => state.agdaVersion = Some(version)
      | ALS(_alsVersion, agdaVersion, _) => state.agdaVersion = Some(agdaVersion)
      }
    }
  }

  switch state.connection {
  | None =>
    switch await Connection.make(state.memento) {
    | Error(error) => await connectionErrorHandler(state, error)
    | Ok(connection) =>
      state.connection = Some(connection)
      await state.agdaRequestQueue->RequestQueue.push(
        request => sendRequestAndHandleResponses(connection, state, request, handleResponse),
        request,
      )
    }
  | Some(connection) =>
    await state.agdaRequestQueue->RequestQueue.push(
      request => sendRequestAndHandleResponses(connection, state, request, handleResponse),
      request,
    )
  }
}

// like `sendRequest` but collects all responses, for testing
let sendRequestAndCollectResponses = async (state: state, request: Request.t): array<
  Response.t,
> => {
  let responses = ref([])
  let responseHandler = async response => {
    responses.contents->Array.push(response)
  }
  await state->sendRequest(responseHandler, request)
  responses.contents
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
  state.connection->Connection.destroy
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (channels, globalStorageUri, extensionPath, memento, editor) => {
  connection: None,
  agdaVersion: None,
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
  globalStorageUri,
  extensionPath,
  memento: State__Memento.make(memento),
  channels,
}
