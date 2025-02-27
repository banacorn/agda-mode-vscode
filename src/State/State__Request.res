let handleDownloadPolicy = async (state, dispatchCommand, errors, policy) => {
  switch policy {
  | Config.Connection.Download.YesKeepUpToDate =>
    await State__View.Panel.display(
      state,
      Plain(
        "Trying to download and install the latest Agda Language Server and keep it up-to-date",
      ),
      [],
    )

    let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server")
    switch await Connection.downloadLatestALS(
      state.memento,
      state.globalStorageUri,
      reportProgress,
    ) {
    | Error(error) => await State__View.Panel.displayConnectionError(state, error)
    | Ok(_) => await dispatchCommand(Command.Load)
    }
  // State__SwitchVersion.LatestALS.download(state.memento, VSCode.Uri.fsPath(state.globalStorageUri),
  | YesButDontUpdate =>
    await State__View.Panel.display(
      state,
      Plain("Trying to download and install the latest Agda Language Server"),
      [],
    )
  | NoDontAskAgain =>
    await State__View.Panel.displayConnectionError(state, CommandsNotFound(errors))
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

let onCommandsNotFoundError = async (state, dispatchCommand, errors) => {
  let policy = Config.Connection.Download.getDownloadPolicy()
  await handleDownloadPolicy(state, dispatchCommand, errors, policy)
}

let connectionErrorHandler = async (state, dispatchCommand, error) => {
  switch error {
  | Connection__Error.CommandsNotFound(errors) =>
    await onCommandsNotFoundError(state, dispatchCommand, errors)
  | _ => await State__View.Panel.displayConnectionError(state, error)
  }
}

let sendRequest = async (
  state: State.t,
  dispatchCommand: Command.t => promise<unit>,
  handleResponse: Response.t => promise<unit>,
  request: Request.t,
): unit => {
  let sendRequestAndHandleResponses = async (
    connection,
    state: State.t,
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
    | Error(error) => 
      let (header, body) = Connection.Error.toString(error)
      Js.log("Error from sendRequestAndHandleResponses: " ++ header ++ " : " ++ body)
      await connectionErrorHandler(state, dispatchCommand, error)
    | Ok(status) =>
      // display the connection status
      await State__View.Panel.displayConnectionStatus(state, status)
      // update the Agda version
      switch status {
      | Agda(version, _) => state.agdaVersion = Some(version)
      | ALS(_alsVersion, agdaVersion, _) => state.agdaVersion = Some(agdaVersion)
      }
    }
  }

  switch state.connection {
  | None =>
    switch await Connection.make(state.memento, Config.Connection.getAgdaPaths(), ["als", "agda"]) {
    | Error(error) => await connectionErrorHandler(state, dispatchCommand, error)
    | Ok(connection) =>
      state.connection = Some(connection)
      await state.agdaRequestQueue->State.RequestQueue.push(
        request => sendRequestAndHandleResponses(connection, state, request, handleResponse),
        request,
      )
    }
  | Some(connection) =>
    await state.agdaRequestQueue->State.RequestQueue.push(
      request => sendRequestAndHandleResponses(connection, state, request, handleResponse),
      request,
    )
  }
}

// like `sendRequest` but collects all responses, for testing
let sendRequestAndCollectResponses = async (
  state: State.t,
  dispatchCommand,
  request: Request.t,
): array<Response.t> => {
  let responses = ref([])
  let responseHandler = async response => {
    responses.contents->Array.push(response)
  }
  await state->sendRequest(dispatchCommand, responseHandler, request)
  responses.contents
}
