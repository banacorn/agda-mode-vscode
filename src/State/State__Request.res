let askUserAboutDownloadPolicy = async () => {
  let messageOptions = {
    VSCode.MessageOptions.modal: true,
    detail: "Do you want to download and install the latest Agda Language Server?",
  }
  let result = await VSCode.Window.showWarningMessageWithOptions(
    "Cannot find Agda or Agda Language Server",
    messageOptions,
    [Config.Connection.DownloadPolicy.toString(Yes), Config.Connection.DownloadPolicy.toString(No)],
  ) // 📺

  // parse the result
  result->Option.mapOr(
    Config.Connection.DownloadPolicy.Undecided,
    Config.Connection.DownloadPolicy.fromString,
  )
}

let downloadLatestALS = (state: State.t) => async platform => {
  let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
  await Connection.downloadLatestALS(
    // ⬇️
    state.memento,
    state.globalStorageUri,
    platform,
    reportProgress,
  )
}

// let handleDownloadPolicy = async (state, dispatchCommand, errors, policy) => {
//   switch policy {
//   | Config.Connection.DownloadPolicy.Yes =>
//     await State__View.Panel.display(
//       state,
//       Plain("Trying to download and install the latest Agda Language Server"),
//       [],
//     ) // 📺

//     switch await Connection__Download__Platform.determine() {
//     | Ok(platform) =>
//       let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
//       switch await Connection.downloadLatestALS(
//         // ⬇️
//         state.memento,
//         state.globalStorageUri,
//         platform,
//         reportProgress,
//       ) {
//       | Error(error) => await State__View.Panel.displayConnectionError(state, Download(error)) // 📺
//       | Ok(_) => await dispatchCommand(Command.Load) // 💨
//       }
//     | Error(raw) =>
//       await State__View.Panel.displayConnectionError(state, TempPlatformNotSupported(raw)) // 📺
//     }

//   | No => await State__View.Panel.displayConnectionError(state, CommandsNotFound(errors)) // 📺
//   | Undecided =>
//     // ask the user
//     let newPolicy = await askUserAboutDownloadPolicy()
//     // update the policy
//     await Config.Connection.DownloadPolicy.set(newPolicy)
//   }
// }

// let onCommandsNotFoundError = async (state, dispatchCommand, errors) => {
//   let policy = Config.Connection.DownloadPolicy.get()
//   await handleDownloadPolicy(state, dispatchCommand, errors, policy)
// }

let connectionErrorHandler = async (state, dispatchCommand, error) => {
  switch error {
  // | Connection__Error.CommandsNotFound(errors) =>
  //   await onCommandsNotFoundError(state, dispatchCommand, errors)
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
    | Error(error) => await connectionErrorHandler(state, dispatchCommand, error)
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
    let platform = await Connection__Download__Platform.determine()
    switch await Connection.make(
      state.memento,
      Config.Connection.getAgdaPaths(),
      ["als", "agda"],
      platform,
      askUserAboutDownloadPolicy,
      downloadLatestALS(state),
    ) {
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
