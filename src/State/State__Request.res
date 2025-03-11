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
    Config.Connection.DownloadPolicy.No,
    Config.Connection.DownloadPolicy.fromString,
  )
}

module LatestALS = {
  // check if the latest ALS is already downloaded
  let alreadyDownloaded = (state: State.t) => async () => {
    let path = NodeJs.Path.join([VSCode.Uri.fsPath(state.globalStorageUri), "latest-als"])
    switch await NodeJs.Fs.access(path) {
    | () =>
      switch await Connection.Target.fromRawPath(path) {
      | Ok(target) => Some(target)
      | Error(_) => None
      }
    | exception _ => None
    }
  }

  let download = (state: State.t) => async platform => {
    let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
    await Connection.downloadLatestALS(
      // ⬇️
      state.memento,
      state.globalStorageUri,
      platform,
      reportProgress,
    )
  }
}

let sendRequest = async (
  state: State.t,
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
    | Error(error) => await State__View.Panel.displayConnectionError(state, error)
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
      LatestALS.alreadyDownloaded(state),
      LatestALS.download(state),
    ) {
    | Error(error) => await State__View.Panel.displayConnectionError(state, error)
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
let sendRequestAndCollectResponses = async (state: State.t, request: Request.t): array<
  Response.t,
> => {
  let responses = ref([])
  let responseHandler = async response => {
    responses.contents->Array.push(response)
  }
  await state->sendRequest(responseHandler, request)
  responses.contents
}
