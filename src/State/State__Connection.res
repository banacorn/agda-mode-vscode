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
    | Ok() =>
      // display the connection state
      await State__View.Panel.displayConnectionStatus(state, Some(connection))
      // update versions
      switch connection {
      | Agda(_, _, version) => state.agdaVersion = Some(version)
      | ALS(_, _, Some(_alsVersion, agdaVersion, _)) => state.agdaVersion = Some(agdaVersion)
      | ALS(_, _, None) => state.agdaVersion = None
      | ALSWASM(_, _, _, Some(_alsVersion, agdaVersion, _)) => state.agdaVersion = Some(agdaVersion)
      | ALSWASM(_, _, _, None) => state.agdaVersion = None
      }
    }
  }

  switch state.connection {
  | None =>
    switch await Connection.makeWithFallback(
      state.platformDeps,
      state.memento,
      state.globalStorageUri,
      Config.Connection.getAgdaPaths(),
      ["als", "agda"],
      state.channels.log,
    ) {
    | Error(error) => await State__View.Panel.displayConnectionError(state, error)
    | Ok(connection) =>
      state.connection = Some(connection)
      await sendRequestAndHandleResponses(connection, state, request, handleResponse)
    }
  | Some(connection) =>
    await sendRequestAndHandleResponses(connection, state, request, handleResponse)
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
