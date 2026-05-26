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
    let composedHandler = state.middlewares->Array.reduceRight(handler, (h, mw) => mw(h))
    let onResponse = async response => {
      let _ = await Registry__Connection.withOwnerContext(state.id, () => composedHandler(response))
      state.channels.log->Chan.emit(ResponseHandled(response))
    }

    state.channels.log->Chan.emit(RequestSent(request))
    // only resolve the promise after:
    //  1. the result of connection has been displayed
    //  2. all responses have been handled
    let result = await Registry__Connection.execute(state.id, connection =>
      Connection.sendRequest(connection, state.document, request, onResponse)
    )

    switch result {
    | Error(error) => await State__View.Panel.displayConnectionError(state, error)
    | Ok() =>
      // display the connection state
      await State__View.Panel.displayConnectionStatus(state, Some(connection))
      // update versions
      switch connection {
      | Agda(_, _, version) => state.agdaVersion = Some(version)
      | ALS(_, _, {agdaVersion}) => state.agdaVersion = Some(agdaVersion)
      | ALSWASM(_, _, _, {agdaVersion}) => state.agdaVersion = Some(agdaVersion)
      }
    }
  }

  state.channels.log->Chan.emit(Log.Connection(Log.Connection.ActivationFlow(ActivationStarted)))
  let freshEstablishStarted = ref(false)
  let make = () => {
    freshEstablishStarted := true
    state.channels.log->Chan.emit(Log.Connection(Log.Connection.ActivationFlow(FreshEstablishStarted)))
    Connection.makeWithFallback(
      state.platformDeps,
      state.memento,
      state.globalStorageUri,
      Config.Connection.getAgdaPaths(),
      ["als", "agda"],
      state.channels.log,
    )
  }

  switch await Registry__Connection.acquire(state.id, make) {
  | Error(error) =>
    state.channels.log->Chan.emit(Log.Connection(Log.Connection.ActivationFlow(ActivationFailed)))
    await State__View.Panel.displayConnectionError(state, error)
  | Ok(connection) =>
    if !freshEstablishStarted.contents {
      state.channels.log->Chan.emit(Log.Connection(Log.Connection.ActivationFlow(ExistingConnectionReused)))
    }
    state.channels.log->Chan.emit(Log.Connection(Log.Connection.ActivationFlow(ActivationSucceeded)))
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
