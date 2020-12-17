open! Task
open Belt

// helper function of `executeTask`
let rec sendAgdaRequest = (
  dispatchCommand: Command.t => Promise.t<unit>,
  state: State.t,
  request: Request.t,
): Promise.t<unit> => {
  let printLog = false
  let (log, log2) = if printLog {
    (Js.log, Js.log2)
  } else {
    (_ => (), (_, _) => ())
  }

  let displayConnectionError = error => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), Plain(body))
  }

  // deferred responses are queued here
  let deferredLastResponses = []

  // this promise get resolved after the request to Agda is completed
  let (promise, resolve) = Promise.pending()
  let handle = ref(None)
  let handler: result<Connection.response, Connection.Error.t> => unit = x =>
    switch x {
    | Error(error) => displayConnectionError(error)->ignore
    | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
      let body = Parser.Error.toString(error)
      display(state, Error("Internal Parse Error"), Plain(body))->ignore
    | Ok(Yield(Ok(NonLast(response)))) =>
      Handle__Response.handle(
        state,
        dispatchCommand,
        sendAgdaRequest(dispatchCommand, state),
        response,
      )->ignore
    | Ok(Yield(Ok(Last(priority, response)))) =>
      Js.Array.push(response, deferredLastResponses)->ignore
    | Ok(Stop) =>
      // handle the deferred response
      deferredLastResponses
      ->Array.map(
        Handle__Response.handle(state, dispatchCommand, sendAgdaRequest(dispatchCommand, state)),
      )
      ->Util.oneByOne
      ->Promise.get(_ => resolve())
    }

  state->State.connect->Promise.mapOk(connection => {
    let document = VSCode.TextEditor.document(state.editor)
    let version = connection.metadata.version
    let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
    let libraryPath = Config.getLibraryPath()
    let highlightingMethod = Config.getHighlightingMethod()
    let backend = Config.getBackend()
    let encoded = Request.encode(
      document,
      version,
      filepath,
      backend,
      libraryPath,
      highlightingMethod,
      request,
    )
    log2("<<<", encoded)
    Connection.send(encoded, connection)
    connection
  })->Promise.flatMap(x =>
    switch x {
    | Ok(connection) =>
      handle := Some(connection.Connection.chan->Chan.on(handler))
      promise
    | Error(error) => displayConnectionError(error)->Promise.flatMap(() => promise)
    }
  )->Promise.tap(() => handle.contents->Option.forEach(f => f()))
}
