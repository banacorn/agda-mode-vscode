open! Task
open Belt

// helper function of `executeTask`
let rec sendAgdaRequest = (
  dispatchCommand: Command.t => Promise.t<unit>,
  state: State.t,
  request: Request.t,
): Promise.t<unit> => {
  Js.log("<<< " ++ Request.toString(request))
  let displayConnectionError = error => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), Plain(body))
  }

  // deferred responses are queued here
  let deferredLastResponses: array<(int, Response.t)> = []

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
      Js.log(">>> " ++ Response.toString(response))
      Handle__Response.handle(
        state,
        dispatchCommand,
        sendAgdaRequest(dispatchCommand, state),
        response,
      )->ignore
    | Ok(Yield(Ok(Last(priority, response)))) =>
      Js.log(">>* " ++ string_of_int(priority) ++ " " ++ Response.toString(response))
      Js.Array.push((priority, response), deferredLastResponses)->ignore
    | Ok(Stop) =>
      Js.log(">>| ")
      // sort the deferred Responses by priority (ascending order)
      let deferredLastResponses =
        Js.Array.sortInPlaceWith(
          (x, y) => compare(fst(x), fst(y)),
          deferredLastResponses,
        )->Array.map(snd)

      // apply decorations after all "NonLast" tasks before all "Last" tasks
      Handle__Decoration.apply(state)->Promise.map(() =>
        // handle the deferred response
        deferredLastResponses->Array.map(
          Handle__Response.handle(state, dispatchCommand, sendAgdaRequest(dispatchCommand, state)),
        )
      )->Promise.flatMap(Util.oneByOne)->Promise.get(_ => resolve())
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
