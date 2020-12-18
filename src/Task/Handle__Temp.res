open! Task
open Belt

// There are 2 kinds of Responses
//  NonLast Response :
//    * get handled first
//    * don't invoke `sendAgdaRequest`
//  Last Response :
//    * have priorities, those with the smallest priority number are executed first
//    * only get handled:
//        1. after prompt has reappeared
//        2. after all NonLast Responses
//        3. after all interactive highlighting is complete
//    * may invoke `sendAgdaRequest`

// This module makes sure that Last Responses are handled after NonLast Responses
module Lock: {
  let runNonLast: Promise.t<'a> => Promise.t<'a>
  let onceDone: unit => Promise.t<unit>
} = {
  // keep the number of running NonLast Response
  let tally = ref(0)
  let allDone = Chan.make()
  // NonLast Responses should fed here
  let runNonLast = promise => {
    tally := tally.contents + 1
    promise->Promise.tap(_ => {
      tally := tally.contents - 1
      if tally.contents == 0 {
        allDone->Chan.emit()
      }
    })
  }
  // gets resolved once there's no NonLast Responses running
  let onceDone = () =>
    if tally.contents == 0 {
      Promise.resolved()
    } else {
      allDone->Chan.once
    }
}

// helper function of `executeTask`
let rec sendAgdaRequest = (
  dispatchCommand: Command.t => Promise.t<unit>,
  state: State.t,
  request: Request.t,
): Promise.t<unit> => {
  // Js.log("<<< " ++ Request.toString(request))
  let displayConnectionError = error => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), Plain(body))
  }

  // deferred responses are queued here
  let deferredLastResponses: array<(int, Response.t)> = []

  // this promise get resolved after all Responses has been received from Agda
  let (promise, stopListener) = Promise.pending()
  let handle = ref(None)
  let agdaResponseListener: result<Connection.response, Connection.Error.t> => unit = x =>
    switch x {
    | Error(error) => displayConnectionError(error)->ignore
    | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
      let body = Parser.Error.toString(error)
      display(state, Error("Internal Parse Error"), Plain(body))->ignore
    | Ok(Yield(Ok(NonLast(response)))) =>
      // Js.log(">>> " ++ Response.toString(response))
      Lock.runNonLast(
        Handle__Response.handle(
          state,
          dispatchCommand,
          sendAgdaRequest(dispatchCommand, state),
          response,
        ),
      )->ignore
    | Ok(Yield(Ok(Last(priority, response)))) =>
      // Js.log(">>* " ++ string_of_int(priority) ++ " " ++ Response.toString(response))
      Js.Array.push((priority, response), deferredLastResponses)->ignore
    | Ok(Stop) =>
      // Js.log(">>| ")
      // sort the deferred Responses by priority (ascending order)
      let deferredLastResponses =
        Js.Array.sortInPlaceWith(
          (x, y) => compare(fst(x), fst(y)),
          deferredLastResponses,
        )->Array.map(snd)

      // wait until all NonLast Responses are handled
      Lock.onceDone()
      // stop the Agda Response listener
      ->Promise.tap(_ => stopListener())
      // apply decoration before handling Last Responses
      ->Promise.flatMap(_ => Handle__Decoration.apply(state))
      ->Promise.map(() =>
        deferredLastResponses->Array.map(
          Handle__Response.handle(state, dispatchCommand, sendAgdaRequest(dispatchCommand, state)),
        )
      )
      ->Promise.flatMap(Util.oneByOne)
      ->ignore
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
      handle := Some(connection.Connection.chan->Chan.on(agdaResponseListener))
      promise
    | Error(error) => displayConnectionError(error)->Promise.flatMap(() => promise)
    }
  )->Promise.tap(() => handle.contents->Option.forEach(destroyListener => destroyListener()))
}

// let make = (
//   extentionPath: string,
//   editor: VSCode.TextEditor.t,
//   removeFromRegistry: unit => unit,
//   chan: Chan.t<IM.Log.t>,
// ) => {
//   let state = State.make(extentionPath, chan, editor)
//   let dispatcher = {
//     state: state,
//     blocking: TaskQueue.make(executeTask(state)),
//     critical: TaskQueue.make(executeTask(state)),
//   }

//   let subscribe = disposable => disposable->Js.Array.push(state.subscriptions)->ignore

//   // listens to events from the view
//   state.view
//   ->View__Controller.onEvent(event => dispatchCommand(dispatcher, EventFromView(event))->ignore)
//   ->Js.Array.push(state.subscriptions)
//   ->ignore

//   // register event listeners for the input method
//   VSCode.Window.onDidChangeTextEditorSelection(.event => {
//     let document = VSCode.TextEditor.document(editor)
//     let intervals =
//       event
//       ->VSCode.TextEditorSelectionChangeEvent.selections
//       ->Array.map(selection => (
//         IM.toOffset(document, VSCode.Selection.start(selection)),
//         IM.toOffset(document, VSCode.Selection.end_(selection)),
//       ))

//     Handle__InputMethod.select(state, intervals)->ignore
//   })->subscribe
//   VSCode.Workspace.onDidChangeTextDocument(.event => {
//     let changes = IM.Input.fromTextDocumentChangeEvent(editor, event)
//     Handle__InputMethod.keyUpdateEditorIM(state, changes)->ignore
//   })->subscribe

//   // remove it from the Registry if it requests to be destroyed
//   state->State.onRemoveFromRegistry->Promise.get(removeFromRegistry)

//   // definition provider for go-to-definition
//   let definitionProvider = (fileName, point) => {
//     // only provide source location, when the filenames are matched
//     let currentFileName =
//       state.editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath

//     if fileName == currentFileName {
//       Decoration.lookupSrcLoc(state.decorations, point)
//     } else {
//       None
//     }
//   }

//   // hover provider
//   let hoverProvider = (fileName, point) => {
//     // only provide source location, when the filenames are matched
//     let currentFileName =
//       state.editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath

//     if fileName == currentFileName {
//       let range = VSCode.Range.make(point, point)
//       Some(Promise.resolved(([""], range)))
//     } else {
//       None
//     }
//   }

//   // registering feature providers
//   let disposables = Editor.Provider.registerProvider(definitionProvider, hoverProvider)
//   state.subscriptions = Js.Array.concat(state.subscriptions, disposables)

//   // these two arrays are called "legends"
//   let tokenTypes = Highlighting.Aspect.TokenType.enumurate
//   let tokenModifiers = Highlighting.Aspect.TokenModifier.enumurate

//   let documentSemanticTokensProvider = (fileName, push) => {
//     let useSemanticHighlighting = Config.getSemanticHighlighting()
//     // Js.log("useSemanticHighlighting")
//     let document = VSCode.TextEditor.document(editor)
//     let currentFileName = document->VSCode.TextDocument.fileName->Parser.filepath
//     if useSemanticHighlighting && fileName == currentFileName {
//       Some(Decoration.generateSemanticTokens(editor, state.decorations.highlightings, push))
//     } else {
//       None
//     }
//   }
//   let disposables = Editor.Provider.registerTestingProvider(
//     documentSemanticTokensProvider,
//     (tokenTypes, tokenModifiers),
//   )
//   state.subscriptions = Js.Array.concat(state.subscriptions, disposables)

//   // return the dispatcher
//   dispatcher
// }
