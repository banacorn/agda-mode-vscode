open Belt

// For throttling Requests send to Agda
// 1 Request to Agda at a time
module RequestQueue: {
  type t
  let make: unit => t
  // only gets resolved after the Request has been handled
  let push: (t, Request.t => Promise.t<unit>, Request.t) => Promise.t<unit>
} = {
  type t = {
    queue: array<unit => Promise.t<unit>>,
    mutable busy: bool,
  }

  let make = () => {
    queue: [],
    busy: false,
  }

  let rec kickStart = self =>
    if self.busy {
      // busy running, just leave it be
      ()
    } else {
      // pop the front of the queue
      switch Js.Array.shift(self.queue) {
      | None => () // nothing to pop
      | Some(thunk) =>
        self.busy = true
        thunk()->Promise.get(() => {
          self.busy = false
          kickStart(self)
        })
      }
    }

  // only gets resolved after the Request has been handled
  let push = (self, sendRequestAndHandleResponses, request) => {
    let (promise, resolve) = Promise.pending()
    let thunk = () => sendRequestAndHandleResponses(request)->Promise.tap(resolve)
    // push to the back of the queue
    Js.Array.push(thunk, self.queue)->ignore
    // kick start
    kickStart(self)
    promise
  }
}

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  view: ViewController.t,
  mutable connection: option<Connection.t>,
  mutable goals: array<Goal.t>,
  mutable decoration: Decoration.t,
  mutable cursor: option<VSCode.Position.t>,
  editorIM: IM.t,
  promptIM: IM.t,
  mutable subscriptions: array<VSCode.Disposable.t>,
  // for self destruction
  onRemoveFromRegistry: Chan.t<unit>,
  // Agda Request queue
  mutable agdaRequestQueue: RequestQueue.t,
}
type state = t

// control the scope of command key-binding
module Context = {
  // most of the commands will work only after agda-mode:load
  let setLoaded = value => VSCode.Commands.setContext("agdaMode", value)->ignore
  // input method related key-bindings
  let setPrompt = value => VSCode.Commands.setContext("agdaModePrompting", value)->ignore
  let setIM = value => VSCode.Commands.setContext("agdaModeTyping", value)->ignore
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  View
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

module type View = {
  let reveal: state => unit
  // display stuff
  let display: (state, View.Header.t, View.Body.t) => Promise.t<unit>
  let displayEmacs: (state, View.Body.Emacs.t, View.Header.t, string) => Promise.t<unit>
  let displayOutOfGoalError: state => Promise.t<unit>
  let displayConnectionError: (state, Connection.Error.t) => Promise.t<unit>

  // Input Method
  let updateIM: (state, View.EventToView.InputMethod.t) => Promise.t<unit>
  let updatePromptIM: (state, string) => Promise.t<unit>
  // Prompt
  let prompt: (state, View.Header.t, View.Prompt.t, string => Promise.t<unit>) => Promise.t<unit>
  let interruptPrompt: state => Promise.t<unit>
}
module View: View = {
  let reveal = state => {
    state.view->ViewController.reveal
    Context.setLoaded(true)
  }

  // display stuff
  let display = (state, header, body) => ViewController.sendEvent(state.view, Display(header, body))
  let displayEmacs = (state, kind, header, body) =>
    ViewController.sendEvent(
      state.view,
      Display(header, Emacs(kind, View.Header.toString(header), body)),
    )
  let displayOutOfGoalError = state =>
    display(state, Error("Out of goal"), Plain("Please place the cursor in a goal"))

  let displayConnectionError = (state, error) => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), Plain(body))
  }

  // update the Input Method
  let updateIM = (state, event) => ViewController.sendEvent(state.view, InputMethod(event))
  let updatePromptIM = (state, content) =>
    ViewController.sendEvent(state.view, PromptIMUpdate(content))

  // Header + Prompt
  let prompt = (
    state,
    header,
    prompt,
    callbackOnPromptSuccess: string => Promise.t<unit>,
  ): Promise.t<unit> => {
    // focus on the panel before prompting
    Context.setPrompt(true)
    state.view->ViewController.focus

    // send request to view
    ViewController.sendRequest(state.view, Prompt(header, prompt), response =>
      switch response {
      | PromptSuccess(result) =>
        callbackOnPromptSuccess(result)->Promise.map(() => {
          // put the focus back to the editor after prompting
          Context.setPrompt(false)
          state.document->Editor.focus
        })
      | PromptInterrupted => Promise.resolved()
      }
    )
  }
  let interruptPrompt = state => ViewController.sendEvent(state.view, PromptInterrupt)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  Connection
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
module type Connection = {
  let disconnect: state => Promise.t<unit>
  let sendRequest: (state, Response.t => Promise.t<unit>, Request.t) => Promise.t<unit>
}
module Connection: Connection = {
  // connect if not connected yet
  let connect = state =>
    switch state.connection {
    | None =>
      Connection.make(Config.getAgdaPath, Config.setAgdaPath)->Promise.tapOk(conn =>
        state.connection = Some(conn)
      )
    | Some(connection) => Promise.resolved(Ok(connection))
    }
  let disconnect = state =>
    switch state.connection {
    | None => Promise.resolved()
    | Some(connection) =>
      Connection.destroy(connection)
      Promise.resolved()
    }

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
  let sendRequestAndHandleResponses = (
    state: state,
    handleResponse: Response.t => Promise.t<unit>,
    request: Request.t,
  ): Promise.t<unit> => {
    // Js.log("<<< " ++ Request.toString(request))

    // deferred responses are queued here
    let deferredLastResponses: array<(int, Response.t)> = []

    // this promise get resolved after all Responses has been received from Agda
    let (promise, stopListener) = Promise.pending()
    let handle = ref(None)
    let agdaResponseListener: result<Connection.response, Connection.Error.t> => unit = x =>
      switch x {
      | Error(error) => View.displayConnectionError(state, error)->ignore
      | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
        let body = Parser.Error.toString(error)
        View.display(state, Error("Internal Parse Error"), Plain(body))->ignore
      | Ok(Yield(Ok(NonLast(response)))) =>
        // Js.log(">>> " ++ Response.toString(response))
        Lock.runNonLast(handleResponse(response))->ignore
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
        ->Promise.flatMap(_ => Decoration.apply(state.decoration, state.editor))
        ->Promise.map(() => deferredLastResponses->Array.map(handleResponse))
        ->Promise.flatMap(Util.oneByOne)
        ->ignore
      }

    state
    ->connect
    ->Promise.mapOk(connection => {
      let version = connection.metadata.version
      let filepath = state.document->VSCode.TextDocument.fileName->Parser.filepath
      let libraryPath = Config.getLibraryPath()
      let highlightingMethod = Config.getHighlightingMethod()
      let backend = Config.getBackend()
      let encoded = Request.encode(
        state.document,
        version,
        filepath,
        backend,
        libraryPath,
        highlightingMethod,
        request,
      )
      Connection.send(encoded, connection)
      connection
    })
    ->Promise.flatMap(x =>
      switch x {
      | Ok(connection) =>
        handle := Some(connection.Connection.chan->Chan.on(agdaResponseListener))
        promise
      | Error(error) => View.displayConnectionError(state, error)->Promise.flatMap(() => promise)
      }
    )
    ->Promise.tap(() => handle.contents->Option.forEach(destroyListener => destroyListener()))
  }

  let sendRequest = (
    state: state,
    handleResponse: Response.t => Promise.t<unit>,
    request: Request.t,
  ): Promise.t<unit> =>
    state.agdaRequestQueue->RequestQueue.push(
      sendRequestAndHandleResponses(state, handleResponse),
      request,
    )
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  State
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// construction/destruction
let destroy = state => {
  state.view->ViewController.destroy
  state.onRemoveFromRegistry->Chan.emit()
  state.onRemoveFromRegistry->Chan.destroy
  state.goals->Array.forEach(Goal.destroy)
  state.decoration->Decoration.destroy
  Context.setLoaded(false)
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state->Connection.disconnect
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (extentionPath, chan, editor) => {
  Context.setLoaded(true)
  // view initialization
  let view = ViewController.make(extentionPath)

  {
    editor: editor,
    document: VSCode.TextEditor.document(editor),
    view: view,
    connection: None,
    goals: [],
    decoration: Decoration.make(),
    cursor: None,
    editorIM: IM.make(chan),
    promptIM: IM.make(chan),
    subscriptions: [],
    onRemoveFromRegistry: Chan.make(),
    agdaRequestQueue: RequestQueue.make(),
  }
}
