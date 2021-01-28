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

type viewCache =
  Event(View.EventToView.t) | Request(View.Request.t, View.Response.t => Promise.t<unit>)

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  view: ViewController.t,
  mutable connection: option<Connection.t>,
  mutable viewCache: option<viewCache>,
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
  let activate: state => unit
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
  let sendEvent = (state, event) => {
    state.viewCache = Some(Event(event))
    state.view->ViewController.sendEvent(event)
  }
  let sendRequest = (state, request, callback) => {
    state.viewCache = Some(Request(request, callback))
    state.view->ViewController.sendRequest(request, callback)
  }

  let activate = state =>
    state.viewCache->Option.forEach(content =>
      switch content {
      | Event(event) => state.view->ViewController.sendEvent(event)->ignore
      | Request(request, callback) =>
        state.view->ViewController.sendRequest(request, callback)->ignore
      }
    )

  let reveal = state => {
    state.view->ViewController.reveal
    Context.setLoaded(true)
  }

  // display stuff
  let display = (state, header, body) => sendEvent(state, Display(header, body))
  let displayEmacs = (state, kind, header, body) =>
    sendEvent(state, Display(header, Emacs(kind, View.Header.toString(header), body)))
  let displayOutOfGoalError = state =>
    display(state, Error("Out of goal"), Plain("Please place the cursor in a goal"))

  let displayConnectionError = (state, error) => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), Plain(body))
  }

  // update the Input Method
  let updateIM = (state, event) => sendEvent(state, InputMethod(event))
  let updatePromptIM = (state, content) => sendEvent(state, PromptIMUpdate(content))

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
    sendRequest(state, Prompt(header, prompt), response =>
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
  let interruptPrompt = state => sendEvent(state, PromptInterrupt)
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  Connection
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
module type Connection = {
  let connect: state => Promise.t<result<Connection.t, Connection.Error.t>>
  let disconnect: state => Promise.t<unit>
  let sendRequest: (state, Response.t => Promise.t<unit>, Request.t) => Promise.t<unit>
}
module Connection: Connection = {
  let connect = state =>
    switch state.connection {
    | None => Connection.make()->Promise.tapOk(conn => state.connection = Some(conn))
    | Some(connection) => Promise.resolved(Ok(connection))
    }
  let disconnect = state =>
    switch state.connection {
    | None => Promise.resolved()
    | Some(connection) =>
      state.connection = None
      Connection.destroy(connection)
    }

  // helper function of `executeTask`
  let sendRequestAndHandleResponses = (
    state: state,
    handleResponse: Response.t => Promise.t<unit>,
    request: Request.t,
  ): Promise.t<unit> => {
    // this promise get resolved after all Responses has been received from Agda
    let handle = ref(None)

    let handleResult = result =>
      switch result {
      | Error(error) =>
        let (head, body) = Connection.Error.toString(error)
        View.display(state, Error(head), Plain(body))
      | Ok(response) => handleResponse(response)
      }

    state
    ->connect
    ->Promise.flatMap(x =>
      switch x {
      | Ok(connection) =>
        let promise = Connection.onResponse(connection, handleResult)
        Connection.sendRequest(connection, state.document, request)
        promise
      | Error(error) => View.displayConnectionError(state, error)
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
let destroy = (state, alsoRemoveFromRegistry) => {
  if alsoRemoveFromRegistry {
    state.onRemoveFromRegistry->Chan.emit()
  }
  state.onRemoveFromRegistry->Chan.destroy
  state.goals->Array.forEach(Goal.destroy)
  state.decoration->Decoration.destroy
  Context.setLoaded(false)
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  state->Connection.disconnect
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (chan, editor, view) => {
  Context.setLoaded(true)

  {
    editor: editor,
    document: VSCode.TextEditor.document(editor),
    connection: None,
    view: view,
    viewCache: None,
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
