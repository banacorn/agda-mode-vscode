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

// cache the stuff previously displayed in the view, so that we can restore them later
module ViewCache = {
  type t = {
    mutable display: option<(View.Header.t, View.Body.t)>,
    mutable prompt: option<(View.Header.t, View.Prompt.t, View.Response.t => Promise.t<unit>)>,
  }

  let make = () => {
    display: None,
    prompt: None,
  }

  let cacheEvent = (self, event: View.EventToView.t) =>
    switch event {
    // cache the event only when it's a "Display"
    | Display(header, body) => self.display = Some((header, body))
    | _ => ()
    }

  let cacheRequest = (self, event: View.Request.t, callback) =>
    switch event {
    | Prompt(header, prompt) => self.prompt = Some(header, prompt, callback)
    }

  let clearPrompt = self => self.prompt = None

  // if there's no Prompt, then restore Display instead
  let restore = (self, view) =>
    switch self.prompt {
    | Some((header, prompt, callback)) =>
      view->ViewController.sendRequest(Prompt(header, prompt), callback)->ignore
    | None =>
      self.display->Option.forEach(((header, body)) =>
        view->ViewController.sendEvent(Display(header, body))->ignore
      )
    }
}

type t = {
  mutable editor: VSCode.TextEditor.t,
  mutable document: VSCode.TextDocument.t,
  view: ViewController.t,
  viewCache: ViewCache.t,
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
  // let displayEmacs: (state, View.Body.Emacs.t, View.Header.t, string) => Promise.t<unit>
  let displayOutOfGoalError: state => Promise.t<unit>
  let displayConnectionError: (state, Connection.Error.t) => Promise.t<unit>
  let displayConnectionStatus: (state, Connection.status) => Promise.t<unit>
  // Input Method
  let updateIM: (state, View.EventToView.InputMethod.t) => Promise.t<unit>
  let updatePromptIM: (state, string) => Promise.t<unit>
  // Prompt
  let prompt: (state, View.Header.t, View.Prompt.t, string => Promise.t<unit>) => Promise.t<unit>
  let interruptPrompt: state => Promise.t<unit>
}

module View: View = {
  let sendEvent = (state, event: View.EventToView.t) => {
    state.viewCache->ViewCache.cacheEvent(event)
    state.view->ViewController.sendEvent(event)
  }
  let sendRequest = (state, request: View.Request.t, callback) => {
    state.viewCache->ViewCache.cacheRequest(request, callback)
    state.view->ViewController.sendRequest(request, callback)
  }

  let activate = state => ViewCache.restore(state.viewCache, state.view)

  let reveal = state => {
    state.view->ViewController.reveal
  }

  // display stuff
  let display = (state, header, body) => sendEvent(state, Display(header, body))
  let displayOutOfGoalError = state =>
    display(
      state,
      Error("Out of goal"),
      [Item.plainText("Please place the cursor in a goal")],
    )

  let displayConnectionError = (state, error) => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), [Item.plainText(body)])
  }

  // display connection status
  let displayConnectionStatus = (state, status) =>
    switch status {
    | Connection.Emacs(_) => sendEvent(state, SetStatus("Emacs"))
    | LSP(_, ViaStdIO(_, _)) => sendEvent(state, SetStatus("LSP"))
    | LSP(_, ViaTCP(_)) => sendEvent(state, SetStatus("LSP (TCP)"))
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
          Context.setPrompt(false)
          // put the focus back to the editor after prompting
          Editor.focus(state.document)
          // prompt success, clear the cached prompt
          ViewCache.clearPrompt(state.viewCache)
        })
      | PromptInterrupted =>
        Context.setPrompt(false)
        // put the focus back to the editor after prompting
        Editor.focus(state.document)
        // prompt interrupted, clear the cached prompt
        ViewCache.clearPrompt(state.viewCache)
        // restore the previously cached view
        ViewCache.restore(state.viewCache, state.view)
        Promise.resolved()
      }
    )
  }

  let interruptPrompt = state =>
    sendEvent(state, PromptInterrupt)->Promise.tap(() => {
      Context.setPrompt(false)
      // put the focus back to the editor after prompting
      Editor.focus(state.document)
      // prompt interrupted, clear the cached prompt
      ViewCache.clearPrompt(state.viewCache)
      // restore the previously cached view
      ViewCache.restore(state.viewCache, state.view)
    })
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  State
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let sendRequest = (
  state: state,
  handleResponse: Response.t => Promise.t<unit>,
  request: Request.t,
): Promise.t<unit> => {
  let sendRequestAndHandleResponses = (state, request, handler) => {
    let onResponse = result =>
      switch result {
      | Error(error) => View.displayConnectionError(state, error)
      | Ok(response) => handler(response)
      }
    Connection.sendRequest(
      Config.Connection.useAgdaLanguageServer(),
      state.document,
      request,
      onResponse,
    )->Promise.flatMap(result =>
      switch result {
      | Error(error) => View.displayConnectionError(state, error)
      | Ok(status) => View.displayConnectionStatus(state, status)
      }
    )
  }

  state.agdaRequestQueue->RequestQueue.push(
    request => sendRequestAndHandleResponses(state, request, handleResponse),
    request,
  )
}

// construction/destruction
let destroy = (state, alsoRemoveFromRegistry) => {
  if alsoRemoveFromRegistry {
    state.onRemoveFromRegistry->Chan.emit()
  }
  state.onRemoveFromRegistry->Chan.destroy
  state.goals->Array.forEach(Goal.destroy)
  state.decoration->Decoration.destroy
  state.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  Connection.stop()
  // TODO: delete files in `.indirectHighlightingFileNames`
}

let make = (chan, editor, view) => {
  editor: editor,
  document: VSCode.TextEditor.document(editor),
  view: view,
  viewCache: ViewCache.make(),
  goals: [],
  decoration: Decoration.make(),
  cursor: None,
  editorIM: IM.make(chan),
  promptIM: IM.make(chan),
  subscriptions: [],
  onRemoveFromRegistry: Chan.make(),
  agdaRequestQueue: RequestQueue.make(),
}
