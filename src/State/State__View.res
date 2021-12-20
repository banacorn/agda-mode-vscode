open State__Type
open Belt

module type Panel = {
  let get: state => WebviewPanel.t
  // restore panel content after the corresponding editor was activated
  let restore: state => unit
  // display stuff
  let display: (state, View.Header.t, View.Body.t) => Promise.t<unit>
  let displayInAppendMode: (state, View.Header.t, View.Body.t) => Promise.t<unit>
  let displayOutOfGoalError: state => Promise.t<unit>
  let displayConnectionError: (state, Connection.Error.t) => Promise.t<unit>
  let displayStatus: (state, string) => Promise.t<unit>
  let displayConnectionStatus: (state, Connection.status) => Promise.t<unit>
  // Input Method
  let updateIM: (state, View.EventToView.InputMethod.t) => Promise.t<unit>
  let updatePromptIM: (state, string) => Promise.t<unit>
  // Prompt
  let prompt: (
    state,
    View.Header.t,
    View.Prompt.t,
    string => Promise.t<result<unit, Connection.Error.t>>,
  ) => Promise.t<unit>
  let interruptPrompt: state => Promise.t<unit>
}

module Panel: Panel = {
  let get = state => Singleton.Panel.make(state.extensionPath)

  let sendEvent = (state, event: View.EventToView.t) => {
    state.panelCache->ViewCache.cacheEvent(event)
    state->get->WebviewPanel.sendEvent(event)
  }
  let sendRequest = (state, request: View.Request.t, callback) => {
    state.panelCache->ViewCache.cacheRequest(request, callback)
    state->get->WebviewPanel.sendRequest(request, callback)
  }

  let restore = state => ViewCache.restore(state.panelCache, state->get)

  // display stuff
  let display = (state, header, body) => sendEvent(state, Display(header, body))
  let displayInAppendMode = (state, header, body) => sendEvent(state, Append(header, body))

  let displayOutOfGoalError = state =>
    display(state, Error("Out of goal"), [Item.plainText("Please place the cursor in a goal")])

  let displayConnectionError = (state, error) => {
    let (header, body) = Connection.Error.toString(error)
    display(state, Error("Connection Error: " ++ header), [Item.plainText(body)])
  }

  // display connection status
  let displayStatus = (state, string) => sendEvent(state, SetStatus(string))
  let displayConnectionStatus = (state, status) =>
    switch status {
    | Connection.Emacs(version, _) => displayStatus(state, "Emacs v" ++ version)
    | LSP(version, ViaCommand(_, _, _, LanguageServerMule.Method.FromGitHub(_, release, _))) =>
      displayStatus(state, "ALS prebuilt " ++ release.tagName ++ " (Agda v" ++ version ++ ")")
    | LSP(version, ViaCommand(_)) => displayStatus(state, "ALS v" ++ version)
    | LSP(_, ViaTCP(_)) => displayStatus(state, "ALS (TCP)")
    }

  // update the Input Method
  let updateIM = (state, event) => sendEvent(state, InputMethod(event))
  let updatePromptIM = (state, content) => sendEvent(state, PromptIMUpdate(content))

  // Header + Prompt
  let prompt = (
    state,
    header,
    prompt,
    callbackOnPromptSuccess: string => Promise.t<result<unit, Connection.Error.t>>,
  ): Promise.t<unit> => {
    // focus on the panel before prompting
    Context.setPrompt(true)

    // send request to view
    sendRequest(state, Prompt(header, prompt), response =>
      switch response {
      | PromptSuccess(result) =>
        callbackOnPromptSuccess(result)->Promise.map(_ => {
          Context.setPrompt(false)
          // put the focus back to the editor after prompting
          Editor.focus(state.document)
          // prompt success, clear the cached prompt
          ViewCache.clearPrompt(state.panelCache)
        })
      | PromptInterrupted =>
        Context.setPrompt(false)
        // put the focus back to the editor after prompting
        Editor.focus(state.document)
        // prompt interrupted, clear the cached prompt
        ViewCache.clearPrompt(state.panelCache)
        // restore the previously cached view
        ViewCache.restore(state.panelCache, state->get)
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
      ViewCache.clearPrompt(state.panelCache)
      // restore the previously cached view
      ViewCache.restore(state.panelCache, state->get)
    })
}

module type DebugBuffer = {
  // lifecycle of the singleton
  let make: state => WebviewPanel.t
  let exists: unit => bool
  let destroy: unit => unit
  // all of the following methods will not have any effect if the singleton does not exist
  // let reveal: state => Promise.t<unit>
  let display: array<(int, string)> => Promise.t<unit>
  let displayInAppendMode: array<(int, string)> => Promise.t<unit>
  let reveal: state => Promise.t<unit>
  // restore panel content after the corresponding editor was activated
  let restore: state => Promise.t<unit>
}

module DebugBuffer: DebugBuffer = {
  let make = state => Singleton.DebugBuffer.make(state.extensionPath)
  let exists = () => Singleton.DebugBuffer.get()->Option.isSome
  let destroy = Singleton.DebugBuffer.destroy

  let sendEvent = (event: View.EventToView.t) =>
    Singleton.DebugBuffer.get()->Option.mapWithDefault(Promise.resolved(), x =>
      x->WebviewPanel.sendEvent(event)
    )

  let display = msgs => {
    let header = View.Header.Plain("Agda Debug Buffer")
    let body = msgs->Array.map(((verbosity, msg)) => {
      let verbosity = string_of_int(verbosity)
      let style = ""
      let body = RichText.string(msg)
      Item.Labeled(verbosity, style, body, None, None)
    })
    sendEvent(Display(header, body))
  }
  let displayInAppendMode = msgs => {
    let header = View.Header.Plain("Agda Debug Buffer")
    let body = msgs->Array.map(((verbosity, msg)) => {
      let verbosity = string_of_int(verbosity)
      let style = ""
      let body = RichText.string(msg)
      Item.Labeled(verbosity, style, body, None, None)
    })
    sendEvent(Append(header, body))
  }

  let reveal = state =>
    Singleton.DebugBuffer.get()->Option.mapWithDefault(Promise.resolved(), debugBuffer => {
      WebviewPanel.reveal(debugBuffer)
      display(state.runningInfoLog)
    })

  let restore = state => display(state.runningInfoLog)
}
