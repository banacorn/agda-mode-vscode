open State__Type

module type Module = {
  // restore panel content after the corresponding editor was activated
  let restore: state => unit
  // display stuff
  let display: (state, View.Header.t, View.Body.t) => Promise.t<unit>
  let displayInAppendMode: (state, View.Header.t, View.Body.t) => Promise.t<unit>
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

module Module: Module = {
  let sendEvent = (state, event: View.EventToView.t) => {
    state.panelCache->ViewCache.cacheEvent(event)
    state.panel->WebviewPanel.sendEvent(event)
  }
  let sendRequest = (state, request: View.Request.t, callback) => {
    state.panelCache->ViewCache.cacheRequest(request, callback)
    state.panel->WebviewPanel.sendRequest(request, callback)
  }

  let restore = state => ViewCache.restore(state.panelCache, state.panel)

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
    state.panel->WebviewPanel.focus

    // send request to view
    sendRequest(state, Prompt(header, prompt), response =>
      switch response {
      | PromptSuccess(result) =>
        callbackOnPromptSuccess(result)->Promise.map(() => {
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
        ViewCache.restore(state.panelCache, state.panel)
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
      ViewCache.restore(state.panelCache, state.panel)
    })
}

include Module