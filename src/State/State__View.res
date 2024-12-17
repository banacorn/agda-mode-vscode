open State__Type

module type Panel = {
  let get: state => WebviewPanel.t
  // restore panel content after the corresponding editor was activated
  let restore: state => unit
  // display stuff
  let display: (state, View.Header.t, View.Body.t) => promise<unit>
  let displayInAppendMode: (state, View.Header.t, View.Body.t) => promise<unit>
  let displayOutOfGoalError: state => promise<unit>
  let displayConnectionError: (state, Connection.Error.t) => promise<unit>
  let displayStatus: (state, string) => promise<unit>
  let displayConnectionStatus: (state, Connection.Target.t) => promise<unit>
  // Input Method
  let updateIM: (state, View.EventToView.InputMethod.t) => promise<unit>
  let updatePromptIM: (state, string) => promise<unit>
  // Prompt
  let prompt: (state, View.Header.t, View.Prompt.t, string => promise<unit>) => promise<unit>
  let interruptPrompt: state => promise<unit>
  // Style
  let setFontSize: (state, string) => promise<unit>
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
    | Connection.Target.Agda(version, _) => displayStatus(state, "Agda v" ++ version)
    | ALS(version, ViaPipe(_, _, _, Connection__IPC.FromGitHub(_, release, _))) =>
      displayStatus(state, "ALS prebuilt " ++ release.tag_name ++ " (Agda v" ++ version ++ ")")
    | ALS(version, ViaPipe(_)) => displayStatus(state, "ALS v" ++ version)
    | ALS(_, ViaTCP(_)) => displayStatus(state, "ALS (TCP)")
    }

  // update the Input Method
  let updateIM = (state, event) => sendEvent(state, InputMethod(event))
  let updatePromptIM = (state, content) => sendEvent(state, PromptIMUpdate(content))

  let setFontSize = (state, fontSize) => sendEvent(state, ConfigurationChange(fontSize))

  // Header + Prompt
  let prompt = (state, header, prompt, callbackOnPromptSuccess: string => promise<unit>): promise<
    unit,
  > => {
    // focus on the panel before prompting
    Context.setPrompt(true)

    // send request to view
    sendRequest(state, Prompt(header, prompt), async response =>
      switch response {
      | PromptSuccess(result) =>
        let _ = await callbackOnPromptSuccess(result)
        Context.setPrompt(false)
        // put the focus back to the editor after prompting
        Editor.focus(state.document)
        // prompt success, clear the cached prompt
        ViewCache.clearPrompt(state.panelCache)
      | PromptInterrupted =>
        Context.setPrompt(false)
        // put the focus back to the editor after prompting
        Editor.focus(state.document)
        // prompt interrupted, clear the cached prompt
        ViewCache.clearPrompt(state.panelCache)
        // restore the previously cached view
        ViewCache.restore(state.panelCache, state->get)
      }
    )
  }

  let interruptPrompt = async state => {
    await sendEvent(state, PromptInterrupt)
    Context.setPrompt(false)
    // put the focus back to the editor after prompting
    Editor.focus(state.document)
    // prompt interrupted, clear the cached prompt
    ViewCache.clearPrompt(state.panelCache)
    // restore the previously cached view
    ViewCache.restore(state.panelCache, state->get)
  }
}

module type DebugBuffer = {
  // lifecycle of the singleton
  let make: state => WebviewPanel.t
  let exists: unit => bool
  let destroy: unit => unit
  // all of the following methods will not have any effect if the singleton does not exist
  // let reveal: state => promise<unit>
  let display: array<(int, string)> => promise<unit>
  let displayInAppendMode: array<(int, string)> => promise<unit>
  let reveal: state => promise<unit>
  // restore panel content after the corresponding editor was activated
  let restore: state => promise<unit>
}

module DebugBuffer: DebugBuffer = {
  let make = state => Singleton.DebugBuffer.make(state.extensionPath)
  let exists = () => Singleton.DebugBuffer.get()->Option.isSome
  let destroy = Singleton.DebugBuffer.destroy

  let sendEvent = (event: View.EventToView.t) =>
    Singleton.DebugBuffer.get()->Option.mapOr(Promise.resolve(), x =>
      x->WebviewPanel.sendEvent(event)
    )

  let display = msgs => {
    let header = View.Header.Plain("Agda Debug Buffer")
    let body = msgs->Array.map(((_, msg)) => {
      let body = RichText.string(msg)
      Item.Unlabeled(body, None, None)
    })
    sendEvent(Display(header, body))
  }
  let displayInAppendMode = msgs => {
    let header = View.Header.Plain("Agda Debug Buffer")
    let body = msgs->Array.map(((_, msg)) => {
      let body = RichText.string(msg)
      Item.Unlabeled(body, None, None)
    })
    sendEvent(Append(header, body))
  }

  let reveal = state =>
    Singleton.DebugBuffer.get()->Option.mapOr(Promise.resolve(), debugBuffer => {
      WebviewPanel.reveal(debugBuffer)
      display(state.runningInfoLog)
    })

  let restore = state => display(state.runningInfoLog)
}
