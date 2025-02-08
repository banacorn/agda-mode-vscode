module type Panel = {
  let get: State.t => WebviewPanel.t
  // restore panel content after the corresponding editor was activated
  let restore: State.t => unit
  // display stuff
  let display: (State.t, View.Header.t, View.Body.t) => promise<unit>
  let displayInAppendMode: (State.t, View.Header.t, View.Body.t) => promise<unit>
  let displayOutOfGoalError: State.t => promise<unit>
  let displayConnectionError: (State.t, Connection.Error.t) => promise<unit>
  let displayStatus: (State.t, string) => promise<unit>
  let displayConnectionStatus: (State.t, Connection.Target.t) => promise<unit>
  // Input Method
  let updateIM: (State.t, View.EventToView.InputMethod.t) => promise<unit>
  let updatePromptIM: (State.t, string) => promise<unit>
  // Prompt
  let prompt: (State.t, View.Header.t, View.Prompt.t, string => promise<unit>) => promise<unit>
  let interruptPrompt: State.t => promise<unit>
  // Style
  let setFontSize: (State.t, string) => promise<unit>
}

module Panel: Panel = {
  let get = (state: State.t) => Singleton.Panel.make(state.extensionPath)

  let sendEvent = (state: State.t, event: View.EventToView.t) => {
    state.panelCache->State.ViewCache.cacheEvent(event)
    state->get->WebviewPanel.sendEvent(event)
  }
  let sendRequest = (state: State.t, request: View.Request.t, callback) => {
    state.panelCache->State.ViewCache.cacheRequest(request, callback)
    state->get->WebviewPanel.sendRequest(request, callback)
  }

  let restore = (state: State.t) => State.ViewCache.restore(state.panelCache, state->get)

  // display stuff
  let display = (state: State.t, header, body) => sendEvent(state, Display(header, body))
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
    | ALS(alsVersion, agdaVersion, Ok(ViaPipe(_, _, _, Connection__IPC.FromGitHub(_)))) =>
      displayStatus(state, "Prebuilt Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion)
    | ALS(alsVersion, agdaVersion, Ok(ViaPipe(_))) =>
      displayStatus(state, "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion)
    | ALS(_, _, Ok(ViaTCP(_))) => displayStatus(state, "ALS (TCP)")
    | ALS(alsVersion, agdaVersion, Error(_)) =>
      displayStatus(state, "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion)
    }

  // update the Input Method
  let updateIM = (state, event) => sendEvent(state, InputMethod(event))
  let updatePromptIM = (state, content) => sendEvent(state, PromptIMUpdate(content))

  let setFontSize = (state, fontSize) => sendEvent(state, ConfigurationChange(fontSize))

  // Header + Prompt
  let prompt = (
    state: State.t,
    header,
    prompt,
    callbackOnPromptSuccess: string => promise<unit>,
  ): promise<unit> => {
    // focus on the panel before prompting
    State.Context.setPrompt(true)

    // send request to view
    sendRequest(state, Prompt(header, prompt), async response =>
      switch response {
      | PromptSuccess(result) =>
        let _ = await callbackOnPromptSuccess(result)
        State.Context.setPrompt(false)
        // put the focus back to the editor after prompting
        Editor.focus(state.document)
        // prompt success, clear the cached prompt
        State.ViewCache.clearPrompt(state.panelCache)
      | PromptInterrupted =>
        State.Context.setPrompt(false)
        // put the focus back to the editor after prompting
        Editor.focus(state.document)
        // prompt interrupted, clear the cached prompt
        State.ViewCache.clearPrompt(state.panelCache)
        // restore the previously cached view
        State.ViewCache.restore(state.panelCache, state->get)
      }
    )
  }

  let interruptPrompt = async state => {
    await sendEvent(state, PromptInterrupt)
    State.Context.setPrompt(false)
    // put the focus back to the editor after prompting
    Editor.focus(state.document)
    // prompt interrupted, clear the cached prompt
    State.ViewCache.clearPrompt(state.panelCache)
    // restore the previously cached view
    State.ViewCache.restore(state.panelCache, state->get)
  }
}

module type DebugBuffer = {
  // lifecycle of the singleton
  let make: State.t => WebviewPanel.t
  let exists: unit => bool
  let destroy: unit => unit
  // all of the following methods will not have any effect if the singleton does not exist
  // let reveal: state => promise<unit>
  let display: array<(int, string)> => promise<unit>
  let displayInAppendMode: array<(int, string)> => promise<unit>
  let reveal: State.t => promise<unit>
  // restore panel content after the corresponding editor was activated
  let restore: State.t => promise<unit>
}

module DebugBuffer: DebugBuffer = {
  let make = (state: State.t) => Singleton.DebugBuffer.make(state.extensionPath)
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

  let reveal = (state: State.t) =>
    Singleton.DebugBuffer.get()->Option.mapOr(Promise.resolve(), debugBuffer => {
      WebviewPanel.reveal(debugBuffer)
      display(state.runningInfoLog)
    })

  let restore = (state: State.t) => display(state.runningInfoLog)
}
