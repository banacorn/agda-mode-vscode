// Smart constructors for controlling the view

let viewEvent = (state, event) => state->State.sendEventToView(event)->Promise.map(_ => ())

// Header + Body
let display = (state, header, body) => viewEvent(state, Display(header, body))
let displayEmacs = (state, kind, header, body) =>
  viewEvent(state, Display(header, Emacs(kind, View.Header.toString(header), body)))

let displayOutOfGoalError = state =>
  display(state, Error("Out of goal"), Plain("Please place the cursor in a goal"))

let sendViewRequest = (
  state: State.t,
  request,
  callback: View.Response.t => Promise.t<unit>,
): Promise.t<unit> => {
  state->State.sendRequestToView(request)->Promise.flatMap(x =>
    switch x {
    | None => Promise.resolved()
    | Some(response) => callback(response)
    }
  )
}

// Header + Prompt
let prompt = (
  state: State.t,
  header,
  prompt,
  callbackOnPromptSuccess: string => Promise.t<unit>,
): Promise.t<unit> => {
  // focus on the panel before prompting
  VSCode.Commands.setContext("agdaModePrompting", true)->ignore
  state.view->View__Controller.focus

  // send request to view
  sendViewRequest(state, Prompt(header, prompt), response =>
    switch response {
    | PromptSuccess(result) => callbackOnPromptSuccess(result)->Promise.map(() => {
        // put the focus back to the editor after prompting
        VSCode.Commands.setContext("agdaModePrompting", false)->ignore
        state.document->Editor.focus
      })
    | PromptInterrupted => Promise.resolved()
    }
  )
}
