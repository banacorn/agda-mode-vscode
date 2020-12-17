type rec t =
  | DispatchCommand(Command.t)
  // Agda
  | AgdaRequest(Request.t)
  // View
  | ViewRequest(View.Request.t, View.Response.t => list<t>)
  // Misc
  | WithState(State.t => unit)
  | WithStateP(State.t => Promise.t<list<t>>)
  | Destroy
  | Debug(string)

let toString = x =>
  switch x {
  | DispatchCommand(cmd) => "Command[" ++ (Command.toString(cmd) ++ "]")
  | Destroy => "Destroy"
  | AgdaRequest(_req) => "AgdaRequest"
  | ViewRequest(_, _) => "ViewRequest"
  | WithState(_) => "WithState"
  | WithStateP(_) => "WithStateP"
  | Debug(msg) => "Debug[" ++ (msg ++ "]")
  }

// Smart constructors for controlling the view

let viewEvent = event => WithStateP(
  state => state->State.sendEventToView(event)->Promise.map(_ => list{}),
)

// Header + Body
let display = (header, body) => viewEvent(Display(header, body))
let displayEmacs = (kind, header, body) =>
  viewEvent(Display(header, Emacs(kind, View.Header.toString(header), body)))

let displayOutOfGoalError = display(
  Error("Out of goal"),
  Plain("Please place the cursor in a goal"),
)

// Header + Prompt
let prompt = (header, prompt, callbackOnPromptSuccess: string => list<t>) => list{
  WithState(
    state => {
      // focus on the panel before prompting
      VSCode.Commands.setContext("agdaModePrompting", true)->ignore
      state.view->View__Controller.focus
    },
  ),
  ViewRequest(
    Prompt(header, prompt),
    response => {
      let tasks = switch response {
      | PromptSuccess(result) => callbackOnPromptSuccess(result)
      | PromptInterrupted => list{}
      }
      Belt.List.concat(
        tasks,
        list{
          WithState(
            state => {
              // put the focus back to the editor after prompting
              VSCode.Commands.setContext("agdaModePrompting", false)->ignore
              state.editor->VSCode.TextEditor.document->Editor.focus
            },
          ),
        },
      )
    },
  ),
}
