module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  module Request = Request.Impl(Editor);

  type t =
    | DispatchCommand(Command.t)
    // Agda
    | AgdaRequest(Request.t)
    // View
    | ViewEvent(View.EventToView.t)
    | ViewRequest(View.Request.t, View.Response.t => list(t))
    // Misc
    | Decoration(Decoration.action)
    | Error(Error.t)
    | Goal(Goal.action(t))
    | WithState(State.t => unit)
    | WithStateP(State.t => Promise.t(list(t)))
    | Destroy
    | Debug(string);

  let toString =
    fun
    | DispatchCommand(cmd) => "Command[" ++ Command.toString(cmd) ++ "]"
    | Destroy => "Destroy"
    | AgdaRequest(_req) => "AgdaRequest"
    | ViewEvent(_) => "ViewEvent"
    | ViewRequest(_, _) => "ViewRequest"
    | Error(_) => "Error"
    | Goal(Instantiate(_)) => "Goal[Instantiate]"
    | Goal(UpdateRange) => "Goal[UpdateRange]"
    | Goal(Next) => "Goal[Next]"
    | Goal(Previous) => "Goal[Previous]"
    | Goal(Modify(_, _)) => "Goal[Modify]"
    | Goal(SaveCursor) => "Goal[SaveCursor]"
    | Goal(RestoreCursor) => "Goal[RestoreCursor]"
    | Goal(SetCursor(_)) => "Goal[SetCursor]"
    | Goal(JumpToOffset(_)) => "Goal[JumpToOffset]"
    | Goal(RemoveBoundaryAndDestroy(_)) => "Goal[RemoveBoundaryAndDestroy]"
    | Goal(ReplaceWithLines(_, _)) => "Goal[ReplaceWithLines]"
    | Goal(ReplaceWithLambda(_, _)) => "Goal[ReplaceWithLambda]"
    | Goal(LocalOrGlobal2(_, _, _)) => "Goal[LocalOrGlobal2]"
    | Goal(LocalOrGlobal(_, _)) => "Goal[LocalOrGlobal]"
    | Decoration(AddDirectly(_)) => "Decoration[AddDirectly]"
    | Decoration(AddIndirectly(_)) => "Decoration[AddIndirectly]"
    | Decoration(Clear) => "Decoration[Clear]"
    | Decoration(Apply) => "Decoration[Apply]"
    | Decoration(Refresh) => "Decoration[Refresh]"
    | WithState(_) => "WithState"
    | WithStateP(_) => "WithStateP"
    | Debug(msg) => "Debug[" ++ msg ++ "]";

  // Smart constructors for controlling the view

  // Header + Body
  let display = (header, body) => ViewEvent(Display(header, body));
  let displayEmacs = (kind, header, body) =>
    ViewEvent(
      Display(header, Emacs(kind, View.Header.toString(header), body)),
    );

  // Header + Prompt
  let prompt =
      (
        header,
        body,
        placeholder,
        value,
        callbackOnPromptSuccess: string => list(t),
      ) => [
    WithState(
      state => {
        // focus on the panel before prompting
        Editor.setContext("agdaModePrompting", true)->ignore;
        state.view->Editor.View.focus;
      },
    ),
    ViewEvent(Display(Plain(header), Nothing)),
    ViewRequest(
      Prompt(header, body, placeholder, value),
      response => {
        let tasks =
          switch (response) {
          | View.Response.Success => []
          | PromptSuccess(result) => callbackOnPromptSuccess(result)
          | PromptInterrupted => [
              display(Error("Prompt Cancelled"), Nothing),
            ]
          };
        Belt.List.concat(
          tasks,
          [
            WithState(
              state => {
                // put the focus back to the editor after prompting
                Editor.setContext("agdaModePrompting", false)->ignore;
                state.editor->Editor.focus;
              },
            ),
          ],
        );
      },
    ),
  ];
};
