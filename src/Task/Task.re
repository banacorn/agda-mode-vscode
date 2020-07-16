module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  module Request = Request.Impl(Editor);

  type goal =
    | Instantiate(array(int))
    | UpdateRange
    | Next
    | Previous
    | Modify(Goal.t, string => string)
    | SaveCursor
    | RestoreCursor
    | SetCursor(int)
    | RemoveBoundaryAndDestroy(Goal.t)
    | ReplaceWithLines(Goal.t, array(string))
    | ReplaceWithLambda(Goal.t, array(string))
    // for commands that have both the local (goal-specific) and global (top-level) version
    | LocalOrGlobal(Goal.t => list(t), list(t))
    | LocalOrGlobal2(
        (Goal.t, string) => list(t),
        Goal.t => list(t),
        list(t),
      )

  and t =
    | DispatchCommand(Command.t)
    //
    | SuicideByCop
    // Connection
    | SendRequest(Request.t)
    // View
    | SendEventToView(View.EventToView.t)
    | SendRequestToView(View.Request.t, View.Response.t => list(t))
    // Misc
    | Decoration(Decoration.action)
    | RefreshAllHighlightings
    | Error(Error.t)
    | Goal(goal)
    | WithState(State.t => unit)
    | WithStateP(State.t => Promise.t(list(t)))
    | Debug(string);

  let toString =
    fun
    | DispatchCommand(cmd) => "Command[" ++ Command.toString(cmd) ++ "]"
    | SuicideByCop => "SuicideByCop"
    | SendRequest(_req) => "SendRequest"
    | SendEventToView(_) => "SendEventToView"
    | SendRequestToView(_, _) => "SendRequestToView"
    | RefreshAllHighlightings => "RefreshAllHighlightings"
    | Error(_) => "Error"
    | Goal(Instantiate(_)) => "Goal[Instantiate]"
    | Goal(UpdateRange) => "Goal[UpdateRange]"
    | Goal(Next) => "Goal[Next]"
    | Goal(Previous) => "Goal[Previous]"
    | Goal(Modify(_, _)) => "Goal[Modify]"
    | Goal(SaveCursor) => "Goal[SaveCursor]"
    | Goal(RestoreCursor) => "Goal[RestoreCursor]"
    | Goal(SetCursor(_)) => "Goal[SetCursor]"
    | Goal(RemoveBoundaryAndDestroy(_)) => "Goal[RemoveBoundaryAndDestroy]"
    | Goal(ReplaceWithLines(_, _)) => "Goal[ReplaceWithLines]"
    | Goal(ReplaceWithLambda(_, _)) => "Goal[ReplaceWithLambda]"
    | Goal(LocalOrGlobal2(_, _, _)) => "Goal[LocalOrGlobal2]"
    | Goal(LocalOrGlobal(_, _)) => "Goal[LocalOrGlobal]"
    | Decoration(Add(_)) => "Decoration[Add]"
    | Decoration(RemoveAll) => "Decoration[RemoveAll]"
    | WithState(_) => "WithState"
    | WithStateP(_) => "WithStateP"
    | Debug(msg) => "Debug[" ++ msg ++ "]";

  // Smart constructors
  let display' = header =>
    fun
    | None => SendEventToView(Display(header, Nothing))
    | Some(message) => SendEventToView(Display(header, Plain(message)));
  let display = header => display'(Plain(header));
  let displayError = header => display'(Error(header));
  let displayWarning = header => display'(Warning(header));
  let displaySuccess = header => display'(Success(header));

  let query =
      (header, placeholder, value, callbackOnQuerySuccess: string => list(t)) => [
    WithState(
      state => {
        // focus on the panel before inquiring
        Editor.setContext("agdaModeQuerying", true)->ignore;
        state.view->Editor.View.focus;
        Js.log("focus in");
      },
    ),
    SendRequestToView(
      Query(header, placeholder, value),
      response => {
        let tasks =
          switch (response) {
          | View.Response.Success => []
          | QuerySuccess(result) => callbackOnQuerySuccess(result)
          | QueryInterrupted => [displayError("Query Cancelled", None)]
          };
        Belt.List.concat(
          tasks,
          [
            WithState(
              state => {
                // put the focus back to the editor after inquiring
                Editor.setContext("agdaModeQuerying", false)->ignore;
                state.editor->Editor.focus;
                Js.log("focus back");
              },
            ),
          ],
        );
      },
    ),
  ];
};
