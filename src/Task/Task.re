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
    | Decoration(StopAddingIndirectly) => "Decoration[StopAddingIndirectly]"
    | Decoration(RemoveAll) => "Decoration[RemoveAll]"
    | Decoration(Refresh) => "Decoration[Refresh]"
    | WithState(_) => "WithState"
    | WithStateP(_) => "WithStateP"
    | Debug(msg) => "Debug[" ++ msg ++ "]";

  // Smart constructors
  let display' = header =>
    fun
    | None => ViewEvent(Display(header, Nothing))
    | Some(message) => ViewEvent(Display(header, Plain(message)));
  let display = header => display'(Plain(header));
  //
  let displayHeaderOnly = header => ViewEvent(Display(header, Nothing));
  let displayEmacs = (kind, header, body) =>
    ViewEvent(
      Display(header, Emacs(kind, View.Header.toString(header), body)),
    );
  //
  let displayError = header => display'(Error(header));
  let displayWarning = header => display'(Warning(header));
  let displaySuccess = header => display'(Success(header));
  let timeStart = label => WithState(_ => Js.Console.timeStart(label));
  let timeEnd = label => WithState(_ => Js.Console.timeEnd(label));
  let query =
      (
        header,
        body,
        placeholder,
        value,
        callbackOnQuerySuccess: string => list(t),
      ) => [
    WithState(
      state => {
        // focus on the panel before inquiring
        Editor.setContext("agdaModeQuerying", true)->ignore;
        state.view->Editor.View.focus;
      },
    ),
    ViewRequest(
      Query(header, body, placeholder, value),
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
              },
            ),
          ],
        );
      },
    ),
  ];
};
