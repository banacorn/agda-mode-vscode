module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module Request = Request.Impl(Editor);
  // module ViewHandler = Handle__View.Impl(Editor);

  type goal =
    | Instantiate(array(int))
    | UpdateRange
    | Next
    | Previous
    | Modify(Goal.t, string => string)
    | SaveCursor
    | RestoreCursor
    | RemoveBoundaryAndDestroy(Goal.t)
    | ReplaceWithLines(Goal.t, array(string))
    | ReplaceWithLambda(Goal.t, array(string))
    | GetPointedOr((Goal.t, option(string)) => list(t), list(t))
    | GetIndexedOr(int, (Goal.t, option(string)) => list(t), list(t))

  and t =
    | DispatchCommand(Command.t)
    //
    | Terminate
    // Connection
    | SendRequest(Request.t)
    // View
    | ViewReq(View.Request.t, View.Response.t => list(t))
    | ViewEvent(View.Event.t)
    // Misc
    | Error(Error.t)
    | Goal(goal)
    | WithState(State.t => Promise.t(list(t)))
    | Debug(string);

  let toString =
    fun
    | DispatchCommand(cmd) => "Command[" ++ Command.toString(cmd) ++ "]"
    | Terminate => "Terminate"
    | SendRequest(_req) => "SendRequest"
    | ViewReq(_, _) => "ViewReq"
    | ViewEvent(_) => "ViewEvent"
    | Error(_) => "Error"
    | Goal(Instantiate(_)) => "Goal[Instantiate]"
    | Goal(UpdateRange) => "Goal[UpdateRange]"
    | Goal(Next) => "Goal[Next]"
    | Goal(Previous) => "Goal[Previous]"
    | Goal(Modify(_, _)) => "Goal[Modify]"
    | Goal(SaveCursor) => "Goal[SaveCursor]"
    | Goal(RestoreCursor) => "Goal[RestoreCursor]"
    | Goal(RemoveBoundaryAndDestroy(_)) => "Goal[RemoveBoundaryAndDestroy]"
    | Goal(ReplaceWithLines(_, _)) => "Goal[ReplaceWithLines]"
    | Goal(ReplaceWithLambda(_, _)) => "Goal[ReplaceWithLambda]"
    | Goal(GetPointedOr(_, _)) => "Goal[GetPointedOr]"
    | Goal(GetIndexedOr(_)) => "Goal[GetIndexedOr]"
    | WithState(_) => "WithState"
    | Debug(msg) => "Debug[" ++ msg ++ "]";

  type request =
    | Agda(Request.t)
    | View(View.Request.t, View.Response.t => list(t));

  let classify =
    fun
    | SendRequest(req) => Some(Agda(req))
    | ViewReq(req, callback) => Some(View(req, callback))
    | _ => None;

  // Smart constructors
  let display' = header =>
    fun
    | None => ViewReq(Plain(header, Nothing), _ => [])
    | Some(message) => ViewReq(Plain(header, Plain(message)), _ => []);
  let display = header => display'(Plain(header));
  let displayError = header => display'(Error(header));
  let displayWarning = header => display'(Warning(header));
  let displaySuccess = header => display'(Success(header));

  let afterQuery = callbackOnQuerySuccess =>
    fun
    | View.Response.Success => []
    | QuerySuccess(result) => callbackOnQuerySuccess(result)
    | QueryInterrupted => [displayError("Query Cancelled", None)]
    | Event(Initialized) => []
    | Event(Destroyed) => [Terminate];

  let query = (header, placeholder, value, callbackOnQuerySuccess) => [
    WithState(
      state => {
        // focus on the panel before inquiring
        Editor.setContext("agdaModeQuerying", true)->ignore;
        state.view->Editor.View.focus;
        Promise.resolved([]);
      },
    ),
    ViewReq(
      Plain(header, Query(placeholder, value)),
      response => {
        let tasks =
          switch (response) {
          | View.Response.Success => []
          | QuerySuccess(result) => callbackOnQuerySuccess(result)
          | QueryInterrupted => [displayError("Query Cancelled", None)]
          | Event(Initialized) => []
          | Event(Destroyed) => [Terminate]
          };
        Belt.List.concat(
          tasks,
          [
            WithState(
              state => {
                // put the focus back to the editor after inquiring
                Editor.setContext("agdaModeQuerying", false)->ignore;
                state.editor->Editor.focus;
                Promise.resolved([]);
              },
            ),
          ],
        );
      },
    ),
  ];
};