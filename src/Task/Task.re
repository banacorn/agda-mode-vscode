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
    // | RestoreCursor(unit => Promise.t(list(t)))
    | SaveCursor
    | RestoreCursor
    | RemoveBoundaryAndDestroy(Goal.t)
    | GetPointedOr((Goal.t, option(string)) => list(t), list(t))
    | GetIndexedOr(int, (Goal.t, option(string)) => list(t), list(t))

  and t =
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

  let query = (header, placeholder, value, callbackOnQuerySuccess) => [
    // focus on the panel before inquiring
    WithState(
      state => {
        Editor.setContext("agdaModeQuerying", true)->ignore;
        state.view->Editor.View.focus;
        Promise.resolved([]);
      },
    ),
    ViewReq(
      Plain(header, Query(placeholder, value)),
      fun
      | View.Response.Success => []
      | QuerySuccess(result) => callbackOnQuerySuccess(result)
      | QueryInterrupted => [displayError("Query Cancelled", None)]
      | Event(Initialized) => []
      | Event(Destroyed) => [Terminate],
    ),
    // put the focus back to the editor after inquiring
    WithState(
      state => {
        Editor.setContext("agdaModeQuerying", false)->ignore;
        state.editor->Editor.focus;
        Promise.resolved([]);
      },
    ),
  ];
};