module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module Request = Request.Impl(Editor);

  type goal =
    | Instantiate(array(int))
    | UpdateRange
    | Next
    | Previous
    | Modify(Goal.t, string => string)
    | RemoveBoundaryAndDestroy(Goal.t)
    | GetPointedOr((Goal.t, option(string)) => list(t), list(t))
    | GetIndexedOr(int, (Goal.t, option(string)) => list(t), list(t))

  and t =
    //
    | Terminate
    // Connection
    | SendRequest(Request.t)
    // View
    | ViewReq(View.Request.t, View.Response.t => Promise.t(list(t)))
    // | ViewRes(View.Response.t)
    | ViewEvent(View.Event.t)
    // Misc
    | Error(Error.t)
    | Goal(goal)
    | WithState(State.t => Promise.t(list(t)))
    | Debug(string);

  type request =
    | Agda(Request.t)
    | View(View.Request.t, View.Response.t => Promise.t(list(t)));

  let classify =
    fun
    | SendRequest(req) => Some(Agda(req))
    | ViewReq(req, callback) => Some(View(req, callback))
    | _ => None;

  // Smart constructors
  let display' = header =>
    fun
    | None => ViewReq(Plain(header, Nothing), _ => Promise.resolved([]))
    | Some(message) =>
      ViewReq(Plain(header, Plain(message)), _ => Promise.resolved([]));
  let display = header => display'(Plain(header));
  let displayError = header => display'(Error(header));
  let displayWarning = header => display'(Warning(header));
  let displaySuccess = header => display'(Success(header));

  let inquire = (header, placeholder, value, callback) =>
    ViewReq(Plain(header, Inquire(placeholder, value)), callback);

  let focus = () => ViewReq(Focus, _ => Promise.resolved([]));
};