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
    | ViewReq(View.Request.t)
    | ViewRes(View.Response.t)
    | ViewListener(option(string) => Promise.t(list(t)))
    // Misc
    | Error(Error.t)
    | Goal(goal)
    | WithState(State.t => Promise.t(list(t)))
    | Debug(string);

  type request =
    | Agda(Request.t)
    | View(View.Request.t);

  let classify =
    fun
    | SendRequest(req) => Some(Agda(req))
    | ViewReq(req) => Some(View(req))
    | _ => None;

  // Smart constructors
  let display' = header =>
    fun
    | None => ViewReq(Plain(header, Nothing))
    | Some(message) => ViewReq(Plain(header, Plain(message)));
  let display = header => display'(Plain(header));
  let displayError = header => display'(Error(header));
  let displayWarning = header => display'(Warning(header));
  let displaySuccess = header => display'(Success(header));

  let inquire = (header, placeholder, value) =>
    ViewReq(Plain(header, Inquire(placeholder, value)));
};