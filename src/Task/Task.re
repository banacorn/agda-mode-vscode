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
    | GetPointedOr(Goal.t => Promise.t(list(t)), list(t))
    | GetIndexedOr(int, Goal.t => Promise.t(list(t)), list(t))

  and t =
    //
    | Terminate
    // Connection
    | SendRequest(Request.t)
    // View
    | ViewReq(View.Request.t)
    | ViewRes(View.Response.t)
    // Misc
    | Error(Error.t)
    | Goal(goal)
    | WithState(State.t => Promise.t(list(t)))
    | Debug(string);

  // Smart constructors
  let display = (header, body) => ViewReq(Plain(Plain(header), body));
  let displayError = (header, body) => ViewReq(Plain(Error(header), body));
  let displayWarning = (header, body) =>
    ViewReq(Plain(Warning(header), body));
  let displaySuccess = (header, body) =>
    ViewReq(Plain(Warning(header), body));
};