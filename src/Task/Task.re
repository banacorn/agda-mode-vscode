module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);
  module Request = Request.Impl(Editor);

  type goal =
    | Instantiate(array(int))
    | Next
    | Previous
    | GetPointedOr(Goal.t => Promise.t(list(t)), list(t))
  and t =
    //
    | Terminate
    // Connection
    | SendRequest(Request.t)
    // View
    | ViewReq(View.Request.t)
    | ViewRes(View.Response.t)
    // Misc
    | Goal(goal)
    | WithState(State.t => Promise.t(list(t)));
};