module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);

  type goal =
    | Instantiate(array(int))
    | Next
    | Previous;

  type t =
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