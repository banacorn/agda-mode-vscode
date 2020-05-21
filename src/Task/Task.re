module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  type t =
    //
    | Terminate
    | DispatchCommand(Command.t)
    // Connection
    | SendRequest(Request.t)
    // View
    | ViewReq(View.Request.t)
    | ViewRes(View.Response.t)
    // Misc
    | WithState(State.t => Promise.t(list(t)));
};