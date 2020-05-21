module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  type t =
    // | Initialize
    | Terminate
    | WithState(State.t => Promise.t(list(t)))
    | DispatchCommand(Command.t)
    | Connect
    | SendRequest(Request.t)
    | ViewReq(View.Request.t)
    | ViewRes(View.Response.t);
};