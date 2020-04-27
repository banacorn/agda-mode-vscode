module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  type t =
    | WithState(State.t => Promise.t(list(t)))
    | DispatchCommand(Command.t)
    | SendRequest(Request.t);
};