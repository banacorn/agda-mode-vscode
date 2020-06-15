open Belt;

module Impl = (Editor: Sig.Editor) => {
  module ErrorHandler = Handle__Error.Impl(Editor);
  // module ViewHandler = Handle__View.Impl(Editor);

  module GoalHandler = Handle__Goal.Impl(Editor);
  module ResponseHandler = Handle__Response.Impl(Editor);
  module Task = Task.Impl(Editor);
  // module State = State.Impl(Editor);
  // module Request = Request.Impl(Editor);
  // type t = Runner.t(Command.t);
  open! Task;

  type status =
    | Available
    | Occupied;

  type t = {
    mutable agda: status,
    mutable view: status,
    mutable command: status,
  };
};