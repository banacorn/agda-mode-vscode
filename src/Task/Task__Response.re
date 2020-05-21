// open Response;

// from Agda Response to Tasks
module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open! Task;
  let handle =
    fun
    | _ => [];
};