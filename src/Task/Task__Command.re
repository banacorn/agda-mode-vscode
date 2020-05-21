open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open! Task;
  // from Editor Command to Tasks
  let dispatch =
    fun
    | Load => [Connect, SendRequest(Load)]
    | Quit => [Terminate];
};