open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  open! Task;
  // from Editor Command to Tasks
  let handle =
    fun
    | Load => [SendRequest(Load)]
    | Quit => [Terminate]
    | ViewResponse(response) => [ViewRes(response)];
};