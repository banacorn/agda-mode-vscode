open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  // from Editor Command to Tasks
  let handle =
    fun
    | Load => [Task.SendRequest(Load)]
    | Quit => [Terminate]
    | ViewResponse(response) => [ViewRes(response)];
};