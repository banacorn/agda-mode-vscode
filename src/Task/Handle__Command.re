open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  // from Editor Command to Tasks
  let handle =
    fun
    | Load => [
        Task.WithState(
          state => Editor.save(state.editor)->Promise.map(_ => []),
        ),
        SendRequest(Load),
      ]
    | Quit => [Terminate]
    | NextGoal => [Goal(Next)]
    | PreviousGoal => [Goal(Previous)]
    | GoalType(normalization) => [
        Task.WithState(
          state => {
            Js.log(
              "GOALTYPE " ++ Command.Normalization.toString(normalization),
            );
            Promise.resolved([]);
          },
        ),
      ]
    | ViewResponse(response) => [ViewRes(response)];
};