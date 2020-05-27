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
    | Auto => [Debug("auto")]
    | GoalType(normalization) => [
        Goal(
          GetPointedOr(
            goal => {
              Promise.resolved([
                Task.SendRequest(GoalType(normalization, goal)),
              ])
            },
            [Error(Error.OutOfGoal)],
          ),
        ),
      ]
    | ViewResponse(response) => [ViewRes(response)];
};