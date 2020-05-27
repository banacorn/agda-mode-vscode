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
        Goal(
          GetPointedOr(
            goal => {
              Promise.resolved([
                Task.SendRequest(GoalType(normalization, goal)),
              ])
            },
            [
              ViewReq(
                Plain(
                  Error("Out of goal"),
                  Some("Please place the cursor in a goal"),
                ),
              ),
            ],
          ),
        ),
      ]
    | ViewResponse(response) => [ViewRes(response)];
};