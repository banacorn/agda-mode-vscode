open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);

  open! Task;
  // from Editor Command to Tasks
  let handle =
    fun
    | Load => [
        Task.WithState(
          state => Editor.save(state.editor)->Promise.map(_ => []),
        ),
        Goal(SaveCursor),
        SendRequest(Load),
        Goal(RestoreCursor),
      ]
    | Quit => [Terminate]
    | NextGoal => [Goal(Next)]
    | PreviousGoal => [Goal(Previous)]
    | Give => [
        Goal(
          GetPointedOr(
            (goal, content) =>
              switch (content) {
              | None =>
                query(
                  View.Request.Header.Plain(Command.toString(Command.Give)),
                  Some("expression to give:"),
                  None,
                  expr =>
                  [Goal(Modify(goal, _ => expr)), SendRequest(Give(goal))]
                )
              | Some(_) => [SendRequest(Give(goal))]
              },
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | Refine => [
        Goal(
          GetPointedOr(
            (goal, _) =>
              [
                Goal(SaveCursor),
                SendRequest(Refine(goal)),
                Goal(RestoreCursor),
              ],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | Auto => [
        Goal(
          GetPointedOr(
            (goal, _) => {[SendRequest(Auto(goal))]},
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | Case => {
        let header =
          View.Request.Header.Plain(Command.toString(Command.Case));
        let placeholder = Some("expression to case:");
        [
          Goal(
            GetPointedOr(
              goal =>
                (
                  fun
                  | None =>
                    query(header, placeholder, None, expr =>
                      [
                        Goal(Modify(goal, _ => expr)),
                        SendRequest(Case(goal)),
                      ]
                    )
                  | Some(_) => [SendRequest(Case(goal))]
                ),
              [Error(OutOfGoal)],
            ),
          ),
        ];
      }
    | InferType(normalization) => {
        let header =
          View.Request.Header.Plain(
            Command.toString(Command.InferType(normalization)),
          );
        let placeholder = Some("expression to infer:");
        [
          Goal(
            GetPointedOr(
              goal =>
                (
                  fun
                  | None =>
                    query(header, placeholder, None, expr =>
                      [SendRequest(InferType(normalization, expr, goal))]
                    )
                  | Some(expr) => [
                      SendRequest(InferType(normalization, expr, goal)),
                    ]
                ),
              query(header, placeholder, None, expr =>
                [SendRequest(InferTypeGlobal(normalization, expr))]
              ),
            ),
          ),
        ];
      }
    | GoalType(normalization) => [
        Goal(
          GetPointedOr(
            (goal, _) => {
              [Task.SendRequest(GoalType(normalization, goal))]
            },
            [Error(Error.OutOfGoal)],
          ),
        ),
      ]
    | ViewEvent(event) => [ViewEvent(event)]
    | Escape => [ViewReq(InterruptQuery, _ => [])];
};