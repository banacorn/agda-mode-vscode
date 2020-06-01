open Command;
open Belt;

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
        SendRequest(Load),
      ]
    | Quit => [Terminate]
    | NextGoal => [Goal(Next)]
    | PreviousGoal => [Goal(Previous)]
    | Auto => [
        Goal(
          GetPointedOr(
            (goal, _) => {[Task.SendRequest(Auto(goal))]},
            [Error(Error.OutOfGoal)],
          ),
        ),
      ]
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
                    inquire(
                      header,
                      placeholder,
                      None,
                      fun
                      | View.Response.QuerySuccess(expr) => {
                          Promise.resolved([
                            SendRequest(
                              InferType(normalization, expr, goal),
                            ),
                          ]);
                        }
                      | _ => Promise.resolved([]),
                    )
                  | Some(expr) => [
                      SendRequest(InferType(normalization, expr, goal)),
                    ]
                ),
              inquire(
                header,
                placeholder,
                None,
                fun
                | View.Response.QuerySuccess(expr) => {
                    Promise.resolved([
                      SendRequest(InferTypeGlobal(normalization, expr)),
                    ]);
                  }
                | _ => Promise.resolved([]),
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
    | ViewEvent(event) => [ViewEvent(event)];
};