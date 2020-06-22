open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module InputMethodHandler = Handle__InputMethod.Impl(Editor);

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
                  Command.toString(Command.Give),
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
            (goal, _) => [Goal(SaveCursor), SendRequest(Refine(goal))],
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
        let header = Command.toString(Command.Case);
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
        let header = Command.toString(Command.InferType(normalization));
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
            (goal, _) => {[SendRequest(GoalType(normalization, goal))]},
            [Error(Error.OutOfGoal)],
          ),
        ),
      ]
    | GoalTypeAndContext(normalization) => [
        Goal(
          GetPointedOr(
            (goal, _) => {
              [SendRequest(GoalTypeAndContext(normalization, goal))]
            },
            [Error(Error.OutOfGoal)],
          ),
        ),
      ]
    | EventFromView(event) =>
      switch (event) {
      | Initialized => []
      | Destroyed => [Terminate]
      | InputMethod(InsertChar(char)) => [
          DispatchCommand(InputMethod(InsertChar(char))),
        ]
      | InputMethod(ChooseSymbol(symbol)) => [
          DispatchCommand(InputMethod(ChooseSymbol(symbol))),
        ]
      }
    | Escape => [SendEventToView(InterruptQuery)]
    | InputMethod(action) => InputMethodHandler.handle(action);
};