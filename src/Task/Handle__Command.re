open Command;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module InputMethodHandler = Handle__InputMethod.Impl(Editor);

  open! Task;
  // from Editor Command to Tasks
  let handle = command => {
    let header = Command.toString(command);
    switch (command) {
    | Load => [
        Task.WithStateP(
          state => Editor.save(state.editor)->Promise.map(_ => []),
        ),
        Goal(SaveCursor),
        RemoveAllHighlightings,
        SendRequest(Load),
      ]
    | Quit => [RemoveAllHighlightings, Terminate]
    | Restart => [Debug("Restart")]
    | Compile => [SendRequest(Compile)]
    | ToggleDisplayOfImplicitArguments => [
        SendRequest(ToggleDisplayOfImplicitArguments),
      ]
    | ShowConstraints => [SendRequest(ShowConstraints)]
    | SolveConstraints(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [SendRequest(SolveConstraints(normalization, goal))],
            [SendRequest(SolveConstraintsGlobal(normalization))],
          ),
        ),
      ]
    | ShowGoals => [SendRequest(ShowGoals)]
    | NextGoal => [Goal(Next)]
    | PreviousGoal => [Goal(Previous)]
    | SearchAbout(normalization) =>
      query(header, Some("name:"), None, expr =>
        [SendRequest(SearchAbout(normalization, expr))]
      )
    | Give => [
        Goal(
          LocalOrGlobal2(
            (goal, _) => [SendRequest(Give(goal))],
            goal =>
              query(header, Some("expression to give:"), None, expr =>
                [Goal(Modify(goal, _ => expr)), SendRequest(Give(goal))]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | Refine => [
        Goal(
          LocalOrGlobal(
            goal => [Goal(SaveCursor), SendRequest(Refine(goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | ElaborateAndGive(normalization) =>
      let placeholder = Some("expression to elaborate and give:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [SendRequest(ElaborateAndGive(normalization, expr, goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [SendRequest(ElaborateAndGive(normalization, expr, goal))]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ];
    | Auto => [
        Goal(
          LocalOrGlobal(
            goal => {[SendRequest(Auto(goal))]},
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | Case =>
      let placeholder = Some("expression to case:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, _) => [Goal(SaveCursor), SendRequest(Case(goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [
                  Goal(Modify(goal, _ => expr)), // place the queried expression in the goal
                  Goal(SaveCursor),
                  SendRequest(Case(goal)),
                ]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ];
    | HelperFunctionType(normalization) =>
      let placeholder = Some("expression:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [SendRequest(HelperFunctionType(normalization, expr, goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [SendRequest(HelperFunctionType(normalization, expr, goal))]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ];
    | InferType(normalization) =>
      let placeholder = Some("expression to infer:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [SendRequest(InferType(normalization, expr, goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [SendRequest(InferType(normalization, expr, goal))]
              ),
            query(header, placeholder, None, expr =>
              [SendRequest(InferTypeGlobal(normalization, expr))]
            ),
          ),
        ),
      ];
    | Context(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [SendRequest(Context(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalType(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [SendRequest(GoalType(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalTypeAndContext(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [SendRequest(GoalTypeAndContext(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalTypeContextAndInferredType(normalization) =>
      let placeholder = Some("expression to type:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [
                SendRequest(
                  GoalTypeContextAndInferredType(normalization, expr, goal),
                ),
              ],
            goal =>
              query(header, placeholder, None, expr =>
                [
                  SendRequest(
                    GoalTypeContextAndInferredType(normalization, expr, goal),
                  ),
                ]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ];
    | GoalTypeContextAndCheckedType(normalization) =>
      let placeholder = Some("expression to type:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [
                SendRequest(
                  GoalTypeContextAndCheckedType(normalization, expr, goal),
                ),
              ],
            goal =>
              query(header, placeholder, None, expr =>
                [
                  SendRequest(
                    GoalTypeContextAndCheckedType(normalization, expr, goal),
                  ),
                ]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ];
    | ModuleContents(normalization) =>
      let placeholder = Some("module name:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [SendRequest(ModuleContents(normalization, expr, goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [SendRequest(ModuleContents(normalization, expr, goal))]
              ),
            query(header, placeholder, None, expr =>
              [SendRequest(ModuleContentsGlobal(normalization, expr))]
            ),
          ),
        ),
      ];
    | ComputeNormalForm(computeMode) =>
      let placeholder = Some("expression to normalize:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [SendRequest(ComputeNormalForm(computeMode, expr, goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [SendRequest(ComputeNormalForm(computeMode, expr, goal))]
              ),
            query(header, placeholder, None, expr =>
              [SendRequest(ComputeNormalFormGlobal(computeMode, expr))]
            ),
          ),
        ),
      ];
    | WhyInScope =>
      let placeholder = Some("name:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) => [SendRequest(WhyInScope(expr, goal))],
            goal =>
              query(header, placeholder, None, expr =>
                [SendRequest(WhyInScope(expr, goal))]
              ),
            query(header, placeholder, None, expr =>
              [SendRequest(WhyInScopeGlobal(expr))]
            ),
          ),
        ),
      ];
    | EventFromView(event) =>
      switch (event) {
      | Initialized => []
      | Destroyed => [Terminate]
      | InputMethod(InsertChar(char)) => [
          Goal(SaveCursor),
          DispatchCommand(InputMethod(InsertChar(char))),
          Goal(RestoreCursor),
        ]
      | InputMethod(ChooseSymbol(symbol)) => [
          Goal(SaveCursor),
          DispatchCommand(InputMethod(ChooseSymbol(symbol))),
          Goal(RestoreCursor),
        ]
      }
    | Escape => [SendEventToView(InterruptQuery)]
    | InputMethod(action) => InputMethodHandler.handle(action)
    };
  };
};
