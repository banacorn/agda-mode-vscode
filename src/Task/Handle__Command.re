open Belt;
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
        timeStart(">>> LOAD"),
        display("Loading ...", None),
        timeStart(">>> save editor"),
        Task.WithStateP(
          state => Editor.save(state.editor)->Promise.map(_ => []),
        ),
        timeEnd(">>> save editor"),
        timeStart(">>> Send request"),
        AgdaRequest(Load),
        timeEnd(">>> Send request"),
        timeEnd(">>> LOAD"),
      ]
    | Quit => []
    | Restart => [DispatchCommand(Load)]
    | Refresh => [Goal(UpdateRange), Decoration(Refresh)]
    | Compile => [AgdaRequest(Compile)]
    | ToggleDisplayOfImplicitArguments => [
        AgdaRequest(ToggleDisplayOfImplicitArguments),
      ]
    | ShowConstraints => [AgdaRequest(ShowConstraints)]
    | SolveConstraints(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [AgdaRequest(SolveConstraints(normalization, goal))],
            [AgdaRequest(SolveConstraintsGlobal(normalization))],
          ),
        ),
      ]
    | ShowGoals => [AgdaRequest(ShowGoals)]
    | NextGoal => [Goal(Next)]
    | PreviousGoal => [Goal(Previous)]
    | SearchAbout(normalization) =>
      query(header, None, Some("name:"), None, expr =>
        [AgdaRequest(SearchAbout(normalization, expr))]
      )
    | Give => [
        Goal(
          LocalOrGlobal2(
            (goal, _) => [AgdaRequest(Give(goal))],
            goal =>
              query(header, None, Some("expression to give:"), None, expr =>
                [Goal(Modify(goal, _ => expr)), AgdaRequest(Give(goal))]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | Refine => [
        Goal(
          LocalOrGlobal(
            goal => [AgdaRequest(Refine(goal))],
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
              [AgdaRequest(ElaborateAndGive(normalization, expr, goal))],
            goal =>
              query(header, None, placeholder, None, expr =>
                [AgdaRequest(ElaborateAndGive(normalization, expr, goal))]
              ),
            [Error(OutOfGoal)],
          ),
        ),
      ];
    | Auto => [
        WithStateP(
          state => {
            let isFLOLAC =
              Editor.getFileName(state.editor)
              ->Option.mapWithDefault(false, fileName => {
                  Js.Re.test_(
                    [%re "/^FLOLAC-/"],
                    Node.Path.basename(fileName),
                  )
                });
            if (isFLOLAC) {
              Promise.resolved([ViewEvent(Prank)]);
            } else {
              Promise.resolved([
                Goal(
                  LocalOrGlobal(
                    goal => {[AgdaRequest(Auto(goal))]},
                    [Error(OutOfGoal)],
                  ),
                ),
              ]);
            };
          },
        ),
      ]
    | Case =>
      let placeholder = Some("variable to case split:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, _) => [AgdaRequest(Case(goal))],
            // _goal =>
            //   [
            //     displayWarning(
            //       "Don't know which variable to case split",
            //       Some(
            //         "Please specify the variable you wish to split in the goal",
            //       ),
            //     ),
            //   ],
            goal =>
              query(
                header,
                Some("Please specify which variable you wish to split"),
                placeholder,
                None,
                expr =>
                [
                  Goal(Modify(goal, _ => expr)), // place the queried expression in the goal
                  AgdaRequest(Case(goal)),
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
              [AgdaRequest(HelperFunctionType(normalization, expr, goal))],
            goal =>
              query(header, None, placeholder, None, expr =>
                [AgdaRequest(HelperFunctionType(normalization, expr, goal))]
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
              [AgdaRequest(InferType(normalization, expr, goal))],
            goal =>
              query(header, None, placeholder, None, expr =>
                [AgdaRequest(InferType(normalization, expr, goal))]
              ),
            query(header, None, placeholder, None, expr =>
              [AgdaRequest(InferTypeGlobal(normalization, expr))]
            ),
          ),
        ),
      ];
    | Context(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [AgdaRequest(Context(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalType(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [AgdaRequest(GoalType(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalTypeAndContext(normalization) => [
        Goal(
          LocalOrGlobal(
            goal => [AgdaRequest(GoalTypeAndContext(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalTypeContextAndInferredType(normalization) => [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [
                AgdaRequest(
                  GoalTypeContextAndInferredType(normalization, expr, goal),
                ),
              ],
            // fallback to `GoalTypeAndContext` when there's no content
            goal => [AgdaRequest(GoalTypeAndContext(normalization, goal))],
            [Error(OutOfGoal)],
          ),
        ),
      ]
    | GoalTypeContextAndCheckedType(normalization) =>
      let placeholder = Some("expression to type:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) =>
              [
                AgdaRequest(
                  GoalTypeContextAndCheckedType(normalization, expr, goal),
                ),
              ],
            goal =>
              query(header, None, placeholder, None, expr =>
                [
                  AgdaRequest(
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
              [AgdaRequest(ModuleContents(normalization, expr, goal))],
            goal =>
              query(header, None, placeholder, None, expr =>
                [AgdaRequest(ModuleContents(normalization, expr, goal))]
              ),
            query(header, None, placeholder, None, expr =>
              [AgdaRequest(ModuleContentsGlobal(normalization, expr))]
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
              [AgdaRequest(ComputeNormalForm(computeMode, expr, goal))],
            goal =>
              query(header, None, placeholder, None, expr =>
                [AgdaRequest(ComputeNormalForm(computeMode, expr, goal))]
              ),
            query(header, None, placeholder, None, expr =>
              [AgdaRequest(ComputeNormalFormGlobal(computeMode, expr))]
            ),
          ),
        ),
      ];
    | WhyInScope =>
      let placeholder = Some("name:");
      [
        Goal(
          LocalOrGlobal2(
            (goal, expr) => [AgdaRequest(WhyInScope(expr, goal))],
            goal =>
              query(header, None, placeholder, None, expr =>
                [AgdaRequest(WhyInScope(expr, goal))]
              ),
            query(header, None, placeholder, None, expr =>
              [AgdaRequest(WhyInScopeGlobal(expr))]
            ),
          ),
        ),
      ];
    | EventFromView(event) =>
      switch (event) {
      | Initialized => []
      | Destroyed => [Destroy]
      | InputMethod(InsertChar(char)) => [
          DispatchCommand(InputMethod(InsertChar(char))),
        ]
      | InputMethod(ChooseSymbol(symbol)) => [
          DispatchCommand(InputMethod(ChooseSymbol(symbol))),
        ]
      | QueryChange(input) => [
          DispatchCommand(InputMethod(QueryChange(input))),
        ]
      | JumpToTarget(link) => [
          WithState(
            state => {
              Editor.focus(state.editor);
              switch (Editor.getFileName(state.editor)) {
              | None => ()
              | Some(path) =>
                switch (link) {
                | ToRange(NoRange) => ()
                | ToRange(Range(None, _intervals)) => ()
                | ToRange(Range(Some(filePath), intervals)) =>
                  // only select the intervals when it's on the same file
                  if (path == filePath) {
                    let ranges =
                      intervals->Array.map(Editor.View.fromInterval);
                    Editor.setSelections(state.editor, ranges);
                  }
                | ToHole(index) =>
                  let goal =
                    Js.Array.find(
                      (goal: Goal.t) => goal.index == index,
                      state.goals,
                    );
                  switch (goal) {
                  | None => ()
                  | Some(goal) => Goal.setCursor(goal, state.editor)
                  };
                }
              };
            },
          ),
        ]
      | MouseOver(_) => [Debug("MouseOver")]
      | MouseOut(_) => [Debug("MouseOut")]
      }
    | Escape => [
        WithStateP(
          state => {
            Promise.resolved(
              if (state.editorIM.activated) {
                [DispatchCommand(InputMethod(Deactivate))];
              } else {
                [ViewEvent(QueryInterrupt)];
              },
            )
          },
        ),
      ]
    | InputMethod(action) => InputMethodHandler.handle(action)
    };
  };
};
