// from Agda Response to Tasks
open Belt;
open VSCode;
open! Task;
open Response;
module DisplayInfo = {
  let handle =
    fun
    | Response.DisplayInfo.CompilationOk => [
        display(Success("Compilation Done!"), Nothing),
      ]
    | Constraints(None) => [display(Plain("No Constraints"), Nothing)]
    | Constraints(Some(body)) => [
        displayEmacs(Outputs, Plain("Constraints"), body),
      ]
    | AllGoalsWarnings(header, "nil") => [
        display(Success(header), Nothing),
      ]
    | AllGoalsWarnings(header, body) => [
        displayEmacs(AllGoalsWarnings, Plain(header), body),
      ]
    | Time(body) => [displayEmacs(Text, Plain("Time"), body)]
    | Error(body) => [displayEmacs(Error, Error("Error!"), body)]
    | Intro(body) => [displayEmacs(Text, Plain("Intro"), body)]
    | Auto(body) => [displayEmacs(Text, Plain("Auto"), body)]
    | ModuleContents(body) => [
        displayEmacs(Text, Plain("Module Contents"), body),
      ]
    | SearchAbout(body) => [
        displayEmacs(SearchAbout, Plain("Search About"), body),
      ]
    | WhyInScope(body) => [displayEmacs(Text, Plain("Scope info"), body)]
    | NormalForm(body) => [displayEmacs(Text, Plain("Normal form"), body)]
    | GoalType(body) => [
        displayEmacs(GoalType, Plain("Goal and Context"), body),
      ]
    | CurrentGoal(payload) => [
        display(Plain("Current goal"), Plain(payload)),
      ]
    | InferredType(payload) => [
        display(Plain("Inferred type"), Plain(payload)),
      ]
    | Context(body) => [displayEmacs(Outputs, Plain("Context"), body)]
    | HelperFunction(payload) => [
        WithStateP(
          _ =>
            Env.clipboard
            ->Clipboard.writeText(payload)
            ->Promise.map(() => []),
        ),
        display(
          Plain("Helper function (copied to clipboard)"),
          Plain(payload),
        ),
      ]
    | Version(payload) => [display(Plain("Version"), Plain(payload))];
};

let handle = response => {
  // Js.log(Response.toString(response));
  switch (response) {
  | HighlightingInfoDirect(_remove, annotations) => [
      Decoration(AddViaPipe(annotations)),
    ]
  | HighlightingInfoIndirect(filepath) => [
      Decoration(AddViaFile(filepath)),
    ]
  | ClearHighlighting => [Decoration(Clear)]
  | Status(_displayImplicit, _checked) =>
    // display(
    //   "Status",
    //   Some(
    //     "Typechecked: "
    //     ++ string_of_bool(checked)
    //     ++ "\nDisplay implicit arguments: "
    //     ++ string_of_bool(displayImplicit),
    //   ),
    // ),
    []
  // if (displayImplicit || checked) {
  //   [
  //     display(
  //       "Status",
  //       Some(
  //         "Typechecked: "
  //         ++ string_of_bool(checked)
  //         ++ "\nDisplay implicit arguments: "
  //         ++ string_of_bool(displayImplicit),
  //       ),
  //     ),
  //   ];
  // } else {
  //   [];
  // }
  | JumpToError(filepath, offset) => [
      WithStateP(
        state => {
          // only jump to site of error
          // when it's on the same file
          let path =
            state.editor
            ->TextEditor.document
            ->TextDocument.fileName
            ->Parser.filepath;
          if (path == filepath) {
            Promise.resolved([Goal(SetCursor(offset - 1))]);
          } else {
            Promise.resolved([]);
          };
        },
      ),
    ]
  | InteractionPoints(indices) => [
      BenchStart("$$$ Instantiating goals"),
      Goal(Instantiate(indices)),
      BenchEnd("$$$ Instantiating goals"),
    ]
  | GiveAction(index, give) => [
      WithStateP(
        state => {
          let found = state.goals->Array.keep(goal => goal.index == index);
          switch (found[0]) {
          | None =>
            Promise.resolved([
              display(
                Error("Error: Give failed"),
                Plain("Cannot find goal #" ++ string_of_int(index)),
              ),
            ])
          | Some(goal) =>
            Promise.resolved(
              switch (give) {
              | Paren => [
                  Goal(Modify(goal, content => "(" ++ content ++ ")")),
                  Goal(RemoveBoundaryAndDestroy(goal)),
                ]
              | NoParen =>
                // do nothing
                [Goal(RemoveBoundaryAndDestroy(goal))]
              | String(content) => [
                  Goal(
                    Modify(
                      goal,
                      _ =>
                        Js.String.replaceByRe(
                          [%re "/\\\\n/g"],
                          "\n",
                          content,
                        ),
                    ),
                  ),
                  Goal(RemoveBoundaryAndDestroy(goal)),
                ]
              },
            )
          };
        },
      ),
    ]
  | MakeCase(makeCaseType, lines) => [
      Goal(
        LocalOrGlobal(
          goal => {
            switch (makeCaseType) {
            | Function => [
                Goal(ReplaceWithLines(goal, lines)),
                DispatchCommand(Load),
              ]
            | ExtendedLambda => [
                Goal(ReplaceWithLambda(goal, lines)),
                DispatchCommand(Load),
              ]
            }
          },
          [Error(OutOfGoal)],
        ),
      ),
    ]
  | SolveAll(solutions) => [
      WithStateP(
        state => {
          let solveOne = ((index, solution)) => {
            let goals = state.goals->Array.keep(goal => goal.index == index);
            switch (goals[0]) {
            | None => []
            | Some(goal) => [
                Goal(Modify(goal, _ => solution)),
                AgdaRequest(Give(goal)),
              ]
            };
          };

          // solve them one by one
          let tasks = solutions->Array.map(solveOne)->List.concatMany;
          let size = Array.length(solutions);
          let after =
            if (size == 0) {
              [display(Error("No solutions found"), Nothing)];
            } else {
              [
                display(
                  Success(string_of_int(size) ++ " goals solved"),
                  Nothing,
                ),
              ];
            };
          Promise.resolved(List.concat(tasks, after));
        },
      ),
    ]
  | DisplayInfo(info) => DisplayInfo.handle(info)
  | RunningInfo(_verbosity, message) => [
      display(Plain("Type-checking"), Plain(message)),
    ]
  | _ => []
  // | others => [Debug(Response.toString(others))];
  };
};
