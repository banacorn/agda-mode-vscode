// from Agda Response to Tasks
module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  open Belt;
  open! Task;
  open Response;
  module DisplayInfo = {
    let handle =
      fun
      | Response.DisplayInfo.CompilationOk => [
          displayHeaderOnly(Success("Compilation Done!")),
        ]
      | Constraints(None) => [displayHeaderOnly(Plain("No Constraints"))]
      | Constraints(Some(body)) => [
          displayEmacs(Outputs, Plain("Constraints"), body),
        ]
      | AllGoalsWarnings(header, "nil") => [
          displayHeaderOnly(Success(header)),
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
      | NormalForm(body) => [
          displayEmacs(Text, Plain("Normal form"), body),
        ]
      | GoalType(body) => [
          displayEmacs(GoalType, Plain("Goal and Context"), body),
        ]
      | CurrentGoal(payload) => [display("Current goal", Some(payload))]
      | InferredType(payload) => [display("Inferred type", Some(payload))]
      | Context(body) => [displayEmacs(Outputs, Plain("Context"), body)]
      | HelperFunction(payload) => [
          WithStateP(
            _ => Editor.copyToClipboard(payload)->Promise.map(() => []),
          ),
          display("Helper function (copied to clipboard)", Some(payload)),
        ]
      | Version(payload) => [display("Version", Some(payload))];
  };

  let handle = response => {
    Js.log(Response.toString(response));
    switch (response) {
    | HighlightingInfoDirect(_remove, annotations) => [
        Decoration(Add(annotations)),
      ]
    | HighlightingInfoIndirect(filepath) => [
        WithStateP(
          _ => {
            let readFile = N.Util.promisify(N.Fs.readFile);
            readFile(. filepath)
            ->Promise.Js.fromBsPromise
            ->Promise.Js.toResult
            ->Promise.map(
                fun
                | Ok(content) => {
                    open! Parser.SExpression;
                    let expressions =
                      content->Node.Buffer.toString->Parser.SExpression.parse;

                    // TODO: we should do something about these parse errors
                    let _parseErrors: array((int, string)) =
                      expressions->Array.keepMap(
                        fun
                        | Error(error) => Some(error)
                        | Ok(_) => None,
                      );

                    let annotations: array(Highlighting.t) =
                      expressions
                      ->Array.keepMap(
                          fun
                          | Error(_) => None // filter errors out
                          | Ok(L(xs)) =>
                            Some(Highlighting.parseIndirectHighlightings(xs))
                          | Ok(_) => Some([||]),
                        )
                      ->Array.concatMany;

                    [Decoration(Add(annotations))];
                  }
                // TODO: we should do something about these parse errors
                | Error(_err) => [],
              );
          },
        ),
      ]
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
            switch (Editor.getFileName(state.editor)) {
            | None => Promise.resolved([])
            | Some(path) =>
              if (path == filepath) {
                Promise.resolved([Goal(SetCursor(offset - 1))]);
              } else {
                Promise.resolved([]);
              }
            }
          },
        ),
      ]
    | InteractionPoints(indices) => [Goal(Instantiate(indices))]
    | GiveAction(index, give) => [
        WithStateP(
          state => {
            let found = state.goals->Array.keep(goal => goal.index == index);
            switch (found[0]) {
            | None =>
              Promise.resolved([
                displayError(
                  "Error: Give failed",
                  Some("Cannot find goal #" ++ string_of_int(index)),
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
                [displayError("No solutions found", None)];
              } else {
                [
                  displaySuccess(
                    string_of_int(size) ++ " goals solved",
                    None,
                  ),
                ];
              };
            Promise.resolved(List.concat(tasks, after));
          },
        ),
      ]
    | DisplayInfo(info) => DisplayInfo.handle(info)
    | RunningInfo(_verbosity, message) => [
        display("Type-checking", Some(message)),
      ]
    | _ => []
    // | others => [Debug(Response.toString(others))];
    };
  };
};
