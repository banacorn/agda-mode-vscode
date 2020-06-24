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
          displaySuccess("Compilation Done!", None),
        ]
      | Constraints(None) => [display("No Constraints", None)]
      | Constraints(Some(payload)) => [
          display("Constraints", Some(payload)),
        ]
      | AllGoalsWarnings(header, body) => [display(header, Some(body))]
      | Time(payload) => [display("Time", Some(payload))]
      | Error(payload) => [displayError("Error", Some(payload))]
      | Intro(payload) => [display("Intro", Some(payload))]
      | Auto(payload) => [displaySuccess("Auto", Some(payload))]
      | ModuleContents(payload) => [
          display("Module Contents", Some(payload)),
        ]
      | SearchAbout(payload) => [
          display("Searching about ...", Some(payload)),
        ]
      | WhyInScope(payload) => [display("Scope info", Some(payload))]
      | NormalForm(payload) => [display("Normal form", Some(payload))]
      | GoalType(payload) => [display("Goal Type", Some(payload))]
      | CurrentGoal(payload) => [display("Current goal", Some(payload))]
      | InferredType(payload) => [display("Inferred type", Some(payload))]
      | Context(payload) => [display("Context", Some(payload))]
      | HelperFunction(payload) => [
          WithStateP(
            _ => Editor.copyToClipboard(payload)->Promise.map(() => []),
          ),
          display("Helper function (copied to clipboard)", Some(payload)),
        ]
      | Version(payload) => [display("Version", Some(payload))];
  };

  let handle = response => {
    switch (response) {
    | HighlightingInfoDirect(_remove, annotations) => [
        AddHighlightings(annotations),
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

                    let annotations: array(Response.Highlighting.t) =
                      expressions
                      ->Array.keepMap(
                          fun
                          | Error(_) => None // filter errors out
                          | Ok(L(xs)) =>
                            Some(
                              Response.Highlighting.parseIndirectHighlightings(
                                xs,
                              ),
                            )
                          | Ok(_) => Some([||]),
                        )
                      ->Array.concatMany;

                    [AddHighlightings(annotations)];
                  }
                // TODO: we should do something about these parse errors
                | Error(_err) => [],
              );
          },
        ),
      ]
    | Status(displayImplicit, checked) =>
      if (displayImplicit || checked) {
        [
          display(
            "Status",
            Some(
              "Typechecked: "
              ++ string_of_bool(checked)
              ++ "\nDisplay implicit arguments: "
              ++ string_of_bool(displayImplicit),
            ),
          ),
        ];
      } else {
        [];
      }
    | InteractionPoints(indices) => [
        Goal(Instantiate(indices)),
        Goal(RestoreCursor),
      ]
    | GiveAction(index, give) => [
        Goal(
          GetIndexedOr(
            index,
            (goal, _) => {
              let tasks =
                switch (give) {
                | Paren => [
                    Goal(Modify(goal, content => "(" ++ content ++ ")")),
                  ]
                | NoParen =>
                  // do nothing
                  []
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
                  ]
                };
              List.concatMany([|
                tasks,
                [Goal(RemoveBoundaryAndDestroy(goal))],
              |]);
            },
            [
              displayError(
                "Error: Give failed",
                Some("Cannot find goal #" ++ string_of_int(index)),
              ),
            ],
          ),
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
    | DisplayInfo(info) => DisplayInfo.handle(info)
    | RunningInfo(_verbosity, message) => [
        display("Type-checking", Some(message)),
      ]
    | _ => []
    // | others => [Debug(Response.toString(others))];
    };
  };
};