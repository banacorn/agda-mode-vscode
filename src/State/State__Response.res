// from Agda Response to Tasks
open Belt

open Response
module DisplayInfo = {
  let handle = (state, x) =>
    switch x {
    | Response.DisplayInfo.CompilationOk =>
      State.View.display(state, Success("Compilation Done!"), Nothing)
    | Constraints(None) => State.View.display(state, Plain("No Constraints"), Nothing)
    | Constraints(Some(body)) => State.View.displayEmacs(state, Outputs, Plain("Constraints"), body)
    | AllGoalsWarnings(header, "nil") => State.View.display(state, Success(header), Nothing)
    | AllGoalsWarnings(header, body) =>
      State.View.displayEmacs(state, AllGoalsWarnings, Plain(header), body)
    | Time(body) => State.View.displayEmacs(state, Text, Plain("Time"), body)
    | Error(body) => State.View.displayEmacs(state, Error, Error("Error!"), body)
    | Intro(body) => State.View.displayEmacs(state, Text, Plain("Intro"), body)
    | Auto(body) => State.View.displayEmacs(state, Text, Plain("Auto"), body)
    | ModuleContents(body) => State.View.displayEmacs(state, Text, Plain("Module Contents"), body)
    | SearchAbout(body) => State.View.displayEmacs(state, SearchAbout, Plain("Search About"), body)
    | WhyInScope(body) => State.View.displayEmacs(state, Text, Plain("Scope info"), body)
    | NormalForm(body) => State.View.displayEmacs(state, Text, Plain("Normal form"), body)
    | GoalType(body) => State.View.displayEmacs(state, GoalType, Plain("Goal and Context"), body)
    | CurrentGoal(payload) => State.View.display(state, Plain("Current goal"), Plain(payload))
    | InferredType(payload) => State.View.display(state, Plain("Inferred type"), Plain(payload))
    | Context(body) => State.View.displayEmacs(state, Outputs, Plain("Context"), body)
    | HelperFunction(payload) =>
      VSCode.Env.clipboard
      ->VSCode.Clipboard.writeText(payload)
      ->Promise.flatMap(() =>
        State.View.display(state, Plain("Helper function (copied to clipboard)"), Plain(payload))
      )
    | Version(payload) => State.View.display(state, Plain("Version"), Plain(payload))
    }
}

let rec handle = (
  state: State.t,
  dispatchCommand: Command.t => Promise.t<unit>,
  response: Response.t,
): Promise.t<unit> => {
  let sendAgdaRequest = State.Connection.sendRequest(state, handle(state, dispatchCommand))
  switch response {
  | HighlightingInfoDirect(_remove, annotations) =>
    State.Decoration.addViaPipe(state, annotations)
    Promise.resolved()
  | HighlightingInfoIndirect(filepath) =>
    State.Decoration.addViaFile(state, filepath)
    Promise.resolved()
  | ClearHighlighting =>
    State.Decoration.clear(state)
    Promise.resolved()
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
    Promise.resolved()

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
  | JumpToError(filepath, offset) =>
    // only jump to site of error
    // when it's on the same file
    let path = state.document->VSCode.TextDocument.fileName->Parser.filepath
    if path == filepath {
      let point = state.document->VSCode.TextDocument.positionAt(offset - 1)
      Editor.Cursor.set(state.editor, point)
    }
    Promise.resolved()
  | InteractionPoints(indices) => State__Goal.instantiate(state, indices)
  | GiveAction(index, give) =>
    let found = state.goals->Array.keep(goal => goal.index == index)
    switch found[0] {
    | None =>
      State.View.display(
        state,
        Error("Error: Give failed"),
        Plain("Cannot find goal #" ++ string_of_int(index)),
      )
    | Some(goal) =>
      switch give {
      | Paren =>
        State__Goal.modify(state, goal, content => "(" ++ (content ++ ")"))->Promise.flatMap(() =>
          State__Goal.removeBoundaryAndDestroy(state, goal)
        )
      | NoParen =>
        // do nothing
        State__Goal.removeBoundaryAndDestroy(state, goal)
      | String(content) =>
        State__Goal.modify(state, goal, _ =>
          Js.String.replaceByRe(%re("/\\\\n/g"), "\n", content)
        )->Promise.flatMap(() => State__Goal.removeBoundaryAndDestroy(state, goal))
      }
    }
  | MakeCase(makeCaseType, lines) =>
    State__Goal.caseSimple(
      state,
      goal =>
        switch makeCaseType {
        | Function => State__Goal.replaceWithLines(state, goal, lines)
        | ExtendedLambda => State__Goal.replaceWithLambda(state, goal, lines)
        }->Promise.flatMap(() => dispatchCommand(Load)),
      State.View.displayOutOfGoalError(state),
    )
  | SolveAll(solutions) =>
    let solveOne = ((index, solution)): Promise.t<unit> => {
      let goals = state.goals->Array.keep(goal => goal.index == index)
      switch goals[0] {
      | None => Promise.resolved()
      | Some(goal) =>
        State__Goal.modify(state, goal, _ => solution)->Promise.flatMap(() =>
          sendAgdaRequest(Give(goal))
        )
      }
    }
    // solve them one by one
    solutions
    ->Array.map(solveOne)
    ->Util.oneByOne
    ->Promise.flatMap(_ => {
      let size = Array.length(solutions)
      if size == 0 {
        State.View.display(state, Error("No solutions found"), Nothing)
      } else {
        State.View.display(state, Success(string_of_int(size) ++ " goals solved"), Nothing)
      }
    })

  | DisplayInfo(info) => DisplayInfo.handle(state, info)
  | RunningInfo(_verbosity, message) =>
    State.View.display(state, Plain("Type-checking"), Plain(message))
  | _ => Promise.resolved()
  }
}
