// from Agda Response to Tasks
open Belt

open Response
module DisplayInfo = {
  let handle = x =>
    switch x {
    | Response.DisplayInfo.CompilationOk =>
      State.View.display(Success("Compilation Done!"), Nothing)
    | Constraints(None) => State.View.display(Plain("No Constraints"), Nothing)
    | Constraints(Some(body)) => State.View.displayEmacs(Outputs, Plain("Constraints"), body)
    | AllGoalsWarnings(header, "nil") => State.View.display(Success(header), Nothing)
    | AllGoalsWarnings(header, body) =>
      State.View.displayEmacs(AllGoalsWarnings, Plain(header), body)
    | Time(body) => State.View.displayEmacs(Text, Plain("Time"), body)
    | Error(body) => State.View.displayEmacs(Error, Error("Error!"), body)
    | Intro(body) => State.View.displayEmacs(Text, Plain("Intro"), body)
    | Auto(body) => State.View.displayEmacs(Text, Plain("Auto"), body)
    | ModuleContents(body) => State.View.displayEmacs(Text, Plain("Module Contents"), body)
    | SearchAbout(body) => State.View.displayEmacs(SearchAbout, Plain("Search About"), body)
    | WhyInScope(body) => State.View.displayEmacs(Text, Plain("Scope info"), body)
    | NormalForm(body) => State.View.displayEmacs(Text, Plain("Normal form"), body)
    | GoalType(body) => State.View.displayEmacs(GoalType, Plain("Goal and Context"), body)
    | CurrentGoal(payload) => State.View.display(Plain("Current goal"), Plain(payload))
    | InferredType(payload) => State.View.display(Plain("Inferred type"), Plain(payload))
    | Context(body) => State.View.displayEmacs(Outputs, Plain("Context"), body)
    | HelperFunction(payload) =>
      VSCode.Env.clipboard
      ->VSCode.Clipboard.writeText(payload)
      ->Promise.flatMap(() =>
        State.View.display(Plain("Helper function (copied to clipboard)"), Plain(payload))
      )
    | Version(payload) => State.View.display(Plain("Version"), Plain(payload))
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
    state.decoration->Decoration.addViaPipe(annotations)
    Promise.resolved()
  | HighlightingInfoIndirect(filepath) =>
    state.decoration->Decoration.addViaFile(filepath)
    Promise.resolved()
  | ClearHighlighting =>
    state.decoration->Decoration.clear
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
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError()
    | Some((goal, _)) =>
      switch makeCaseType {
      | Function => State__Goal.replaceWithLines(state, goal, lines)
      | ExtendedLambda => State__Goal.replaceWithLambda(state, goal, lines)
      }->Promise.flatMap(() => dispatchCommand(Load))
    }
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
        State.View.display(Error("No solutions found"), Nothing)
      } else {
        State.View.display(Success(string_of_int(size) ++ " goals solved"), Nothing)
      }
    })

  | DisplayInfo(info) => DisplayInfo.handle(info)
  | RunningInfo(_verbosity, message) => State.View.display(Plain("Type-checking"), Plain(message))
  | _ => Promise.resolved()
  }
}
