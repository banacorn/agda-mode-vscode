// from Agda Response to Tasks
open Belt
open! Task
open Response
module DisplayInfo = {
  let handle = (state, x) =>
    switch x {
    | Response.DisplayInfo.CompilationOk => display(state, Success("Compilation Done!"), Nothing)
    | Constraints(None) => display(state, Plain("No Constraints"), Nothing)
    | Constraints(Some(body)) => displayEmacs(state, Outputs, Plain("Constraints"), body)
    | AllGoalsWarnings(header, "nil") => display(state, Success(header), Nothing)
    | AllGoalsWarnings(header, body) => displayEmacs(state, AllGoalsWarnings, Plain(header), body)
    | Time(body) => displayEmacs(state, Text, Plain("Time"), body)
    | Error(body) => displayEmacs(state, Error, Error("Error!"), body)
    | Intro(body) => displayEmacs(state, Text, Plain("Intro"), body)
    | Auto(body) => displayEmacs(state, Text, Plain("Auto"), body)
    | ModuleContents(body) => displayEmacs(state, Text, Plain("Module Contents"), body)
    | SearchAbout(body) => displayEmacs(state, SearchAbout, Plain("Search About"), body)
    | WhyInScope(body) => displayEmacs(state, Text, Plain("Scope info"), body)
    | NormalForm(body) => displayEmacs(state, Text, Plain("Normal form"), body)
    | GoalType(body) => displayEmacs(state, GoalType, Plain("Goal and Context"), body)
    | CurrentGoal(payload) => display(state, Plain("Current goal"), Plain(payload))
    | InferredType(payload) => display(state, Plain("Inferred type"), Plain(payload))
    | Context(body) => displayEmacs(state, Outputs, Plain("Context"), body)
    | HelperFunction(payload) =>
      VSCode.Env.clipboard
      ->VSCode.Clipboard.writeText(payload)
      ->Promise.flatMap(() =>
        display(state, Plain("Helper function (copied to clipboard)"), Plain(payload))
      )
    | Version(payload) => display(state, Plain("Version"), Plain(payload))
    }
}

let handle = (
  state: State.t,
  dispatchCommand: Command.t => Promise.t<unit>,
  sendAgdaRequest: Request.t => Promise.t<unit>,
  response: Response.t,
): Promise.t<unit> =>
  switch response {
  | HighlightingInfoDirect(_remove, annotations) =>
    Handle__Decoration.addViaPipe(state, annotations)
    Promise.resolved()
  | HighlightingInfoIndirect(filepath) =>
    Handle__Decoration.addViaFile(state, filepath)
    Promise.resolved()
  | ClearHighlighting =>
    Handle__Decoration.clear(state)
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
    let path =
      state.editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
    if path == filepath {
      state->Handle__Goal.setCursor(offset)
    }
    Promise.resolved()
  | InteractionPoints(indices) => Handle__Goal.instantiate(state, indices)
  | GiveAction(index, give) =>
    let found = state.goals->Array.keep(goal => goal.index == index)
    switch found[0] {
    | None =>
      display(
        state,
        Error("Error: Give failed"),
        Plain("Cannot find goal #" ++ string_of_int(index)),
      )
    | Some(goal) =>
      switch give {
      | Paren =>
        Handle__Goal.modify(state, goal, content => "(" ++ (content ++ ")"))->Promise.flatMap(() =>
          Handle__Goal.removeBoundaryAndDestroy(state, goal)
        )
      | NoParen =>
        // do nothing
        Handle__Goal.removeBoundaryAndDestroy(state, goal)
      | String(content) =>
        Handle__Goal.modify(state, goal, _ =>
          Js.String.replaceByRe(%re("/\\\\n/g"), "\n", content)
        )->Promise.flatMap(() => Handle__Goal.removeBoundaryAndDestroy(state, goal))
      }
    }
  | MakeCase(makeCaseType, lines) =>
    Handle__Goal.caseSimple(
      state,
      goal =>
        switch makeCaseType {
        | Function => Handle__Goal.replaceWithLines(state, goal, lines)
        | ExtendedLambda => Handle__Goal.replaceWithLambda(state, goal, lines)
        }->Promise.flatMap(() => dispatchCommand(Load)),
      displayOutOfGoalError(state),
    )
  | SolveAll(solutions) =>
    let solveOne = ((index, solution)): Promise.t<unit> => {
      let goals = state.goals->Array.keep(goal => goal.index == index)
      switch goals[0] {
      | None => Promise.resolved()
      | Some(goal) =>
        Handle__Goal.modify(state, goal, _ => solution)->Promise.flatMap(() =>
          sendAgdaRequest(Give(goal))
        )
      }
    }
    // solve them one by one
    solutions->Array.map(solveOne)->Util.oneByOne->Promise.flatMap(_ => {
      let size = Array.length(solutions)
      if size == 0 {
        display(state, Error("No solutions found"), Nothing)
      } else {
        display(state, Success(string_of_int(size) ++ " goals solved"), Nothing)
      }
    })

  | DisplayInfo(info) => DisplayInfo.handle(state, info)
  | RunningInfo(_verbosity, message) => display(state, Plain("Type-checking"), Plain(message))
  | _ => Promise.resolved()
  }
