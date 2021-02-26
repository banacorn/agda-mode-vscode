// from Agda Response to Tasks
open Belt

open Response
module DisplayInfo = {
  let handle = (state, x) =>
    switch x {
    | Response.DisplayInfo.CompilationOk(body) =>
      State.View.display(state, Success("Compilation result"), [Component.Item.plainText(body)])
    | CompilationOkLSP(warnings, errors) =>
      let message = [Component.Item.plainText("The module was successfully compiled.")]
      let errors =
        errors->Array.map(raw => Component.Item.error(Component.Text.plainText(raw), Some(raw)))
      let warnings =
        warnings->Array.map(raw => Component.Item.warning(Component.Text.plainText(raw), Some(raw)))
      State.View.display(
        state,
        Success("Compilation result"),
        Array.concatMany([message, errors, warnings]),
      )
    | Constraints(None) => State.View.display(state, Plain("No Constraints"), [])
    | Constraints(Some(body)) =>
      let items = Emacs__Parser2.parseOutputs(body)
      State.View.display(state, Plain("Constraints"), items)
    | AllGoalsWarnings(header, "nil") => State.View.display(state, Success(header), [])
    | AllGoalsWarnings(header, body) =>
      let items = Emacs__Parser2.parseAllGoalsWarnings(header, body)
      State.View.display(state, Plain(header), items)
    | AllGoalsWarningsLSP(header, goals, metas, warnings, errors) =>
      let goals =
        goals
        ->Array.map(((oc, raw)) => [
          Component.Item.Unlabeled(
            Agda.OutputConstraint.toText(
              Agda.InteractionId.toText,
              Agda.InteractionId.render,
              oc,
              None,
            ),
            Some(raw),
          ),
        ])
        ->Array.concatMany
      let metas =
        metas
        ->Array.map(((oc, raw, range)) => [
          Component.Item.Unlabeled(
            Agda.OutputConstraint.toText(Agda.NamedMeta.toText, Agda.NamedMeta.render, oc, None),
            Some(raw),
          ),
        ])
        ->Array.concatMany
      let errors =
        errors->Array.map(raw => Component.Item.error(Component.Text.plainText(raw), Some(raw)))
      let warnings =
        warnings->Array.map(raw => Component.Item.warning(Component.Text.plainText(raw), Some(raw)))
      State.View.display(state, Plain(header), Array.concatMany([goals, metas, errors, warnings]))
    | Time(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.display(state, Plain("Time"), items)
    | Error(body) =>
      let items = Emacs__Parser2.parseError(body)
      State.View.display(state, Error("Error"), items)
    | Intro(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.display(state, Plain("Intro"), items)
    | Auto(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.display(state, Plain("Auto"), items)
    | ModuleContents(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.display(state, Plain("Module Contents"), items)
    | SearchAbout(body) =>
      let items = Emacs__Parser2.parseSearchAbout(body)
      State.View.display(state, Plain("Search About"), items)
    | WhyInScope(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.display(state, Plain("Scope info"), items)
    | NormalForm(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.display(state, Plain("Normal form"), items)
    | GoalType(body) =>
      let items = Emacs__Parser2.parseGoalType(body)
      State.View.display(state, Plain("Goal and Context"), items)
    | CurrentGoal(payload) =>
      State.View.display(state, Plain("Current goal"), [Component.Item.plainText(payload)])
    | InferredType(payload) =>
      State.View.display(state, Plain("Inferred type"), [Component.Item.plainText(payload)])
    | Context(body) =>
      let items = Emacs__Parser2.parseOutputs(body)
      State.View.display(state, Plain("Context"), items)
    | HelperFunction(payload) =>
      VSCode.Env.clipboard
      ->VSCode.Clipboard.writeText(payload)
      ->Promise.flatMap(() =>
        State.View.display(
          state,
          Plain("Helper function (copied to clipboard)"),
          [Component.Item.plainText(payload)],
        )
      )
    | Version(payload) =>
      State.View.display(state, Plain("Version"), [Component.Item.plainText(payload)])
    }
}

let rec handle = (
  state: State.t,
  dispatchCommand: Command.t => Promise.t<unit>,
  response: Response.t,
): Promise.t<unit> => {
  let sendAgdaRequest = State.Connection.sendRequest(state, handle(state, dispatchCommand))
  switch response {
  | HighlightingInfoDirect(_keep, annotations) =>
    state.decoration->Decoration.addViaPipe(annotations)
    Promise.resolved()
  | HighlightingInfoIndirect(filepath) =>
    state.decoration->Decoration.addViaFile(filepath)
    Promise.resolved()
  | HighlightingInfoIndirectJSON(filepath) =>
    state.decoration->Decoration.addViaJSONFile(filepath)
    Promise.resolved()
  | ClearHighlighting =>
    state.decoration->Decoration.clear
    Promise.resolved()
  | Status(_checked, _displayImplicit) =>
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
        [Component.Item.plainText("Cannot find goal #" ++ string_of_int(index))],
      )
    | Some(goal) =>
      switch give {
      | GiveParen =>
        State__Goal.modify(state, goal, content => "(" ++ (content ++ ")"))->Promise.flatMap(() =>
          State__Goal.removeBoundaryAndDestroy(state, goal)
        )
      | GiveNoParen =>
        // do nothing
        State__Goal.removeBoundaryAndDestroy(state, goal)
      | GiveString(content) =>
        State__Goal.modify(state, goal, _ =>
          Js.String.replaceByRe(%re("/\\\\n/g"), "\n", content)
        )->Promise.flatMap(() => State__Goal.removeBoundaryAndDestroy(state, goal))
      }
    }
  | MakeCase(makeCaseType, lines) =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
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
        State.View.display(state, Error("No solutions found"), [])
      } else {
        State.View.display(state, Success(string_of_int(size) ++ " goals solved"), [])
      }
    })

  | DisplayInfo(info) => DisplayInfo.handle(state, info)
  | RunningInfo(_verbosity, message) =>
    State.View.display(state, Plain("Type-checking"), [Component.Item.plainText(message)])
  | CompleteHighlightingAndMakePromptReappear =>
    // apply decoration before handling Last Responses
    Decoration.apply(state.decoration, state.editor)
  | _ => Promise.resolved()
  }
}
