// from Agda Response to Tasks
open Belt

let removeNewlines = string => string->Js.String2.split("\\n")->Belt.Array.joinWith("\n", x => x)

open Response
module DisplayInfo = {
  let handle = (state, x) =>
    switch x {
    | Response.DisplayInfo.Generic(header, body) =>
      State.View.Panel.display(state, Plain(header), body)
    | CompilationOk(body) =>
      State.View.Panel.display(state, Success("Compilation result"), [Item.plainText(body)])
    | CompilationOkLSP(warnings, errors) =>
      let message = [Item.plainText("The module was successfully compiled.")]
      let errors = errors->Array.map(raw => Item.error(RichText.string(raw), Some(raw)))
      let warnings = warnings->Array.map(raw => Item.warning(RichText.string(raw), Some(raw)))
      State.View.Panel.display(
        state,
        Success("Compilation result"),
        Array.concatMany([message, errors, warnings]),
      )
    | Constraints(None) => State.View.Panel.display(state, Plain("No Constraints"), [])
    | Constraints(Some(body)) =>
      let items = Emacs__Parser2.parseOutputs(body)
      State.View.Panel.display(state, Plain("Constraints"), items)
    | AllGoalsWarnings(header, "nil") => State.View.Panel.display(state, Success(header), [])
    | AllGoalsWarnings(header, body) =>
      let items = Emacs__Parser2.parseAllGoalsWarnings(header, body)
      State.View.Panel.display(state, Plain(header), items)
    | AllGoalsWarningsLSP(header, goals, metas, warnings, errors) =>
      let errors = errors->Array.map(raw => Item.error(RichText.string(raw), Some(raw)))
      let warnings = warnings->Array.map(raw => Item.warning(RichText.string(raw), Some(raw)))
      State.View.Panel.display(
        state,
        Plain(header),
        Array.concatMany([goals, metas, errors, warnings]),
      )
    | Time(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.Panel.display(state, Plain("Time"), items)
    | Error(body) =>
      let items = Emacs__Parser2.parseError(body)
      State.View.Panel.display(state, Error("Error"), items)
    | Intro(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.Panel.display(state, Plain("Intro"), items)
    | Auto(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.Panel.display(state, Plain("Auto"), items)
    | ModuleContents(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.Panel.display(state, Plain("Module Contents"), items)
    | SearchAbout(body) =>
      let items = Emacs__Parser2.parseSearchAbout(body)
      State.View.Panel.display(state, Plain("Search About"), items)
    | WhyInScope(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.Panel.display(state, Plain("Scope info"), items)
    | NormalForm(body) =>
      let items = Emacs__Parser2.parseTextWithLocation(body)
      State.View.Panel.display(state, Plain("Normal form"), items)
    | GoalType(body) =>
      let items = Emacs__Parser2.parseGoalType(body)
      State.View.Panel.display(state, Plain("Goal and Context"), items)
    | CurrentGoalLSP(item) => State.View.Panel.display(state, Plain("Current Goal"), [item])
    | CurrentGoal(payload) =>
      State.View.Panel.display(state, Plain("Current Goal"), [Item.plainText(payload)])
    | InferredType(payload) =>
      State.View.Panel.display(state, Plain("Inferred type"), [Item.plainText(payload)])
    | InferredTypeLSP(item) => State.View.Panel.display(state, Plain("Inferred type"), [item])
    | Context(body) =>
      let items = Emacs__Parser2.parseOutputs(body)
      State.View.Panel.display(state, Plain("Context"), items)
    | HelperFunction(payload) =>
      VSCode.Env.clipboard
      ->VSCode.Clipboard.writeText(payload)
      ->Promise.flatMap(() =>
        State.View.Panel.display(
          state,
          Plain("Helper function (copied to clipboard)"),
          [Item.plainText(payload)],
        )
      )
    | Version(payload) =>
      State.View.Panel.display(state, Plain("Version"), [Item.plainText(payload)])
    }
}

let rec handle = (
  state: State.t,
  dispatchCommand: Command.t => Promise.t<unit>,
  response: Response.t,
): Promise.t<unit> => {
  // pipe Response for testing
  state.channels.response->Chan.emit(response)

  let sendAgdaRequest = State.sendRequest(state, handle(state, dispatchCommand))
  switch response {
  | HighlightingInfoDirect(_keep, annotations) =>
    state.tokens->Tokens.insert(state.editor, annotations)
    Promise.resolved()
  | HighlightingInfoIndirect(filepath) =>
    state.tokens->Tokens.addEmacsFilePath(filepath)
    Promise.resolved()
  | HighlightingInfoIndirectJSON(filepath) =>
    state.tokens->Tokens.addJSONFilePath(filepath)
    Promise.resolved()
  | ClearHighlighting =>
    state.tokens->Tokens.clear
    state.highlighting->Highlighting.clear
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
      State.View.Panel.display(
        state,
        Error("Error: Give failed"),
        [Item.plainText("Cannot find goal #" ++ string_of_int(index))],
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
    | None => State.View.Panel.displayOutOfGoalError(state)
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
        State.View.Panel.display(state, Error("No solutions found"), [])
      } else {
        State.View.Panel.display(state, Success(string_of_int(size) ++ " goals solved"), [])
      }
    })

  | DisplayInfo(info) => DisplayInfo.handle(state, info)
  | RunningInfo(1, message) =>
    let message = removeNewlines(message)
    state.runningInfoLog->Js.Array2.push((1, message))->ignore
    State.View.Panel.displayInAppendMode(
      state,
      Plain("Type-checking"),
      [Item.plainText(message)],
    )->Promise.flatMap(() => State.View.DebugBuffer.displayInAppendMode([(1, message)]))
  | RunningInfo(verbosity, message) =>
    let message = removeNewlines(message)
    state.runningInfoLog->Js.Array2.push((verbosity, message))->ignore
    State.View.DebugBuffer.displayInAppendMode([(verbosity, message)])
  | CompleteHighlightingAndMakePromptReappear =>
    // apply decoration before handling Last Responses
    Highlighting.apply(state.highlighting, state.tokens, state.editor)
  | _ => Promise.resolved()
  }
}
