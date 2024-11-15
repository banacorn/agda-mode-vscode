// from Agda Response to Tasks
let canonicalizeEscape = content => content->String.replaceRegExp(%re("/\\n|\\r\\n/g"), "\n")
let insertBeforeNewline = (prefix, content) =>
  content->String.replaceRegExp(%re("/\n/g"), "\n" ++ prefix)
let removeNewlines = string => string->String.split("\n")->Array.join("\n")

open Response
module DisplayInfo = {
  let handle = async (state, x) =>
    switch x {
    | Response.DisplayInfo.Generic(header, body) =>
      await State.View.Panel.display(state, Plain(header), body)
    | CompilationOk(body) =>
      await State.View.Panel.display(state, Success("Compilation result"), [Item.plainText(body)])
    | CompilationOkLSP(warnings, errors) =>
      let message = [Item.plainText("The module was successfully compiled.")]
      let errors = errors->Array.map(raw => Item.error(RichText.string(raw), Some(raw)))
      let warnings = warnings->Array.map(raw => Item.warning(RichText.string(raw), Some(raw)))
      await State.View.Panel.display(
        state,
        Success("Compilation result"),
        Array.flat([message, errors, warnings]),
      )
    | Constraints(None) => await State.View.Panel.display(state, Plain("No Constraints"), [])
    | Constraints(Some(body)) =>
      let items = Emacs__Parser2.parseOutputs(body)
      await State.View.Panel.display(state, Plain("Constraints"), items)
    | AllGoalsWarnings(header, "nil") => await State.View.Panel.display(state, Success(header), [])
    | AllGoalsWarnings(header, body) =>
      let items = Emacs__Parser2.parseAllGoalsWarnings(header, body)->Emacs__Parser2.render
      await State.View.Panel.display(state, Plain(header), items)
    | AllGoalsWarningsLSP(header, goals, metas, warnings, errors) =>
      let errors = errors->Array.map(raw => Item.error(RichText.string(raw), Some(raw)))
      let warnings = warnings->Array.map(raw => Item.warning(RichText.string(raw), Some(raw)))
      await State.View.Panel.display(
        state,
        Plain(header),
        Array.flat([goals, metas, errors, warnings]),
      )
    | Time(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State.View.Panel.display(state, Plain("Time"), items)
    | Error(body) =>
      let items = Emacs__Parser2.parseError(body)->Emacs__Parser2.render
      await State.View.Panel.display(state, Error("Error"), items)
    | Intro(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State.View.Panel.display(state, Plain("Intro"), items)
    | Auto(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State.View.Panel.display(state, Plain("Auto"), items)
    | ModuleContents(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State.View.Panel.display(state, Plain("Module Contents"), items)
    | SearchAbout(body) =>
      let items = Emacs__Parser2.parseAndRenderSearchAbout(body)
      await State.View.Panel.display(state, Plain("Search About"), items)
    | WhyInScope(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State.View.Panel.display(state, Plain("Scope info"), items)
    | NormalForm(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State.View.Panel.display(state, Plain("Normal form"), items)
    | GoalType(body) =>
      let items = Emacs__Parser2.parseGoalType(body)->Emacs__Parser2.render
      await State.View.Panel.display(state, Plain("Goal and Context"), items)
    | CurrentGoalLSP(item) => await State.View.Panel.display(state, Plain("Current Goal"), [item])
    | CurrentGoal(payload) =>
      await State.View.Panel.display(state, Plain("Current Goal"), [Item.plainText(payload)])
    | InferredType(payload) =>
      await State.View.Panel.display(state, Plain("Inferred type"), [Item.plainText(payload)])
    | InferredTypeLSP(item) => await State.View.Panel.display(state, Plain("Inferred type"), [item])
    | Context(body) =>
      let items = Emacs__Parser2.parseOutputs(body)
      await State.View.Panel.display(state, Plain("Context"), items)
    | HelperFunction(payload) =>
      await VSCode.Env.clipboard->VSCode.Clipboard.writeText(payload)
      await State.View.Panel.display(
        state,
        Plain("Helper function (copied to clipboard)"),
        [Item.plainText(payload)],
      )
    | Version(payload) =>
      await State.View.Panel.display(state, Plain("Version"), [Item.plainText(payload)])
    }
}

let rec handle = async (
  state: State.t,
  dispatchCommand: Command.t => promise<unit>,
  response: Response.t,
): unit => {
  let sendAgdaRequest = State.sendRequest(state, handle(state, dispatchCommand, ...), ...)
  let handleResponse = async () =>
    switch response {
    | HighlightingInfoDirect(_keep, annotations) =>
      state.tokens->Tokens.insert(state.editor, annotations)
    | HighlightingInfoIndirect(filepath) => state.tokens->Tokens.addEmacsFilePath(filepath)
    | HighlightingInfoIndirectJSON(filepath) => state.tokens->Tokens.addJSONFilePath(filepath)
    | ClearHighlighting =>
      state.tokens->Tokens.clear
      state.highlighting->Highlighting.clear
    | Status(_checked, _displayImplicit) => // display(
      //   "Status",
      //   Some(
      //     "Typechecked: "
      //     ++ string_of_bool(checked)
      //     ++ "\nDisplay implicit arguments: "
      //     ++ string_of_bool(displayImplicit),
      //   ),
      // ),
      ()

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
    | InteractionPoints(indices) => await State__Goal.instantiate(state, indices)
    | GiveAction(index, give) =>
      let found = state.goals->Array.filter(goal => goal.index == index)
      switch found[0] {
      | None =>
        await State.View.Panel.display(
          state,
          Error("Error: Give failed"),
          [Item.plainText("Cannot find goal #" ++ string_of_int(index))],
        )
      | Some(goal) =>
        switch give {
        | GiveParen =>
          await State__Goal.modify(state, goal, content => "(" ++ (content ++ ")"))
          await State__Goal.removeBoundaryAndDestroy(state, goal)
        | GiveNoParen =>
          // do nothing
          await State__Goal.removeBoundaryAndDestroy(state, goal)
        | GiveString(content) =>
          let (indent, _text, _) = State__Goal.indentationWidth(state.document, goal)
          // 1. ideally, we want to add a "\t" or equivalent spaces before the indent based on
          // "editor.tabSize" and "editor.insertSpaces"
          // but we cannot load the "editor.tabSize" here
          // so as a workaround we use a default value of 2
          // maybe consider storing these attributed in the state
          // 2. the Emacs plugin seems to use len(text) as the indent, which could be a
          // safet choice
          let defaultIndent = 2
          await State__Goal.modify(state, goal, _ =>
            insertBeforeNewline(
              String.repeat(" ", defaultIndent + indent),
              canonicalizeEscape(content),
            )
          )
          await State__Goal.removeBoundaryAndDestroy(state, goal)
        }
      }
    | MakeCase(makeCaseType, lines) =>
      switch State__Goal.pointed(state) {
      | None => await State.View.Panel.displayOutOfGoalError(state)
      | Some((goal, _)) =>
        switch makeCaseType {
        | Function => await State__Goal.replaceWithLines(state, goal, lines)
        | ExtendedLambda => await State__Goal.replaceWithLambda(state, goal, lines)
        }
        // dispatch `agda-mode:load` but do it asynchronously
        // so that we can finish let `agda-mode:case` finish first
        dispatchCommand(Load)->ignore
      }
    | SolveAll(solutions) =>
      let solveOne = async ((index, solution)) => {
        let goals = state.goals->Array.filter(goal => goal.index == index)
        switch goals[0] {
        | None => ()
        | Some(goal) =>
          await State__Goal.modify(state, goal, _ => solution)
          await sendAgdaRequest(Give(goal))
        }
      }
      // solve them one by one
      let _ =
        await solutions
        ->Array.map(solveOne)
        ->Util.oneByOne
      let size = Array.length(solutions)
      if size == 0 {
        await State.View.Panel.display(state, Error("No solutions found"), [])
      } else {
        await State.View.Panel.display(state, Success(string_of_int(size) ++ " goals solved"), [])
      }
    | DisplayInfo(info) => await DisplayInfo.handle(state, info)
    | RunningInfo(1, message) =>
      Js.log(
        "Type-checking:\n    " ++
        message ++
        "\n => " ++
        removeNewlines(canonicalizeEscape(message)),
      )
      let message = removeNewlines(canonicalizeEscape(message))
      await State.View.Panel.displayInAppendMode(
        state,
        Plain("Type-checking"),
        [Item.plainText(message)],
      )
    | RunningInfo(verbosity, message) =>
      let message = removeNewlines(canonicalizeEscape(message))
      state.runningInfoLog->Array.push((verbosity, message))->ignore
      await State.View.DebugBuffer.displayInAppendMode([(verbosity, message)])
    | CompleteHighlightingAndMakePromptReappear =>
      // apply decoration before handling Last Responses
      await Tokens.readTempFiles(state.tokens, state.editor)
      await Highlighting.apply(state.highlighting, state.tokens, state.editor)
    | _ => ()
    }

  await handleResponse()
  // emit Response when it's been handled
  state.channels.responseHandled->Chan.emit(response)
}
