// from Agda Response to Tasks

// adds indentation to a multiline string
let indent = (content, indent) => {
  let indentation = String.repeat(" ", indent)
  content->String.replaceRegExp(%re("/\n/g"), "\n" ++ indentation) // should also handles CR LF on Windows
}

open Response
module DisplayInfo = {
  let handle = async (state, x) =>
    switch x {
    | Response.DisplayInfo.Generic(header, body) =>
      await State__View.Panel.display(state, Plain(header), body)
    | CompilationOk(body) =>
      await State__View.Panel.display(state, Success("Compilation result"), [Item.plainText(body)])
    | CompilationOkALS(warnings, errors) =>
      let message = [Item.plainText("The module was successfully compiled.")]
      let errors = errors->Array.map(raw => Item.error(RichText.string(raw), Some(raw)))
      let warnings = warnings->Array.map(raw => Item.warning(RichText.string(raw), Some(raw)))
      await State__View.Panel.display(
        state,
        Success("Compilation result"),
        Array.flat([message, errors, warnings]),
      )
    | Constraints(None) => await State__View.Panel.display(state, Plain("No Constraints"), [])
    | Constraints(Some(body)) =>
      let items = Emacs__Parser2.parseOutputs(body)
      await State__View.Panel.display(state, Plain("Constraints"), items)
    | AllGoalsWarnings(header, "nil") => await State__View.Panel.display(state, Success(header), [])
    | AllGoalsWarnings(header, body) =>
      let items = Emacs__Parser2.parseAllGoalsWarnings(header, body)->Emacs__Parser2.render
      await State__View.Panel.display(state, Plain(header), items)
    | AllGoalsWarningsALS(header, goals, metas, warnings, errors) =>
      let errors = errors->Array.map(raw => Item.error(RichText.string(raw), Some(raw)))
      let warnings = warnings->Array.map(raw => Item.warning(RichText.string(raw), Some(raw)))
      await State__View.Panel.display(
        state,
        Plain(header),
        Array.flat([goals, metas, errors, warnings]),
      )
    | Time(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State__View.Panel.display(state, Plain("Time"), items)
    | Error(body) =>
      let items = Emacs__Parser2.parseError(body)->Emacs__Parser2.render
      await State__View.Panel.display(state, Error("Error"), items)
    | Intro(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State__View.Panel.display(state, Plain("Intro"), items)
    | Auto(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State__View.Panel.display(state, Plain("Auto"), items)
    | ModuleContents(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State__View.Panel.display(state, Plain("Module Contents"), items)
    | SearchAbout(body) =>
      let items = Emacs__Parser2.parseAndRenderSearchAbout(body)
      await State__View.Panel.display(state, Plain("Search About"), items)
    | WhyInScope(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State__View.Panel.display(state, Plain("Scope info"), items)
    | NormalForm(body) =>
      let items = Emacs__Parser2.parseAndRenderTextWithLocation(body)
      await State__View.Panel.display(state, Plain("Normal form"), items)
    | GoalType(body) =>
      let items = Emacs__Parser2.parseGoalType(body)->Emacs__Parser2.render
      await State__View.Panel.display(state, Plain("Goal and Context"), items)
    | CurrentGoalALS(item) => await State__View.Panel.display(state, Plain("Current Goal"), [item])
    | CurrentGoal(payload) =>
      await State__View.Panel.display(state, Plain("Current Goal"), [Item.plainText(payload)])
    | InferredType(payload) =>
      await State__View.Panel.display(state, Plain("Inferred type"), [Item.plainText(payload)])
    | InferredTypeALS(item) =>
      await State__View.Panel.display(state, Plain("Inferred type"), [item])
    | Context(body) =>
      let items = Emacs__Parser2.parseOutputs(body)
      await State__View.Panel.display(state, Plain("Context"), items)
    | HelperFunction(payload) =>
      await VSCode.Env.clipboard->VSCode.Clipboard.writeText(payload)
      await State__View.Panel.display(
        state,
        Plain("Helper function (copied to clipboard)"),
        [Item.plainText(payload)],
      )
    | Version(payload) =>
      await State__View.Panel.display(state, Plain("Version"), [Item.plainText(payload)])
    }
}

let rec handle = async (
  state: State.t,
  dispatchCommand: Command.t => promise<unit>,
  response: Response.t,
): unit => {
  let sendAgdaRequest =
    State__Connection.sendRequest(state, handle(state, dispatchCommand, ...), ...)
  let handleResponse = async () =>
    switch response {
    | HighlightingInfoDirect(_keep, annotations) =>
      if !state.isInRefineOrGiveOperation {
        state.tokens->Tokens.insertTokens(state.editor, annotations)
      }
    | HighlightingInfoIndirect(filepath) =>
      if !state.isInRefineOrGiveOperation {
        state.tokens->Tokens.addEmacsFilePath(filepath)
      }
    | HighlightingInfoIndirectJSON(filepath) =>
      if !state.isInRefineOrGiveOperation {
        state.tokens->Tokens.addJSONFilePath(filepath)
      }
    | ClearHighlighting => state.tokens->Tokens.reset
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
        let text = Editor.Text.getAll(state.document)
        let converter = Agda.OffsetConverter.make(text)
        let offset_ = Agda.OffsetConverter.convert(converter, offset - 1)
        let point = state.document->VSCode.TextDocument.positionAt(offset_)
        Editor.Cursor.set(state.editor, point)
      }
    | InteractionPoints(indices) =>
      let holePositions = await state.tokens->Tokens.getHolePositionsFromLoad->Resource.get
      let positionsArray = Map.entries(holePositions)->Iterator.toArray

      state.goals->Goals.addGoalPositions(positionsArray)
      if state.isInRefineOrGiveOperation {
        await state.goals->Goals.resetGoalIndicesOnRefineOrGive(state.editor, indices)
      } else {
        await state.goals->Goals.resetGoalIndicesOnLoad(state.editor, indices)
      }
    | GiveAction(index, give) =>
      switch Goals.getGoalByIndex(state.goals, index) {
      | None =>
        await State__View.Panel.display(
          state,
          Error("Error: Give failed"),
          [Item.plainText("Cannot find goal #" ++ string_of_int(index))],
        )
      | Some(goal) =>
        switch give {
        | GiveParen =>
          // add goal positions - get content before modifying
          let goalContent = Goal.getContent(goal, state.document)
          let goalPositionsRelative = Goals.parseGoalPositionsFromRefine(goalContent)
          let goalPositionsAbsolute = switch Goals.getGoalPositionByIndex(state.goals, index) {
          | None => [] // should not happen
          | Some((offset, _)) =>
            // adjust for the opening parenthesis "(" which shifts positions by 1
            goalPositionsRelative->Array.map(((start, end)) => (
              start + offset + 1,
              end + offset + 1,
            ))
          }
          state.goals->Goals.addGoalPositions(goalPositionsAbsolute)

          await state.goals->Goals.modify(state.document, index, content => "(" ++ content ++ ")")
        | GiveNoParen =>
          // add goal positions
          let goalContent = Goal.getContent(goal, state.document)
          let goalPositionsRelative = Goals.parseGoalPositionsFromRefine(goalContent)
          let goalPositionsAbsolute = switch Goals.getGoalPositionByIndex(state.goals, index) {
          | None => [] // should not happen
          | Some((offset, _)) =>
            goalPositionsRelative->Array.map(((start, end)) => (start + offset, end + offset))
          }
          state.goals->Goals.addGoalPositions(goalPositionsAbsolute)
        | GiveString(content) =>
          let (indentationWidth, _text, _) = Goal.indentationWidth(goal, state.document)
          // 1. ideally, we want to add "\t" or equivalent spaces based on
          //    "editor.tabSize" and "editor.insertSpaces"
          //    but we cannot load the "editor.tabSize" here
          //    so as a workaround, we use a default value of 2
          //    maybe consider storing these attributes in the state in the future
          // 2. the Emacs plugin seems to use len(text) as the indent, which could be a
          //    safer choice
          let defaultIndentation = 2
          let indented = Parser.unescapeEOL(content)->indent(defaultIndentation + indentationWidth)

          // modify the document
          await state.goals->Goals.modify(state.document, index, _ => indented)

          // add goal positions
          let goalPositionsRelative = Goals.parseGoalPositionsFromRefine(indented)
          let goalPositionsAbsolute = switch Goals.getGoalPositionByIndex(state.goals, index) {
          | None => [] // should not happen
          | Some((offset, _)) =>
            goalPositionsRelative->Array.map(((start, end)) => (start + offset, end + offset))
          }
          state.goals->Goals.addGoalPositions(goalPositionsAbsolute)
        }

        if await Goals.removeBoundaryAndDestroy(state.goals, state.document, index) {
          ()
        } else {
          await State__View.Panel.display(
            state,
            Error("Goal-related Error"),
            [Item.plainText("Unable to remove the boundary of goal #" ++ string_of_int(index))],
          )
        }
      }
    | MakeCase(makeCaseType, lines) =>
      // the information about the goal being split is not available at this point of time
      switch state.goals->Goals.getRecentlyCaseSplited {
      | None =>
        await State__View.Panel.display(
          state,
          Error("Cannot split the goal"),
          [Item.plainText("Failed to remember the goal being split")],
        )
      | Some(goal) =>
        let result = switch makeCaseType {
        | Function => await Goal.replaceWithLines(goal, state.document, lines)
        | ExtendedLambda => await Goal.replaceWithLambda(goal, state.document, lines)
        }

        switch result {
        | Some(rangeToBeReplaced, indentedLines) =>
          // destroy the old goal
          Goals.removeGoalByIndex(state.goals, goal.index)
          // locate the first new goal and place the cursor there
          Goal.placeCursorAtFirstNewGoal(state.editor, rangeToBeReplaced, indentedLines)
        | None =>
          await State__View.Panel.display(
            state,
            Error("Goal-related Error"),
            [Item.plainText("Unable to replace the lines of goal #" ++ string_of_int(goal.index))],
          )
        }
        await dispatchCommand(Load)
      }
    | SolveAll(solutions) =>
      let solveOne = ((index, solution)) => async () => {
        switch Goals.getGoalByIndex(state.goals, index) {
        | None => ()
        | Some(goal) =>
          // modify the goal content
          await Goals.modify(state.goals, state.document, index, _ => solution)
          // send the give request to Agda
          await sendAgdaRequest(Give(goal))
        }
      }
      // solve them one by one
      let _ =
        await solutions
        ->Array.map(solveOne)
        ->Util.Promise_.oneByOne
      let size = Array.length(solutions)
      if size == 0 {
        await State__View.Panel.display(state, Error("No solutions found"), [])
      } else {
        await State__View.Panel.display(state, Success(string_of_int(size) ++ " goals solved"), [])
      }
    // let solveOne = ((index, solution)) => async () => {
    //   let goal = state.goals->Array.find(goal => goal.index == index)
    //   switch goal {
    //   | None => ()
    //   | Some(goal) =>
    //     await State__Goal.modify(state, goal, _ => solution)
    //     await sendAgdaRequest(Give(goal))
    //   }
    // }
    // // solve them one by one
    // let _ =
    //   await solutions
    //   ->Array.map(solveOne)
    //   ->Util.Promise_.oneByOne
    // let size = Array.length(solutions)
    // if size == 0 {
    //   await State__View.Panel.display(state, Error("No solutions found"), [])
    // } else {
    //   await State__View.Panel.display(state, Success(string_of_int(size) ++ " goals solved"), [])
    // }
    | DisplayInfo(info) => await DisplayInfo.handle(state, info)
    | RunningInfo(1, message) =>
      await State__View.Panel.displayInAppendMode(
        state,
        Plain("Type-checking"),
        [Item.plainText(message)],
      )
    | RunningInfo(verbosity, message) =>
      state.runningInfoLog->Array.push((verbosity, message))->ignore
      await State__View.DebugBuffer.displayInAppendMode([(verbosity, message)])
    | CompleteHighlightingAndMakePromptReappear =>
      // apply decoration before handling Last Responses
      await Tokens.readTempFiles(state.tokens, state.editor)
      // generate highlighting
      state.tokens->Tokens.generateHighlighting(state.editor)
    | _ => ()
    }

  await handleResponse()
  // emit Response when it's been handled
  state.channels.responseHandled->Chan.emit(response)
}
