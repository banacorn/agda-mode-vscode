open Command

// from Editor Command to Tasks
let rec dispatchCommand = async (state: State.t, command): unit => {
  state.channels.log->Chan.emit(CommandDispatched(command))
  let dispatchCommand = dispatchCommand(state, ...)
  let sendAgdaRequest = async request => {
    await State__Connection.sendRequest(
      state,
      State__Response.handle(state, dispatchCommand, ...),
      request
    )
    state.channels.log->Chan.emit(CommandHandled(command))
    state.channels.commandHandled->Chan.emit(command)
  }
  let header = View.Header.Plain(Command.toString(command))
  switch command {
  | Load =>
    await State__View.DebugBuffer.restore(state)
    await State__View.Panel.display(state, Plain("Loading ..."), [])
    // save the document before loading
    let _ = await VSCode.TextDocument.save(state.document)
    // Issue #26 - don't load the document in preview mode
    let options = Some(VSCode.TextDocumentShowOptions.make(~preview=false, ()))
    let _ = await VSCode.Window.showTextDocumentWithShowOptions(state.document, options)
    await sendAgdaRequest(Load)
  | Quit =>
    let _ = await State.destroy(state, true)
  | Restart =>
    // clear the RunningInfo log
    state.runningInfoLog = []
    await dispatchCommand(Load)
  | Refresh =>
    State__View.Panel.restore(state)
    Goals.redecorate(state.goals)
    // re-decorate the editor with the new decorations
    Tokens.removeDecorations(state.tokens, state.editor)
    Tokens.applyDecorations(state.tokens, state.editor)
    await State__View.DebugBuffer.restore(state)
  | Compile => await sendAgdaRequest(Compile)
  | ToggleDisplayOfImplicitArguments => await sendAgdaRequest(ToggleDisplayOfImplicitArguments)
  | ToggleDisplayOfIrrelevantArguments => await sendAgdaRequest(ToggleDisplayOfIrrelevantArguments)
  | ShowConstraints => await sendAgdaRequest(ShowConstraints)
  | SolveConstraints(normalization) =>
    switch state.goals->Goals.getGoalAtCursor(state.editor) {
    | None => await sendAgdaRequest(SolveConstraintsGlobal(normalization))
    | Some(goal) => await sendAgdaRequest(SolveConstraints(normalization, goal))
    }
  | ShowGoals(normalization) => await sendAgdaRequest(ShowGoals(normalization))
  | NextGoal =>
    state.goals->Goals.jumpToTheNextGoal(state.editor)
    state.channels.commandHandled->Chan.emit(NextGoal)
  | PreviousGoal =>
    state.goals->Goals.jumpToThePreviousGoal(state.editor)
    state.channels.commandHandled->Chan.emit(PreviousGoal)
  | SearchAbout(normalization) =>
    await State__View.Panel.prompt(
      state,
      header,
      {body: None, placeholder: Some("name:"), value: None},
      expr => sendAgdaRequest(SearchAbout(normalization, expr)),
    )
  | Give =>
    switch state.goals->Goals.getGoalAtCursor(state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) =>
      if Goal.getContent(goal, state.document) == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder: Some("expression to give:"),
            value: None,
          },
          async expr =>
            if expr == "" {
              await sendAgdaRequest(Give(goal))
            } else {
              await state.goals->Goals.modify(state.document, goal.index, _ => expr)
              await sendAgdaRequest(Give(goal))
            },
        )
      } else {
        await sendAgdaRequest(Give(goal))
      }
    }
  | Refine =>
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) =>
      state.isInRefineOperation = true
      await sendAgdaRequest(Refine(goal))
      state.isInRefineOperation = false
    }
  | ElaborateAndGive(normalization) => {
      let placeholder = Some("expression to elaborate and give:")
      switch Goals.getGoalAtCursor(state.goals, state.editor) {
      | None => await State__View.Panel.displayOutOfGoalError(state)
      | Some(goal) =>
        let expr = Goal.getContent(goal, state.document)
        if expr == "" {
          await State__View.Panel.prompt(
            state,
            header,
            {
              body: None,
              placeholder,
              value: None,
            },
            async expr =>
              if expr == "" {
                await sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))
              } else {
                await state.goals->Goals.modify(state.document, goal.index, _ => expr)
                await sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))
              },
          )
        } else {
          await sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))
        }
      }
    }
  | Auto(normalization) =>
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) => await sendAgdaRequest(Auto(normalization, goal))
    }
  | Case =>
    let placeholder = Some("variable(s) to case split:")
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) =>
      // remember that this goal is being case-split
      // because the information of this goal will not be available when handling the `MakeCase` response
      Goals.markAsCaseSplited(state.goals, goal)
      if Goal.getContent(goal, state.document) == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: Some(
              "Please specify which variable(s) you wish to split, multiple variables are delimited by whitespaces",
            ),
            placeholder,
            value: None,
          },
          async expr =>
            if expr == "" {
              await sendAgdaRequest(Case(goal))
            } else {
              // place the queried expression in the goal
              await state.goals->Goals.modify(state.document, goal.index, _ => expr)
              await sendAgdaRequest(Case(goal))
            },
        )
      } else {
        await sendAgdaRequest(Case(goal))
      }
    }

  | HelperFunctionType(normalization) => {
      let placeholder = Some("expression:")
      switch Goals.getGoalAtCursor(state.goals, state.editor) {
      | None => await State__View.Panel.displayOutOfGoalError(state)
      | Some(goal) =>
        let expr = Goal.getContent(goal, state.document)
        if expr == "" {
          await State__View.Panel.prompt(
            state,
            header,
            {
              body: None,
              placeholder,
              value: None,
            },
            expr => sendAgdaRequest(HelperFunctionType(normalization, expr, goal)),
          )
        } else {
          await sendAgdaRequest(HelperFunctionType(normalization, expr, goal))
        }
      }
    }
  | InferType(normalization) =>
    let placeholder = Some("expression to infer:")
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None =>
      await State__View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(InferTypeGlobal(normalization, expr)),
      )
    | Some(goal) =>
      let expr = Goal.getContent(goal, state.document)
      if expr == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder,
            value: None,
          },
          expr => sendAgdaRequest(InferType(normalization, expr, goal)),
        )
      } else {
        await sendAgdaRequest(InferType(normalization, expr, goal))
      }
    }
  | Context(normalization) =>
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) => await sendAgdaRequest(Context(normalization, goal))
    }
  | GoalType(normalization) =>
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) => await sendAgdaRequest(GoalType(normalization, goal))
    }
  | GoalTypeAndContext(normalization) =>
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) => await sendAgdaRequest(GoalTypeAndContext(normalization, goal))
    }
  | GoalTypeContextAndInferredType(normalization) =>
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) =>
      let expr = Goal.getContent(goal, state.document)
      if expr == "" {
        // fallback to `GoalTypeAndContext` when there's no payload
        await sendAgdaRequest(GoalTypeAndContext(normalization, goal))
      } else {
        await sendAgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal))
      }
    }
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None => await State__View.Panel.displayOutOfGoalError(state)
    | Some(goal) =>
      let expr = Goal.getContent(goal, state.document)
      if expr == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder,
            value: None,
          },
          expr => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)),
        )
      } else {
        await sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal))
      }
    }
  | ModuleContents(normalization) =>
    let placeholder = Some("module name:")
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None =>
      await State__View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(ModuleContentsGlobal(normalization, expr)),
      )
    | Some(goal) =>
      let expr = Goal.getContent(goal, state.document)
      if expr == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder,
            value: None,
          },
          expr => sendAgdaRequest(ModuleContents(normalization, expr, goal)),
        )
      } else {
        await sendAgdaRequest(ModuleContents(normalization, expr, goal))
      }
    }
  | ComputeNormalForm(computeMode) =>
    let placeholder = Some("expression to normalize:")
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None =>
      await State__View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(ComputeNormalFormGlobal(computeMode, expr)),
      )
    | Some(goal) =>
      let expr = Goal.getContent(goal, state.document)
      if expr == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder,
            value: None,
          },
          expr => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal)),
        )
      } else {
        await sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal))
      }
    }
  | WhyInScope =>
    let placeholder = Some("name:")
    switch Goals.getGoalAtCursor(state.goals, state.editor) {
    | None =>
      await State__View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(WhyInScopeGlobal(expr)),
      )
    | Some(goal) =>
      let expr = Goal.getContent(goal, state.document)
      if expr == "" {
        await State__View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder,
            value: None,
          },
          expr => sendAgdaRequest(WhyInScope(expr, goal)),
        )
      } else {
        await sendAgdaRequest(WhyInScope(expr, goal))
      }
    }
  | SwitchAgdaVersion => 
    await State__SwitchVersion.run(state, state.platformDeps)
  | EventFromView(event) =>
    switch event {
    | Initialized => ()
    | Destroyed =>
      switch await State.destroy(state, true) {
      | Error(error) =>
        let (errorHeader, errorBody) = Connection.Error.toString(error)
        let header = View.Header.Error("Cannot destruct the view: " ++ errorHeader)
        let body = [Item.plainText(errorBody)]
        await State__View.Panel.display(state, header, body)
      | Ok() => ()
      }
    | InputMethod(InsertChar(char)) => await dispatchCommand(InputMethod(InsertChar(char)))
    | InputMethod(ChooseSymbol(symbol)) => await State__InputMethod.chooseSymbol(state, symbol)
    | PromptIMUpdate(MouseSelect(interval)) => await State__InputMethod.select(state, [interval])
    | PromptIMUpdate(KeyUpdate(input)) => await State__InputMethod.keyUpdatePromptIM(state, input)
    | PromptIMUpdate(BrowseUp) => await dispatchCommand(InputMethod(BrowseUp))
    | PromptIMUpdate(BrowseDown) => await dispatchCommand(InputMethod(BrowseDown))
    | PromptIMUpdate(BrowseLeft) => await dispatchCommand(InputMethod(BrowseLeft))
    | PromptIMUpdate(BrowseRight) => await dispatchCommand(InputMethod(BrowseRight))
    | PromptIMUpdate(Escape) =>
      if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
        await State__InputMethod.deactivate(state)
      } else {
        await State__View.Panel.interruptPrompt(state)
      }
    | JumpToTarget(link) =>
      Editor.focus(state.document)
      let path = state.document->VSCode.TextDocument.fileName->Parser.filepath
      switch link {
      | SrcLoc(NoRange) => ()
      | SrcLoc(Range(None, _intervals)) => ()
      | SrcLoc(Range(Some(fileName), intervals)) =>
        let fileName = Parser.filepath(fileName)

        // only select the ranges when it's on the same file
        if Parser.filepath(path) == Parser.filepath(fileName) {
          let ranges = intervals->Array.map(Common.AgdaInterval.toVSCodeRange)
          // set cursor selections
          Editor.Selection.setMany(state.editor, ranges)
          // scroll to that part of the document
          ranges[0]->Option.forEach(range => {
            state.editor->VSCode.TextEditor.revealRange(range, None)
          })
        }
      | Hole(index) => Goals.setCursorByIndex(state.goals, state.editor, index)
      }
    }
  | Escape =>
    if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
      await State__InputMethod.deactivate(state)
    } else {
      await State__View.Panel.interruptPrompt(state)
    }
  | InputMethod(Activate) =>
    if Config.InputMethod.getEnabled() {
      await State__InputMethod.activateEditorIM(state)
    }

  | InputMethod(InsertChar(char)) => await State__InputMethod.insertChar(state, char)
  | InputMethod(BrowseUp) => await State__InputMethod.moveUp(state)
  | InputMethod(BrowseDown) => await State__InputMethod.moveDown(state)
  | InputMethod(BrowseLeft) => await State__InputMethod.moveLeft(state)
  | InputMethod(BrowseRight) => await State__InputMethod.moveRight(state)
  | LookupSymbol =>
    // get the selected text
    // query the user instead if no text is selected
    let (promise, resolve, _) = Util.Promise_.pending()
    let selectedText =
      Editor.Text.get(state.document, Editor.Selection.get(state.editor))->String.trim
    if selectedText == "" {
      State__View.Panel.prompt(
        state,
        View.Header.Plain("Lookup Unicode Symbol Input Sequence"),
        {body: None, placeholder: Some("symbol to lookup:"), value: None},
        input => {
          resolve(String.trim(input))
          Promise.resolve()
        },
      )->ignore
    } else {
      resolve(selectedText)
    }

    // lookup and display
    let input = await promise
    let sequences = Translator.lookup(input)->Option.getOr([])
    if Array.length(sequences) == 0 {
      await State__View.Panel.display(
        state,
        View.Header.Warning("No Input Sequences Found for \"" ++ selectedText ++ "\""),
        [],
      )
    } else {
      await State__View.Panel.display(
        state,
        View.Header.Success(
          string_of_int(Array.length(sequences)) ++
          " Input Sequences Found for \"" ++
          selectedText ++ "\"",
        ),
        sequences->Array.map(sequence => Item.plainText(sequence)),
      )
    }
  | OpenDebugBuffer =>
    State__View.DebugBuffer.make(state)->ignore
    await State__View.DebugBuffer.reveal(state)
  }
}
