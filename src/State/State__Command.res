open Belt
open Command

// from Editor Command to Tasks
let rec dispatchCommand = (state: State.t, command): Promise.t<unit> => {
  let dispatchCommand = dispatchCommand(state)
  let sendAgdaRequest = State.Connection.sendRequest(
    state,
    State__Response.handle(state, dispatchCommand),
  )
  let document = VSCode.TextEditor.document(state.editor)
  let header = View.Header.Plain(Command.toString(command))
  switch command {
  | Load =>
    State.View.display(state, Plain("Loading ..."), Nothing)
    ->Promise.flatMap(() => {
      // save the document before loading
      VSCode.TextDocument.save(document)
    })
    ->Promise.flatMap(_ => {
      // Issue #26 - don't load the document in preview mode
      let options = Some(VSCode.TextDocumentShowOptions.make(~preview=false, ()))
      VSCode.Window.showTextDocumentWithShowOptions(document, options)->Promise.flatMap(_ =>
        sendAgdaRequest(Load)
      )
    })
  | Quit => Promise.resolved()
  | Restart => dispatchCommand(Load)
  | Refresh =>
    State__Goal.updateRanges(state)
    State.Decoration.refresh(state)
    Promise.resolved()
  | Compile => sendAgdaRequest(Compile)
  | ToggleDisplayOfImplicitArguments => sendAgdaRequest(ToggleDisplayOfImplicitArguments)
  | ShowConstraints => sendAgdaRequest(ShowConstraints)
  | SolveConstraints(normalization) =>
    State__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(SolveConstraints(normalization, goal)),
      sendAgdaRequest(SolveConstraintsGlobal(normalization)),
    )
  | ShowGoals => sendAgdaRequest(ShowGoals)
  | NextGoal => State__Goal.next(state)
  | PreviousGoal => State__Goal.previous(state)
  | SearchAbout(normalization) =>
    State.View.prompt(state, header, {body: None, placeholder: Some("name:"), value: None}, expr =>
      sendAgdaRequest(SearchAbout(normalization, expr))
    )
  | Give =>
    State__Goal.case(
      state,
      (goal, _) => {sendAgdaRequest(Give(goal))},
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: Some("expression to give:"),
          value: None,
        }, expr =>
          State__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
            sendAgdaRequest(Give(goal))
          )
        ),
      State.View.displayOutOfGoalError(state),
    )
  | Refine =>
    State__Goal.caseSimple(
      state,
      goal => {sendAgdaRequest(Refine(goal))},
      State.View.displayOutOfGoalError(state),
    )
  | ElaborateAndGive(normalization) => {
      let placeholder = Some("expression to elaborate and give:")
      State__Goal.case(
        state,
        (goal, expr) => {sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))},
        goal =>
          State.View.prompt(state, header, {
            body: None,
            placeholder: placeholder,
            value: None,
          }, expr => {sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))}),
        State.View.displayOutOfGoalError(state),
      )
    }
  | Auto =>
    State__Goal.caseSimple(
      state,
      goal => {sendAgdaRequest(Auto(goal))},
      State.View.displayOutOfGoalError(state),
    )
  | Case => {
      let placeholder = Some("variable to case split:")
      State__Goal.case(
        state,
        (goal, _) => {sendAgdaRequest(Case(goal))},
        goal =>
          State.View.prompt(state, header, {
            body: Some("Please specify which variable you wish to split"),
            placeholder: placeholder,
            value: None,
          }, expr =>
            // place the queried expression in the goal
            State__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
              sendAgdaRequest(Case(goal))
            )
          ),
        State.View.displayOutOfGoalError(state),
      )
    }
  | HelperFunctionType(normalization) => {
      let placeholder = Some("expression:")
      State__Goal.case(
        state,
        (goal, expr) => sendAgdaRequest(HelperFunctionType(normalization, expr, goal)),
        goal =>
          State.View.prompt(state, header, {
            body: None,
            placeholder: placeholder,
            value: None,
          }, expr => sendAgdaRequest(HelperFunctionType(normalization, expr, goal))),
        State.View.displayOutOfGoalError(state),
      )
    }
  | InferType(normalization) =>
    let placeholder = Some("expression to infer:")
    State__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(InferType(normalization, expr, goal)),
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(InferType(normalization, expr, goal))),
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(InferTypeGlobal(normalization, expr))),
    )
  | Context(normalization) =>
    State__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(Context(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalType(normalization) =>
    State__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(GoalType(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalTypeAndContext(normalization) =>
    State__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(GoalTypeAndContext(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalTypeContextAndInferredType(normalization) =>
    State__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal)),
      // fallback to `GoalTypeAndContext` when there's no content
      goal => sendAgdaRequest(GoalTypeAndContext(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    state->State__Goal.case(
      (goal, expr) => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)),
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal))),
      State.View.displayOutOfGoalError(state),
    )
  | ModuleContents(normalization) =>
    let placeholder = Some("module name:")
    State__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(ModuleContents(normalization, expr, goal)),
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(ModuleContents(normalization, expr, goal))),
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ModuleContentsGlobal(normalization, expr))),
    )
  | ComputeNormalForm(computeMode) =>
    let placeholder = Some("expression to normalize:")
    State__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal)),
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal))),
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ComputeNormalFormGlobal(computeMode, expr))),
    )
  | WhyInScope =>
    let placeholder = Some("name:")
    State__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(WhyInScope(expr, goal)),
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(WhyInScope(expr, goal))),
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(WhyInScopeGlobal(expr))),
    )
  | EventFromView(event) =>
    switch event {
    | Initialized => Promise.resolved()
    | Destroyed => State.destroy(state)
    | InputMethod(InsertChar(char)) => dispatchCommand(InputMethod(InsertChar(char)))
    | InputMethod(ChooseSymbol(symbol)) => State__InputMethod.chooseSymbol(state, symbol)
    | PromptIMUpdate(MouseSelect(interval)) => State__InputMethod.select(state, [interval])
    | PromptIMUpdate(KeyUpdate(input)) => State__InputMethod.keyUpdatePromptIM(state, input)
    | PromptIMUpdate(BrowseUp) => dispatchCommand(InputMethod(BrowseUp))
    | PromptIMUpdate(BrowseDown) => dispatchCommand(InputMethod(BrowseDown))
    | PromptIMUpdate(BrowseLeft) => dispatchCommand(InputMethod(BrowseLeft))
    | PromptIMUpdate(BrowseRight) => dispatchCommand(InputMethod(BrowseRight))
    | PromptIMUpdate(Escape) =>
      if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
        State__InputMethod.deactivate(state)
      } else {
        State.View.interruptPrompt(state)
      }
    | JumpToTarget(link) =>
      let document = VSCode.TextEditor.document(state.editor)
      Editor.focus(document)
      let path = document->VSCode.TextDocument.fileName->Parser.filepath
      switch link {
      | ToLocation(NoLocation) => Promise.resolved()
      | ToLocation(Location(None, _ranges)) => Promise.resolved()
      | ToLocation(Location(Some(filePath), ranges)) =>
        // only select the ranges when it's on the same file
        if path == filePath {
          let ranges = ranges->Array.map(Editor.Range.fromAgdaRange)
          Editor.Selection.setMany(state.editor, ranges)
        }
        Promise.resolved()
      | ToHole(index) =>
        let goal = Js.Array.find((goal: Goal.t) => goal.index == index, state.goals)
        switch goal {
        | None => ()
        | Some(goal) => Goal.setCursor(goal, state.editor)
        }
        Promise.resolved()
      }
    | MouseOver(_) => Promise.resolved()
    | MouseOut(_) => Promise.resolved()
    }
  | Escape =>
    if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
      State__InputMethod.deactivate(state)
    } else {
      State.View.interruptPrompt(state)
    }
  | InputMethod(Activate) => State__InputMethod.activateEditorIM(state)
  | InputMethod(InsertChar(char)) => State__InputMethod.insertChar(state, char)
  | InputMethod(BrowseUp) => State__InputMethod.moveUp(state)
  | InputMethod(BrowseDown) => State__InputMethod.moveDown(state)
  | InputMethod(BrowseLeft) => State__InputMethod.moveLeft(state)
  | InputMethod(BrowseRight) => State__InputMethod.moveRight(state)
  }
}
