open Belt
open Command

open! Task
// from Editor Command to Tasks
let rec dispatchCommand = (state: State.t, command): Promise.t<unit> => {
  let dispatchCommand = dispatchCommand(state)
  let sendAgdaRequest = Handle__Temp.sendAgdaRequest(dispatchCommand, state)
  let document = VSCode.TextEditor.document(state.editor)
  let header = View.Header.Plain(Command.toString(command))
  switch command {
  | Load => display(state, Plain("Loading ..."), Nothing)->Promise.flatMap(() => {
      // save the document before loading
      VSCode.TextDocument.save(document)
    })->Promise.flatMap(_ => {
      // Issue #26 - don't load the document in preview mode
      let options = Some(VSCode.TextDocumentShowOptions.make(~preview=false, ()))
      VSCode.Window.showTextDocumentWithShowOptions(document, options)->Promise.flatMap(_ =>
        sendAgdaRequest(Load)
      )
    })
  | Quit => Promise.resolved()
  | Restart => dispatchCommand(Load)
  | Refresh =>
    Handle__Goal.updateRanges(state)
    Handle__Decoration.refresh(state)
    Promise.resolved()
  | Compile => sendAgdaRequest(Compile)
  | ToggleDisplayOfImplicitArguments => sendAgdaRequest(ToggleDisplayOfImplicitArguments)
  | ShowConstraints => sendAgdaRequest(ShowConstraints)
  | SolveConstraints(normalization) =>
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(SolveConstraints(normalization, goal)),
      sendAgdaRequest(SolveConstraintsGlobal(normalization)),
    )
  | ShowGoals => sendAgdaRequest(ShowGoals)
  | NextGoal => Handle__Goal.next(state)
  | PreviousGoal => Handle__Goal.previous(state)
  | SearchAbout(normalization) =>
    prompt(state, header, {body: None, placeholder: Some("name:"), value: None}, expr =>
      sendAgdaRequest(SearchAbout(normalization, expr))
    )
  | Give =>
    Handle__Goal.case(
      state,
      (goal, _) => {sendAgdaRequest(Give(goal))},
      goal => prompt(state, header, {
          body: None,
          placeholder: Some("expression to give:"),
          value: None,
        }, expr =>
          Handle__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
            sendAgdaRequest(Give(goal))
          )
        ),
      displayOutOfGoalError(state),
    )
  | Refine =>
    Handle__Goal.caseSimple(
      state,
      goal => {sendAgdaRequest(Refine(goal))},
      displayOutOfGoalError(state),
    )
  | ElaborateAndGive(normalization) => {
      let placeholder = Some("expression to elaborate and give:")
      Handle__Goal.case(
        state,
        (goal, expr) => {sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))},
        goal => prompt(state, header, {
            body: None,
            placeholder: placeholder,
            value: None,
          }, expr => {sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))}),
        displayOutOfGoalError(state),
      )
    }
  | Auto =>
    Handle__Goal.caseSimple(
      state,
      goal => {sendAgdaRequest(Auto(goal))},
      displayOutOfGoalError(state),
    )
  | Case => {
      let placeholder = Some("variable to case split:")
      Handle__Goal.case(
        state,
        (goal, _) => {sendAgdaRequest(Case(goal))},
        goal => prompt(state, header, {
            body: Some("Please specify which variable you wish to split"),
            placeholder: placeholder,
            value: None,
          }, expr =>
            // place the queried expression in the goal
            Handle__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
              sendAgdaRequest(Case(goal))
            )
          ),
        displayOutOfGoalError(state),
      )
    }
  | HelperFunctionType(normalization) => {
      let placeholder = Some("expression:")
      Handle__Goal.case(
        state,
        (goal, expr) => sendAgdaRequest(HelperFunctionType(normalization, expr, goal)),
        goal => prompt(state, header, {
            body: None,
            placeholder: placeholder,
            value: None,
          }, expr => sendAgdaRequest(HelperFunctionType(normalization, expr, goal))),
        displayOutOfGoalError(state),
      )
    }
  | InferType(normalization) =>
    let placeholder = Some("expression to infer:")
    Handle__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(InferType(normalization, expr, goal)),
      goal => prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(InferType(normalization, expr, goal))),
      prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(InferTypeGlobal(normalization, expr))),
    )
  | Context(normalization) =>
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(Context(normalization, goal)),
      displayOutOfGoalError(state),
    )
  | GoalType(normalization) =>
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(GoalType(normalization, goal)),
      displayOutOfGoalError(state),
    )
  | GoalTypeAndContext(normalization) =>
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(GoalTypeAndContext(normalization, goal)),
      displayOutOfGoalError(state),
    )
  | GoalTypeContextAndInferredType(normalization) =>
    Handle__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal)),
      // fallback to `GoalTypeAndContext` when there's no content
      goal => sendAgdaRequest(GoalTypeAndContext(normalization, goal)),
      displayOutOfGoalError(state),
    )
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    state->Handle__Goal.case(
      (goal, expr) => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)),
      goal => prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal))),
      displayOutOfGoalError(state),
    )
  | ModuleContents(normalization) =>
    let placeholder = Some("module name:")
    Handle__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(ModuleContents(normalization, expr, goal)),
      goal => prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(ModuleContents(normalization, expr, goal))),
      prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ModuleContentsGlobal(normalization, expr))),
    )
  | ComputeNormalForm(computeMode) =>
    let placeholder = Some("expression to normalize:")
    Handle__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal)),
      goal => prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal))),
      prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ComputeNormalFormGlobal(computeMode, expr))),
    )
  | WhyInScope =>
    let placeholder = Some("name:")
    Handle__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(WhyInScope(expr, goal)),
      goal => prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(WhyInScope(expr, goal))),
      prompt(state, header, {
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
    | InputMethod(ChooseSymbol(symbol)) => Handle__InputMethod.chooseSymbol(state, symbol)
    | PromptIMUpdate(MouseSelect(interval)) => Handle__InputMethod.select(state, [interval])
    | PromptIMUpdate(KeyUpdate(input)) => Handle__InputMethod.keyUpdatePromptIM(state, input)
    | PromptIMUpdate(BrowseUp) => dispatchCommand(InputMethod(BrowseUp))
    | PromptIMUpdate(BrowseDown) => dispatchCommand(InputMethod(BrowseDown))
    | PromptIMUpdate(BrowseLeft) => dispatchCommand(InputMethod(BrowseLeft))
    | PromptIMUpdate(BrowseRight) => dispatchCommand(InputMethod(BrowseRight))
    | PromptIMUpdate(Escape) =>
      if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
        Handle__InputMethod.deactivate(state)
      } else {
        viewEvent(state, PromptInterrupt)
      }
    | JumpToTarget(link) =>
      let document = VSCode.TextEditor.document(state.editor)
      Editor.focus(document)
      let path = document->VSCode.TextDocument.fileName->Parser.filepath
      switch link {
      | ToRange(NoRange) => Promise.resolved()
      | ToRange(Range(None, _intervals)) => Promise.resolved()
      | ToRange(Range(Some(filePath), intervals)) =>
        // only select the intervals when it's on the same file
        if path == filePath {
          let ranges = intervals->Array.map(View__Controller.fromInterval)
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
      Handle__InputMethod.deactivate(state)
    } else {
      viewEvent(state, PromptInterrupt)
    }
  | InputMethod(Activate) => Handle__InputMethod.activateEditorIM(state)
  | InputMethod(InsertChar(char)) => Handle__InputMethod.insertChar(state, char)
  | InputMethod(BrowseUp) => Handle__InputMethod.moveUp(state)
  | InputMethod(BrowseDown) => Handle__InputMethod.moveDown(state)
  | InputMethod(BrowseLeft) => Handle__InputMethod.moveLeft(state)
  | InputMethod(BrowseRight) => Handle__InputMethod.moveRight(state)
  }
}
