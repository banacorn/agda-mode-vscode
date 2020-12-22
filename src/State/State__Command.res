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
    switch State__Goal.pointed(state) {
    | None => sendAgdaRequest(SolveConstraintsGlobal(normalization))
    | Some((goal, _)) => sendAgdaRequest(SolveConstraints(normalization, goal))
    }
  | ShowGoals => sendAgdaRequest(ShowGoals)
  | NextGoal => State__Goal.next(state)
  | PreviousGoal => State__Goal.previous(state)
  | SearchAbout(normalization) =>
    State.View.prompt(state, header, {body: None, placeholder: Some("name:"), value: None}, expr =>
      sendAgdaRequest(SearchAbout(normalization, expr))
    )
  | Give =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, "")) =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: Some("expression to give:"),
        value: None,
      }, expr =>
        State__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
          sendAgdaRequest(Give(goal))
        )
      )
    | Some((goal, _)) => sendAgdaRequest(Give(goal))
    }
  | Refine =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, _)) => sendAgdaRequest(Refine(goal))
    }
  | ElaborateAndGive(normalization) => {
      let placeholder = Some("expression to elaborate and give:")
      switch State__Goal.pointed(state) {
      | None => State.View.displayOutOfGoalError(state)
      | Some((goal, "")) =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => {sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))})
      | Some((goal, expr)) => sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))
      }
    }
  | Auto =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, _)) => sendAgdaRequest(Auto(goal))
    }
  | Case => {
      let placeholder = Some("variable to case split:")
      switch State__Goal.pointed(state) {
      | None => State.View.displayOutOfGoalError(state)
      | Some((goal, "")) =>
        State.View.prompt(state, header, {
          body: Some("Please specify which variable you wish to split"),
          placeholder: placeholder,
          value: None,
        }, expr =>
          // place the queried expression in the goal
          State__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
            sendAgdaRequest(Case(goal))
          )
        )
      | Some((goal, _)) => sendAgdaRequest(Case(goal))
      }
    }
  | HelperFunctionType(normalization) => {
      let placeholder = Some("expression:")
      switch State__Goal.pointed(state) {
      | None => State.View.displayOutOfGoalError(state)
      | Some((goal, "")) =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: placeholder,
          value: None,
        }, expr => sendAgdaRequest(HelperFunctionType(normalization, expr, goal)))
      | Some((goal, expr)) => sendAgdaRequest(HelperFunctionType(normalization, expr, goal))
      }
    }
  | InferType(normalization) =>
    let placeholder = Some("expression to infer:")
    switch State__Goal.pointed(state) {
    | None =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(InferTypeGlobal(normalization, expr)))
    | Some((goal, "")) =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(InferType(normalization, expr, goal)))
    | Some((goal, expr)) => sendAgdaRequest(InferType(normalization, expr, goal))
    }
  | Context(normalization) =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, _)) => sendAgdaRequest(Context(normalization, goal))
    }
  | GoalType(normalization) =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, _)) => sendAgdaRequest(GoalType(normalization, goal))
    }
  | GoalTypeAndContext(normalization) =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, _)) => sendAgdaRequest(GoalTypeAndContext(normalization, goal))
    }
  | GoalTypeContextAndInferredType(normalization) =>
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, "")) =>
      // fallback to `GoalTypeAndContext` when there's no content
      sendAgdaRequest(GoalTypeAndContext(normalization, goal))
    | Some((goal, expr)) =>
      sendAgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal))
    }
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    switch State__Goal.pointed(state) {
    | None => State.View.displayOutOfGoalError(state)
    | Some((goal, "")) =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)))
    | Some((goal, expr)) =>
      sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal))
    }
  | ModuleContents(normalization) =>
    let placeholder = Some("module name:")
    switch State__Goal.pointed(state) {
    | None =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ModuleContentsGlobal(normalization, expr)))
    | Some((goal, "")) =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ModuleContents(normalization, expr, goal)))
    | Some((goal, expr)) => sendAgdaRequest(ModuleContents(normalization, expr, goal))
    }
  | ComputeNormalForm(computeMode) =>
    let placeholder = Some("expression to normalize:")
    switch State__Goal.pointed(state) {
    | None =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ComputeNormalFormGlobal(computeMode, expr)))
    | Some((goal, "")) =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal)))
    | Some((goal, expr)) => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal))
    }
  | WhyInScope =>
    let placeholder = Some("name:")
    switch State__Goal.pointed(state) {
    | None =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(WhyInScopeGlobal(expr)))
    | Some((goal, "")) =>
      State.View.prompt(state, header, {
        body: None,
        placeholder: placeholder,
        value: None,
      }, expr => sendAgdaRequest(WhyInScope(expr, goal)))
    | Some((goal, expr)) => sendAgdaRequest(WhyInScope(expr, goal))
    }
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
