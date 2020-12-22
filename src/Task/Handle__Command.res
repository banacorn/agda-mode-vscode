open Belt
open Command

// There are 2 kinds of Responses
//  NonLast Response :
//    * get handled first
//    * don't invoke `sendAgdaRequest`
//  Last Response :
//    * have priorities, those with the smallest priority number are executed first
//    * only get handled:
//        1. after prompt has reappeared
//        2. after all NonLast Responses
//        3. after all interactive highlighting is complete
//    * may invoke `sendAgdaRequest`

// This module makes sure that Last Responses are handled after NonLast Responses
module Lock: {
  let runNonLast: Promise.t<'a> => Promise.t<'a>
  let onceDone: unit => Promise.t<unit>
} = {
  // keep the number of running NonLast Response
  let tally = ref(0)
  let allDone = Chan.make()
  // NonLast Responses should fed here
  let runNonLast = promise => {
    tally := tally.contents + 1
    promise->Promise.tap(_ => {
      tally := tally.contents - 1
      if tally.contents == 0 {
        allDone->Chan.emit()
      }
    })
  }
  // gets resolved once there's no NonLast Responses running
  let onceDone = () =>
    if tally.contents == 0 {
      Promise.resolved()
    } else {
      allDone->Chan.once
    }
}

// helper function of `executeTask`
let rec sendAgdaRequest = (
  dispatchCommand: Command.t => Promise.t<unit>,
  state: State.t,
  request: Request.t,
): Promise.t<unit> => {
  // Js.log("<<< " ++ Request.toString(request))
  let displayConnectionError = error => {
    let (header, body) = Connection.Error.toString(error)
    State.View.display(state, Error("Connection Error: " ++ header), Plain(body))
  }

  // deferred responses are queued here
  let deferredLastResponses: array<(int, Response.t)> = []

  // this promise get resolved after all Responses has been received from Agda
  let (promise, stopListener) = Promise.pending()
  let handle = ref(None)
  let agdaResponseListener: result<Connection.response, Connection.Error.t> => unit = x =>
    switch x {
    | Error(error) => displayConnectionError(error)->ignore
    | Ok(Parser.Incr.Gen.Yield(Error(error))) =>
      let body = Parser.Error.toString(error)
      State.View.display(state, Error("Internal Parse Error"), Plain(body))->ignore
    | Ok(Yield(Ok(NonLast(response)))) =>
      // Js.log(">>> " ++ Response.toString(response))
      Lock.runNonLast(
        Handle__Response.handle(
          state,
          dispatchCommand,
          sendAgdaRequest(dispatchCommand, state),
          response,
        ),
      )->ignore
    | Ok(Yield(Ok(Last(priority, response)))) =>
      // Js.log(">>* " ++ string_of_int(priority) ++ " " ++ Response.toString(response))
      Js.Array.push((priority, response), deferredLastResponses)->ignore
    | Ok(Stop) =>
      // Js.log(">>| ")
      // sort the deferred Responses by priority (ascending order)
      let deferredLastResponses =
        Js.Array.sortInPlaceWith(
          (x, y) => compare(fst(x), fst(y)),
          deferredLastResponses,
        )->Array.map(snd)

      // wait until all NonLast Responses are handled
      Lock.onceDone()
      // stop the Agda Response listener
      ->Promise.tap(_ => stopListener())
      // apply decoration before handling Last Responses
      ->Promise.flatMap(_ => State.Decoration.apply(state))
      ->Promise.map(() =>
        deferredLastResponses->Array.map(
          Handle__Response.handle(state, dispatchCommand, sendAgdaRequest(dispatchCommand, state)),
        )
      )
      ->Promise.flatMap(Util.oneByOne)
      ->ignore
    }

  state
  ->State.connect
  ->Promise.mapOk(connection => {
    let document = VSCode.TextEditor.document(state.editor)
    let version = connection.metadata.version
    let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
    let libraryPath = Config.getLibraryPath()
    let highlightingMethod = Config.getHighlightingMethod()
    let backend = Config.getBackend()
    let encoded = Request.encode(
      document,
      version,
      filepath,
      backend,
      libraryPath,
      highlightingMethod,
      request,
    )
    Connection.send(encoded, connection)
    connection
  })
  ->Promise.flatMap(x =>
    switch x {
    | Ok(connection) =>
      handle := Some(connection.Connection.chan->Chan.on(agdaResponseListener))
      promise
    | Error(error) => displayConnectionError(error)->Promise.flatMap(() => promise)
    }
  )
  ->Promise.tap(() => handle.contents->Option.forEach(destroyListener => destroyListener()))
}

// from Editor Command to Tasks
let rec dispatchCommand = (state: State.t, command): Promise.t<unit> => {
  let dispatchCommand = dispatchCommand(state)
  let sendAgdaRequest = sendAgdaRequest(dispatchCommand, state)
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
    Handle__Goal.updateRanges(state)
    State.Decoration.refresh(state)
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
    State.View.prompt(state, header, {body: None, placeholder: Some("name:"), value: None}, expr =>
      sendAgdaRequest(SearchAbout(normalization, expr))
    )
  | Give =>
    Handle__Goal.case(
      state,
      (goal, _) => {sendAgdaRequest(Give(goal))},
      goal =>
        State.View.prompt(state, header, {
          body: None,
          placeholder: Some("expression to give:"),
          value: None,
        }, expr =>
          Handle__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
            sendAgdaRequest(Give(goal))
          )
        ),
      State.View.displayOutOfGoalError(state),
    )
  | Refine =>
    Handle__Goal.caseSimple(
      state,
      goal => {sendAgdaRequest(Refine(goal))},
      State.View.displayOutOfGoalError(state),
    )
  | ElaborateAndGive(normalization) => {
      let placeholder = Some("expression to elaborate and give:")
      Handle__Goal.case(
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
    Handle__Goal.caseSimple(
      state,
      goal => {sendAgdaRequest(Auto(goal))},
      State.View.displayOutOfGoalError(state),
    )
  | Case => {
      let placeholder = Some("variable to case split:")
      Handle__Goal.case(
        state,
        (goal, _) => {sendAgdaRequest(Case(goal))},
        goal =>
          State.View.prompt(state, header, {
            body: Some("Please specify which variable you wish to split"),
            placeholder: placeholder,
            value: None,
          }, expr =>
            // place the queried expression in the goal
            Handle__Goal.modify(state, goal, _ => expr)->Promise.flatMap(() =>
              sendAgdaRequest(Case(goal))
            )
          ),
        State.View.displayOutOfGoalError(state),
      )
    }
  | HelperFunctionType(normalization) => {
      let placeholder = Some("expression:")
      Handle__Goal.case(
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
    Handle__Goal.case(
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
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(Context(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalType(normalization) =>
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(GoalType(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalTypeAndContext(normalization) =>
    Handle__Goal.caseSimple(
      state,
      goal => sendAgdaRequest(GoalTypeAndContext(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalTypeContextAndInferredType(normalization) =>
    Handle__Goal.case(
      state,
      (goal, expr) => sendAgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal)),
      // fallback to `GoalTypeAndContext` when there's no content
      goal => sendAgdaRequest(GoalTypeAndContext(normalization, goal)),
      State.View.displayOutOfGoalError(state),
    )
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    state->Handle__Goal.case(
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
    Handle__Goal.case(
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
    Handle__Goal.case(
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
    Handle__Goal.case(
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
        State.View.interruptPrompt(state)
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
          let ranges = intervals->Array.map(View.AgdaInterval.toRange)
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
      State.View.interruptPrompt(state)
    }
  | InputMethod(Activate) => Handle__InputMethod.activateEditorIM(state)
  | InputMethod(InsertChar(char)) => Handle__InputMethod.insertChar(state, char)
  | InputMethod(BrowseUp) => Handle__InputMethod.moveUp(state)
  | InputMethod(BrowseDown) => Handle__InputMethod.moveDown(state)
  | InputMethod(BrowseLeft) => Handle__InputMethod.moveLeft(state)
  | InputMethod(BrowseRight) => Handle__InputMethod.moveRight(state)
  }
}
