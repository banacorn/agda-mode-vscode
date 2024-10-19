open Belt
open Command

// from Editor Command to Tasks
let rec dispatchCommand = async (state: State.t, command): result<unit, Connection.Error.t> => {
  let dispatchCommand = dispatchCommand(state, ...)
  let sendAgdaRequest =
    State.sendRequest(state, State__Response.handle(state, dispatchCommand, ...), ...)
  let header = View.Header.Plain(Command.toString(command))
  switch command {
  | Load =>
    await State.View.DebugBuffer.restore(state)
    await State.View.Panel.display(state, Plain("Loading ..."), [])
    // save the document before loading
    let _ = await VSCode.TextDocument.save(state.document)
    // Issue #26 - don't load the document in preview mode
    let options = Some(VSCode.TextDocumentShowOptions.make(~preview=false, ()))
    let _ = await VSCode.Window.showTextDocumentWithShowOptions(state.document, options)
    await sendAgdaRequest(Load)
  | Quit => Ok()
  | Restart =>
    // clear the RunningInfo log
    state.runningInfoLog = []
    await dispatchCommand(Load)
  | Refresh =>
    state.highlighting->Highlighting.redecorate(state.editor)
    State.View.Panel.restore(state)
    State__Goal.redecorate(state)
    await State.View.DebugBuffer.restore(state)
    Ok()
  | Compile => await sendAgdaRequest(Compile)
  | ToggleDisplayOfImplicitArguments => await sendAgdaRequest(ToggleDisplayOfImplicitArguments)
  | ToggleDisplayOfIrrelevantArguments => await sendAgdaRequest(ToggleDisplayOfIrrelevantArguments)
  | ShowConstraints => await sendAgdaRequest(ShowConstraints)
  | SolveConstraints(normalization) =>
    switch State__Goal.pointed(state) {
    | None => await sendAgdaRequest(SolveConstraintsGlobal(normalization))
    | Some((goal, _)) => await sendAgdaRequest(SolveConstraints(normalization, goal))
    }
  | ShowGoals(normalization) => await sendAgdaRequest(ShowGoals(normalization))
  | NextGoal =>
    State__Goal.next(state)
    Ok()
  | PreviousGoal =>
    State__Goal.previous(state)
    Ok()
  | SearchAbout(normalization) =>
    await State.View.Panel.prompt(
      state,
      header,
      {body: None, placeholder: Some("name:"), value: None},
      expr => sendAgdaRequest(SearchAbout(normalization, expr)),
    )
    Ok()
  | Give =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, "")) =>
      await State.View.Panel.prompt(
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
            await State__Goal.modify(state, goal, _ => expr)
            await sendAgdaRequest(Give(goal))
          },
      )
      Ok()
    | Some((goal, _)) => await sendAgdaRequest(Give(goal))
    }
  | Refine =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, _)) => await sendAgdaRequest(Refine(goal))
    }
  | ElaborateAndGive(normalization) => {
      let placeholder = Some("expression to elaborate and give:")
      switch State__Goal.pointed(state) {
      | None =>
        await State.View.Panel.displayOutOfGoalError(state)
        Ok()
      | Some((goal, "")) =>
        await State.View.Panel.prompt(
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
              await State__Goal.modify(state, goal, _ => expr)
              await sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))
            },
        )
        Ok()
      | Some((goal, expr)) => await sendAgdaRequest(ElaborateAndGive(normalization, expr, goal))
      }
    }
  | Auto =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, _)) => await sendAgdaRequest(Auto(goal))
    }
  | Case => {
      let placeholder = Some("variable to case split:")
      switch State__Goal.pointed(state) {
      | None =>
        await State.View.Panel.displayOutOfGoalError(state)
        Ok()
      | Some((goal, "")) =>
        await State.View.Panel.prompt(
          state,
          header,
          {
            body: Some("Please specify which variable you wish to split"),
            placeholder,
            value: None,
          },
          async expr =>
            if expr == "" {
              await sendAgdaRequest(Case(goal))
            } else {
              // place the queried expression in the goal
              await State__Goal.modify(state, goal, _ => expr)
              await sendAgdaRequest(Case(goal))
            },
        )
        Ok()
      | Some((goal, _)) => await sendAgdaRequest(Case(goal))
      }
    }
  | HelperFunctionType(normalization) => {
      let placeholder = Some("expression:")
      switch State__Goal.pointed(state) {
      | None =>
        await State.View.Panel.displayOutOfGoalError(state)
        Ok()
      | Some((goal, "")) =>
        await State.View.Panel.prompt(
          state,
          header,
          {
            body: None,
            placeholder,
            value: None,
          },
          expr => sendAgdaRequest(HelperFunctionType(normalization, expr, goal)),
        )
        Ok()
      | Some((goal, expr)) => await sendAgdaRequest(HelperFunctionType(normalization, expr, goal))
      }
    }
  | InferType(normalization) =>
    let placeholder = Some("expression to infer:")
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(InferTypeGlobal(normalization, expr)),
      )
      Ok()
    | Some((goal, "")) =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(InferType(normalization, expr, goal)),
      )
      Ok()
    | Some((goal, expr)) => await sendAgdaRequest(InferType(normalization, expr, goal))
    }
  | Context(normalization) =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, _)) => await sendAgdaRequest(Context(normalization, goal))
    }
  | GoalType(normalization) =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, _)) => await sendAgdaRequest(GoalType(normalization, goal))
    }
  | GoalTypeAndContext(normalization) =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, _)) => await sendAgdaRequest(GoalTypeAndContext(normalization, goal))
    }
  | GoalTypeContextAndInferredType(normalization) =>
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, "")) =>
      // fallback to `GoalTypeAndContext` when there's no content
      await sendAgdaRequest(GoalTypeAndContext(normalization, goal))
    | Some((goal, expr)) =>
      await sendAgdaRequest(GoalTypeContextAndInferredType(normalization, expr, goal))
    }
  | GoalTypeContextAndCheckedType(normalization) =>
    let placeholder = Some("expression to type:")
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.displayOutOfGoalError(state)
      Ok()
    | Some((goal, "")) =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal)),
      )
      Ok()
    | Some((goal, expr)) =>
      await sendAgdaRequest(GoalTypeContextAndCheckedType(normalization, expr, goal))
    }
  | ModuleContents(normalization) =>
    let placeholder = Some("module name:")
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(ModuleContentsGlobal(normalization, expr)),
      )
      Ok()
    | Some((goal, "")) =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(ModuleContents(normalization, expr, goal)),
      )
      Ok()
    | Some((goal, expr)) => await sendAgdaRequest(ModuleContents(normalization, expr, goal))
    }
  | ComputeNormalForm(computeMode) =>
    let placeholder = Some("expression to normalize:")
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(ComputeNormalFormGlobal(computeMode, expr)),
      )
      Ok()
    | Some((goal, "")) =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal)),
      )
      Ok()
    | Some((goal, expr)) => await sendAgdaRequest(ComputeNormalForm(computeMode, expr, goal))
    }
  | WhyInScope =>
    let placeholder = Some("name:")
    switch State__Goal.pointed(state) {
    | None =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(WhyInScopeGlobal(expr)),
      )
      Ok()
    | Some((goal, "")) =>
      await State.View.Panel.prompt(
        state,
        header,
        {
          body: None,
          placeholder,
          value: None,
        },
        expr => sendAgdaRequest(WhyInScope(expr, goal)),
      )
      Ok()
    | Some((goal, expr)) => await sendAgdaRequest(WhyInScope(expr, goal))
    }
  | SwitchAgdaVersion =>
    // preserve the original version, in case the new one fails
    let oldAgdaVersion = Config.Connection.getAgdaVersion()
    // prompt the user for the new version
    await State.View.Panel.prompt(
      state,
      header,
      {
        body: None,
        placeholder: None,
        value: Some(oldAgdaVersion),
      },
      async expr => {
        let oldAgdaPath = Config.Connection.getAgdaPath()
        let newAgdaVersion = Js.String.trim(expr)
        // don't connect to the LSP server
        let useLSP = false

        await Config.Connection.setAgdaPath("")
        // set the name of executable to `newAgdaVersion` in the settings
        await Config.Connection.setAgdaVersion(newAgdaVersion)
        await State.View.Panel.display(
          state,
          View.Header.Plain("Switching to '" ++ newAgdaVersion ++ "'"),
          [],
        )
        // stop the old connection
        let _ = await Connection.stop()
        switch await Connection.start(
          state.globalStoragePath,
          useLSP,
          State.onDownload(state, ...)
        ) {
        | Ok(Emacs(version, path)) =>
          // update the connection status
          await State.View.Panel.displayStatus(state, "Emacs v" ++ version)
          await State.View.Panel.display(
            state,
            View.Header.Success("Switched to version '" ++ version ++ "'"),
            [Item.plainText("Found '" ++ newAgdaVersion ++ "' at: " ++ path)],
          )
          Ok()
        | Ok(LSP(version, _)) =>
          // should not happen
          await State.View.Panel.display(
            state,
            View.Header.Success("Panic, Switched to LSP server '" ++ version ++ "'"),
            [Item.plainText("Should have switched to an Agda executable, please file an issue")],
          )
          Ok()
        | Error(error) =>
          let (errorHeader, errorBody) = Connection.Error.toString(error)
          let header = View.Header.Error(
            "Cannot switch Agda version '" ++ newAgdaVersion ++ "' : " ++ errorHeader,
          )
          let body = [Item.plainText(errorBody ++ "\n\n" ++ "Switching back to " ++ oldAgdaPath)]
          await Config.Connection.setAgdaPath(oldAgdaPath)
          await State.View.Panel.display(state, header, body)
          Ok()
        }
      },
    )
    Ok()
  | EventFromView(event) =>
    switch event {
    | Initialized => Ok()
    | Destroyed => await State.destroy(state, true)
    | InputMethod(InsertChar(char)) => await dispatchCommand(InputMethod(InsertChar(char)))
    | InputMethod(ChooseSymbol(symbol)) =>
      await State__InputMethod.chooseSymbol(state, symbol)
      Ok()
    | PromptIMUpdate(MouseSelect(interval)) =>
      await State__InputMethod.select(state, [interval])
      Ok()
    | PromptIMUpdate(KeyUpdate(input)) =>
      await State__InputMethod.keyUpdatePromptIM(state, input)
      Ok()
    | PromptIMUpdate(BrowseUp) => await dispatchCommand(InputMethod(BrowseUp))
    | PromptIMUpdate(BrowseDown) => await dispatchCommand(InputMethod(BrowseDown))
    | PromptIMUpdate(BrowseLeft) => await dispatchCommand(InputMethod(BrowseLeft))
    | PromptIMUpdate(BrowseRight) => await dispatchCommand(InputMethod(BrowseRight))
    | PromptIMUpdate(Escape) =>
      if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
        await State__InputMethod.deactivate(state)
      } else {
        await State.View.Panel.interruptPrompt(state)
      }
      Ok()
    | JumpToTarget(link) =>
      Editor.focus(state.document)
      let path = state.document->VSCode.TextDocument.fileName->Parser.filepath
      switch link {
      | SrcLoc(NoRange) => Ok()
      | SrcLoc(Range(None, _intervals)) => Ok()
      | SrcLoc(Range(Some(fileName), intervals)) =>
        let fileName = Parser.filepath(fileName)
        // Issue #44
        //  In Windows, paths from Agda start from something like "c://" while they are "C://" from VS Code
        //  We need to remove the root from the path before comparing them
        let removeRoot = path => {
          let obj = NodeJs.Path.parse(path)
          let rootLength = String.length(obj.root)
          let newDir = Js.String.sliceToEnd(~from=rootLength, obj.dir)
          let newObj: NodeJs.Path.t = {
            root: "",
            dir: newDir,
            ext: obj.ext,
            name: obj.name,
            base: obj.base,
          }
          NodeJs.Path.format(newObj)
        }

        // only select the ranges when it's on the same file
        if removeRoot(path) == removeRoot(fileName) {
          let ranges = intervals->Array.map(Editor.Range.fromAgdaInterval)
          // set cursor selections
          Editor.Selection.setMany(state.editor, ranges)
          // scroll to that part of the document
          ranges[0]->Option.forEach(range => {
            state.editor->VSCode.TextEditor.revealRange(range, None)
          })
        }
        Ok()
      | Hole(index) =>
        let goal = Js.Array.find((goal: Goal.t) => goal.index == index, state.goals)
        switch goal {
        | None => ()
        | Some(goal) => Goal.setCursor(goal, state.editor)
        }
        Ok()
      }
    }
  | Escape =>
    if state.editorIM->IM.isActivated || state.promptIM->IM.isActivated {
      await State__InputMethod.deactivate(state)
      Ok()
    } else {
      await State.View.Panel.interruptPrompt(state)
      Ok()
    }
  | InputMethod(Activate) =>
    if Config.InputMethod.getEnable() {
      await State__InputMethod.activateEditorIM(state)
      Ok()
    } else {
      // insert the activation key (default: "\") instead
      let activationKey = Config.InputMethod.getActivationKey()
      Editor.Cursor.getMany(state.editor)->Array.forEach(point =>
        Editor.Text.insert(state.document, point, activationKey)->ignore
      )
      Ok()
    }

  | InputMethod(InsertChar(char)) =>
    await State__InputMethod.insertChar(state, char)
    Ok()
  | InputMethod(BrowseUp) =>
    await State__InputMethod.moveUp(state)
    Ok()
  | InputMethod(BrowseDown) =>
    await State__InputMethod.moveDown(state)
    Ok()
  | InputMethod(BrowseLeft) =>
    await State__InputMethod.moveLeft(state)
    Ok()
  | InputMethod(BrowseRight) =>
    await State__InputMethod.moveRight(state)
    Ok()
  | LookupSymbol =>
    // get the selected text
    // query the user instead if no text is selected
    let (promise, resolve, _) = Util.Promise_.pending()
    let selectedText =
      Editor.Text.get(state.document, Editor.Selection.get(state.editor))->Js.String.trim
    if selectedText == "" {
      State.View.Panel.prompt(
        state,
        View.Header.Plain("Lookup Unicode Symbol Input Sequence"),
        {body: None, placeholder: Some("symbol to lookup:"), value: None},
        input => {
          resolve(Js.String.trim(input))
          Promise.resolve(Ok())
        },
      )->ignore
    } else {
      resolve(selectedText)
    }

    // lookup and display
    let input = await promise
    let sequences = Translator.lookup(input)->Option.getWithDefault([])
    if Js.Array.length(sequences) == 0 {
      await State.View.Panel.display(
        state,
        View.Header.Warning("No Input Sequences Found for \"" ++ selectedText ++ "\""),
        [],
      )
    } else {
      await State.View.Panel.display(
        state,
        View.Header.Success(
          string_of_int(Js.Array.length(sequences)) ++
          " Input Sequences Found for \"" ++
          selectedText ++ "\"",
        ),
        sequences->Array.map(sequence => Item.plainText(sequence)),
      )
    }
    Ok()
  | OpenDebugBuffer =>
    State.View.DebugBuffer.make(state)->ignore
    await State.View.DebugBuffer.reveal(state)
    Ok()
  }
}
