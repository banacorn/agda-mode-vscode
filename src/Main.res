open Belt

// if end with '.agda' or '.lagda'
let isAgda = (filepath): bool => {
  let filepath = filepath->Parser.filepath
  Js.Re.test_(%re("/\\.agda$|\\.lagda/i"), filepath)
}

module EventHandler = {
  let onActivateExtension = callback => {
    // there's a visible Agda file in the workplace
    let visible =
      VSCode.Window.visibleTextEditors
      ->Array.map(editor => editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName)
      ->Array.keep(isAgda)
      ->Array.length > 0
    // should activate the view when there's a visible Agda file
    let shouldAcitvateView = visible && !ViewController.isActivated()

    if shouldAcitvateView {
      callback()
    }
  }

  let onDeactivateExtension = callback => {
    // number of Agda States in the Registry
    let openedCount = Registry.size()
    // should deacitvate the view when all Agda States have been destroyed
    let shouldDeacitvateView = openedCount === 0 && ViewController.isActivated()

    if shouldDeacitvateView {
      callback()
    }
  }

  let onOpenEditor = (_extensionPath, editor) => {
    let filePath = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    // filter out ".agda.git" files
    if isAgda(filePath) {
      // this callback will be invoked when the first editor is opened
      onActivateExtension(() => {
        // Client.start()->ignore
        ()
      })

      Registry.get(filePath)->Option.forEach(state => {
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        state.editor = editor
        state.document = editor->VSCode.TextEditor.document

        //
        State__Command.dispatchCommand(state, Refresh)->ignore
      })

      // Client.send(Req(state.filePath, Load))->Promise.flatMap(handleResponse)->ignore
    }
  }

  let onCloseEditor = doc => {
    let filePath = VSCode.TextDocument.fileName(doc)
    if isAgda(filePath) {
      Registry.removeAndDestroy(filePath)
      // this callback will be invoked after all editors have been closed
      onDeactivateExtension(() => {
        ViewController.deactivate()
        // Client.stop()->ignore
      })
    }
  }
}

let makeAndAddToRegistry = (debugChan, extensionPath, editor, fileName) => {
  // see if the view should be activated
  if !ViewController.isActivated() {
    ViewController.activate(extensionPath)
  }

  // not in the Registry, instantiate a State
  let state = State.make(debugChan, editor)

  let subscribe = disposable => disposable->Js.Array.push(state.subscriptions)->ignore

  // listens to events from the view and relay them as Commands
  ViewController.onEvent(event =>
    State__Command.dispatchCommand(state, EventFromView(event))->ignore
  )->subscribe

  // register event listeners for the input method
  VSCode.Window.onDidChangeTextEditorSelection(.event => {
    let document = VSCode.TextEditor.document(editor)
    let intervals =
      event
      ->VSCode.TextEditorSelectionChangeEvent.selections
      ->Array.map(selection => (
        Editor.Position.toOffset(document, VSCode.Selection.start(selection)),
        Editor.Position.toOffset(document, VSCode.Selection.end_(selection)),
      ))

    State__InputMethod.select(state, intervals)->ignore
  })->subscribe
  VSCode.Workspace.onDidChangeTextDocument(.event => {
    let changes = IM.Input.fromTextDocumentChangeEvent(editor, event)
    State__InputMethod.keyUpdateEditorIM(state, changes)->ignore
  })->subscribe

  // remove it from the Registry if it requests to be destroyed
  state.onRemoveFromRegistry->Chan.once->Promise.get(() => Registry.remove(fileName))

  // definition provider for go-to-definition
  Editor.Provider.registerDefinitionProvider((fileName, position) => {
    // only provide source location, when the filename matched
    let currentFileName = state.document->VSCode.TextDocument.fileName->Parser.filepath

    if fileName == currentFileName {
      state.decoration->Decoration.lookupSrcLoc(position)
    } else {
      None
    }
  })->subscribe

  // hover provider
  Editor.Provider.registerHoverProvider((fileName, point) => {
    // only provide source location, when the filename matched
    let currentFileName = state.document->VSCode.TextDocument.fileName->Parser.filepath

    if fileName == currentFileName {
      let range = VSCode.Range.make(point, point)
      Some(Promise.resolved(([""], range)))
    } else {
      None
    }
  })->subscribe

  // these two arrays are called "legends"
  let tokenTypes = Highlighting.Aspect.TokenType.enumurate
  let tokenModifiers = Highlighting.Aspect.TokenModifier.enumurate

  Editor.Provider.registerSemnaticTokenProvider((fileName, pushToken) => {
    let useSemanticHighlighting = Config.getSemanticHighlighting()
    let document = VSCode.TextEditor.document(editor)
    let currentFileName = document->VSCode.TextDocument.fileName->Parser.filepath

    if useSemanticHighlighting && fileName == currentFileName {
      Some(Decoration.generateSemanticTokens(state.decoration, state.editor, pushToken))
    } else {
      None
    }
  }, (tokenTypes, tokenModifiers))->subscribe

  // add this state to the Registry
  Registry.add(fileName, state)
}

let activateWithoutContext = (subscriptions, extensionPath) => {
  let subscribe = x => x->Js.Array.push(subscriptions)->ignore

  // on open
  VSCode.Window.activeTextEditor->Option.forEach(EventHandler.onOpenEditor(extensionPath))
  VSCode.Window.onDidChangeActiveTextEditor(.next =>
    next->Option.forEach(EventHandler.onOpenEditor(extensionPath))
  )->subscribe

  // on close
  VSCode.Workspace.onDidCloseTextDocument(. EventHandler.onCloseEditor)->subscribe

  // on trigger command
  let registerCommand = (name, callback) =>
    VSCode.Commands.registerCommand("agda-mode." ++ name, () =>
      VSCode.Window.activeTextEditor->Option.map(editor => {
        let fileName =
          editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
        callback(editor, fileName)
      })
    )

  // Channel for testing, emits events when something has been completed,
  // for example, when the input method has translated a key sequence into a symbol
  let debugChan = Chan.make()

  Command.names->Array.forEach(((command, name)) =>
    registerCommand(name, (editor, fileName) =>
      if isAgda(fileName) {
        Js.log("[ command ] " ++ name)
        // Commands like "Load", "InputMethod", "Quit", and "Restart" affects the Registry
        switch command {
        | Load
        | InputMethod(Activate) =>
          switch Registry.get(fileName) {
          | None => makeAndAddToRegistry(debugChan, extensionPath, editor, fileName)
          | Some(_) => () // already in the Registry, do nothing
          }
        | Quit => Registry.removeAndDestroy(fileName)
        | Restart =>
          Registry.removeAndDestroy(fileName)
          makeAndAddToRegistry(debugChan, extensionPath, editor, fileName)
        | _ => ()
        }
        // dispatch command
        switch Registry.get(fileName) {
        | None => Promise.resolved()
        | Some(state) => State__Command.dispatchCommand(state, command)
        }
      } else {
        Promise.resolved()
      }
    )->subscribe
  )

  // expose the channel for testing
  debugChan
}

// this function is the entry point of the whole extension
let activate = context => {
  let subscriptions = VSCode.ExtensionContext.subscriptions(context)
  let extensionPath = VSCode.ExtensionContext.extensionPath(context)
  activateWithoutContext(subscriptions, extensionPath)
}

let deactivate = () => ()
