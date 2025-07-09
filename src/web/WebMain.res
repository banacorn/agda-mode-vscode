// Web-compatible entry point for agda-mode VS Code extension
// This version excludes desktop-only modules and Node.js dependencies

// if end with '.agda' or '.lagda'
let isAgda = (fileName): bool => {
  let fileName = fileName->Parser.filepath
  RegExp.test(%re("/\.agda$|\.lagda/i"), fileName)
}

module Inputs: {
  let onOpenEditor: (VSCode.TextEditor.t => unit) => VSCode.Disposable.t
  let onCloseDocument: (VSCode.TextDocument.t => unit) => VSCode.Disposable.t
  let onTriggerCommand: (
    (Command.t, VSCode.TextEditor.t) => promise<option<result<State.t, Connection.Error.t>>>
  ) => array<VSCode.Disposable.t>
} = {
  let onOpenEditor = callback => {
    VSCode.Window.activeTextEditor->Option.forEach(callback)
    VSCode.Window.onDidChangeActiveTextEditor(next => next->Option.forEach(callback))
  }
  let onCloseDocument = callback => VSCode.Workspace.onDidCloseTextDocument(callback)
  // invoke the callback when:
  //  1. the triggered command has prefix "agda-mode."
  //  2. there's an active text edtior
  //  3. the active text editor is an Agda file
  let onTriggerCommand = callback => {
    Command.names->Array.map(((command, name)) =>
      VSCode.Commands.registerCommand("agda-mode." ++ name, () => {
        VSCode.Window.activeTextEditor->Option.map(
          editor => {
            let fileName =
              editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
            if isAgda(fileName) {
              callback(command, editor)
            } else {
              Promise.resolve(None)
            }
          },
        )
      })
    )
  }
}

let initialize = (
  channels,
  extensionPath,
  globalStorageUri,
  memento,
  editor,
  fileName,
  semanticTokensRequest,
) => {
  let panel = Singleton.Panel.make(extensionPath)
  // if the panel is destroyed, destroy all every State in the Registry
  WebviewPanel.onceDestroyed(panel)
  ->Promise.finally(() => Registry.removeAndDestroyAll()->ignore)
  ->Promise.done

  // not in the Registry, instantiate a State
  let state = State.make(
    channels,
    globalStorageUri,
    extensionPath,
    memento,
    editor,
    semanticTokensRequest,
  )
  // Set panel's font size by configuration
  state->State__View.Panel.setFontSize(Config.Buffer.getFontSize())->ignore
  // remove it from the Registry on request
  state.onRemoveFromRegistry
  ->Chan.once
  ->Promise.finally(() => Registry.remove(fileName))
  ->Promise.done

  ////////////////////////////////////////////////////////////////
  // input events
  ////////////////////////////////////////////////////////////////

  let subscribe = disposable => state.subscriptions->Array.push(disposable)->ignore

  let getCurrentEditor = () =>
    switch VSCode.Window.activeTextEditor {
    | Some(editor) => Some(editor)
    | None => VSCode.Window.visibleTextEditors[0]
    }

  state
  ->State__View.Panel.get
  ->WebviewPanel.onEvent(event => {
    switch getCurrentEditor() {
    | Some(editor') =>
      let fileName' =
        editor'->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
      if fileName' == fileName {
        State__Command.dispatchCommand(state, Command.EventFromView(event))->ignore
      }
    | None => ()
    }
  })
  ->subscribe
  // register event listeners for the input method
  VSCode.Window.onDidChangeTextEditorSelection(event => {
    let document = VSCode.TextEditor.document(editor)
    let intervals =
      event
      ->VSCode.TextEditorSelectionChangeEvent.selections
      ->Array.map(selection => (
        VSCode.TextDocument.offsetAt(document, VSCode.Selection.start(selection)),
        VSCode.TextDocument.offsetAt(document, VSCode.Selection.end_(selection)),
      ))

    State__InputMethod.select(state, intervals)->ignore
  })->subscribe
  VSCode.Workspace.onDidChangeTextDocument(event => {
    // update the input method accordingly
    let changes = IM.Input.fromTextDocumentChangeEvent(editor, event)
    State__InputMethod.keyUpdateEditorIM(state, changes)->ignore
    // updates positions of semantic highlighting tokens accordingly
    state.tokens->Tokens.applyEdit(editor, event)
    // updates positions of goals accordingly
    let changes =
      event
      ->VSCode.TextDocumentChangeEvent.contentChanges
      ->Array.map(Tokens.Change.fromTextDocumentContentChangeEvent)
      ->Array.toReversed
    if Array.length(changes) != 0 {
      state.goals->Goals.scanAllGoals(editor, changes)->Promise.done
    }

    // state.highlighting->Highlighting.updateSemanticHighlighting(event)->Promise.done
  })->subscribe

  // definition provider for go-to-definition
  Editor.Provider.registerDefinitionProvider((filepath, position) =>
    Tokens.goToDefinition(state.tokens, state.document)(Parser.Filepath.make(filepath), position)
  )->subscribe

  // add this state to the Registry
  state
}

let registerDocumentSemanticTokensProvider = () => {
  // these two arrays are called "legends"
  let tokenTypes = Highlighting__SemanticToken.TokenType.enumurate
  let tokenModifiers = Highlighting__SemanticToken.TokenModifier.enumurate

  let provideDocumentSemanticTokens = (document, _cancel) => {
    let fileName = document->VSCode.TextDocument.fileName->Parser.filepath
    Registry.requestSemanticTokens(fileName)
    ->Promise.thenResolve(tokens => {
      open Editor.Provider.Mock

      let semanticTokensLegend = SemanticTokensLegend.makeWithTokenModifiers(
        tokenTypes,
        tokenModifiers,
      )
      let builder = SemanticTokensBuilder.makeWithLegend(semanticTokensLegend)

      tokens->Array.forEach(({range, type_, modifiers}) => {
        SemanticTokensBuilder.pushLegend(
          builder,
          Highlighting__SemanticToken.SingleLineRange.toVsCodeRange(range),
          Highlighting__SemanticToken.TokenType.toString(type_),
          modifiers->Option.map(
            xs => xs->Array.map(Highlighting__SemanticToken.TokenModifier.toString),
          ),
        )
      })

      SemanticTokensBuilder.build(builder)
    })
    ->(x => Some(x))
  }

  Editor.Provider.registerDocumentSemanticTokensProvider(
    ~provideDocumentSemanticTokens,
    (tokenTypes, tokenModifiers),
  )
}

// TODO: rename `finalize`
let finalize = isRestart => {
  // after the last Agda file has been closed
  if Registry.isEmpty() {
    // destroy views accordingly
    Singleton.Panel.destroy()

    // don't destroy the DebugBuffer singleton if it's a Restart
    if !isRestart {
      Singleton.DebugBuffer.destroy()
    }
  }
}

let activateWithoutContext = (subscriptions, extensionPath, globalStorageUri, memento) => {
  let subscribe = x => subscriptions->Array.push(x)->ignore
  let subscribeMany = xs => subscriptions->Array.pushMany(xs)->ignore
  // Channel for testing, emits events when something has been completed,
  // for example, when the input method has translated a key sequence into a symbol
  let channels = {
    State.inputMethod: Chan.make(),
    responseHandled: Chan.make(),
    commandHandled: Chan.make(),
    log: Chan.make(),
  }
  // subscribe to the logging channel when in debug mode
  let debug = false
  if debug {
    // log the event
    channels.log
    ->Chan.on(message => {
      Js.log(State.Log.toString(message))
    })
    ->ignore
  }

  // on open editor
  Inputs.onOpenEditor(editor => {
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath

    // filter out ".agda.git" files
    if isAgda(fileName) {
      Registry.getState(fileName)->Option.forEach(state => {
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        state.editor = editor
        state.document = editor->VSCode.TextEditor.document
        State__Command.dispatchCommand(state, Refresh)->ignore
      })
    }
  })->subscribe

  // updates the font size accordingly
  VSCode.Workspace.onDidChangeConfiguration((event: VSCode.ConfigurationChangeEvent.t) => {
    // see if the font size has changed
    let agdaModeFontSizeChanged =
      event->VSCode.ConfigurationChangeEvent.affectsConfiguration(
        "agdaMode.buffer.fontSize",
        #Others(None),
      )
    let editorFontSizeChanged =
      event->VSCode.ConfigurationChangeEvent.affectsConfiguration("editor.fontSize", #Others(None))

    // if so, update the font size in the view
    if agdaModeFontSizeChanged || editorFontSizeChanged {
      let newFontSize = Config.Buffer.getFontSize()
      // find any existing State, so that we can update the font size in the view
      switch Registry.getAllStates()[0] {
      | None => ()
      | Some(state) =>
        // panel->WebviewPanel.sendEvent(ConfigurationChange(fontSize))->ignore
        state->State__View.Panel.setFontSize(newFontSize)->Promise.done
      }
    }
  })->subscribe

  // on close editor
  Inputs.onCloseDocument(document => {
    let fileName = VSCode.TextDocument.fileName(document)->Parser.filepath
    if isAgda(fileName) {
      Registry.removeAndDestroy(fileName)->ignore
      finalize(false)->ignore
    }
  })->subscribe

  // on triggering commands - WEB VERSION: Limited command support
  Inputs.onTriggerCommand(async (command, editor) => {
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
    
    // In web version, we show a message for commands that require desktop features
    let showWebLimitationMessage = (commandName) => {
      VSCode.Window.showWarningMessage(
        `The "${commandName}" command requires desktop features (process spawning, file system) that are not available in VS Code for the Web. Please use the desktop version of VS Code for full agda-mode functionality.`,
        []
      )->ignore
    }
    
    // Check if command requires desktop-only features
    let isDesktopOnlyCommand = switch command {
    | Load => true  // Requires process spawning for Agda/ALS
    | Compile => true  // Requires process spawning
    | _ => false
    }
    
    if isDesktopOnlyCommand {
      showWebLimitationMessage(Command.toString(command))
      None
    } else {
      // destroy (only for web-compatible commands)
      switch command {
      | Quit =>
        await Registry.removeAndDestroy(fileName)
        finalize(false)
      | Restart =>
        await Registry.removeAndDestroy(fileName)
        finalize(true)
      | _ => ()
      }
      
      // make (only for web-compatible commands)
      switch command {
      | InputMethod(Activate) =>  // Input method works without desktop features
        switch Registry.getEntry(fileName) {
        | None =>
          let state = initialize(
            channels,
            extensionPath,
            globalStorageUri,
            memento,
            editor,
            fileName,
            None,
          )
          Registry.add(fileName, state)
        | Some(entry) =>
          switch entry.state {
          | None =>
            // State not found, create a new one
            let state = initialize(
              channels,
              extensionPath,
              globalStorageUri,
              memento,
              editor,
              fileName,
              Some(entry.semanticTokens),
            )
            Registry.add(fileName, state)
          | Some(_) => () // should not happen
          }
        }
      | _ => ()  // Most other commands are disabled in web version
      }
      
      // dispatch (only for web-compatible commands)
      switch Registry.getState(fileName) {
      | None => None
      | Some(state) =>
        await State__Command.dispatchCommand(state, command)
        Some(Ok(state))
      }
    }
  })->subscribeMany
  
  // registerDocumentSemanticTokensProvider
  registerDocumentSemanticTokensProvider()->subscribe

  // expose the channel for testing
  channels
}

// this function is the entry point of the whole extension - WEB VERSION
let activate = context => {
  let subscriptions = VSCode.ExtensionContext.subscriptions(context)
  let extensionPath = VSCode.ExtensionContext.extensionPath(context)

  let globalStorageUri =
    context
    ->VSCode.ExtensionContext.globalStorageUri
    ->VSCode.Uri.with_({
      scheme: "file",
    })

  // In web version, we always know we're in browser environment
  Js.Console.log("agda-mode: web environment - desktop features disabled")
  
  // Show information about web limitations on first activation
  VSCode.Window.showInformationMessage(
    "agda-mode is running in web mode. Some features (Agda/ALS integration, file operations) require the desktop version of VS Code. Unicode input method and syntax highlighting are available.",
    []
  )->ignore

  activateWithoutContext(
    subscriptions,
    extensionPath,
    globalStorageUri,
    Some(VSCode.ExtensionContext.workspaceState(context)),
  )
}

let deactivate = () => ()