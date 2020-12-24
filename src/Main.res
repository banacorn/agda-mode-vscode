// The entry module of the whole extension
open Belt

// if end with '.agda' or '.lagda'
let isAgda = (filepath): bool => {
  let filepath = filepath->Parser.filepath
  Js.Re.test_(%re("/\\.agda$|\\.lagda/i"), filepath)
}

// invoked by `activate` below, with parameters for mocking the context when testing the extension
let activateWithoutContext = disposables => {
  // expose an Chan for testing, emits events when something has been completed,
  // for example, when the input method has translated a key sequence into a symbol
  Js.log("[ extention ] activate")
  let chan = Chan.make()

  // when a TextEditor gets closed, destroy the corresponding State
  VSCode.Workspace.onDidCloseTextDocument(.textDoc =>
    Registry.destroy(textDoc->VSCode.TextDocument.fileName)->ignore
  )
  ->Js.Array.push(disposables)
  ->ignore
  // when a file got renamed, destroy the corresponding State if it becomes something else than Agda file
  VSCode.Workspace.onDidRenameFiles(.event =>
    event
    ->VSCode.FileRenameEvent.files
    ->Array.forEach(file => {
      let oldName = file["oldUri"]->VSCode.Uri.path
      let newName = file["newUri"]->VSCode.Uri.path
      if Registry.contains(oldName) {
        if isAgda(newName) {
          Registry.rename(oldName, newName)
        } else {
          Registry.destroy(oldName)->ignore
        }
      }
    })
  )
  ->Js.Array.push(disposables)
  ->ignore

  // on editor activation, reveal the corresponding Panel (if any)
  // TODO: refactor this
  let onDidChangeActivation = callback => {
    let previous = ref(VSCode.Window.activeTextEditor)

    VSCode.Window.onDidChangeActiveTextEditor(.next =>
      if (
        next
        ->Option.map(VSCode.TextEditor.document)
        ->Option.map(document => document->VSCode.TextDocument.fileName->Parser.filepath) !=
          previous.contents
          ->Option.map(VSCode.TextEditor.document)
          ->Option.map(document => document->VSCode.TextDocument.fileName->Parser.filepath)
      ) {
        callback(previous.contents, next)
        previous := next
      }
    )
  }
  onDidChangeActivation((_prev, next) => {
    // prev->Option.flatMap(Registry.getByEditor)->Option.forEach(State.View.hide)
    next
    ->Option.flatMap(Registry.getByEditor)
    ->Option.forEach(state =>
      next
      ->Option.forEach(editor => {
        // Issue #8
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        state.editor = editor
        State.View.reveal()
        State__Command.dispatchCommand(state, Refresh)->ignore
      })
      ->ignore
    )
  })
  ->Js.Array.push(disposables)
  ->ignore

  // helper function for initializing a State
  let makeAndAddToRegistry = (editor, fileName) => {
    // not in the Registry, instantiate a State
    let state = State.make(chan, editor)

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
    state.onRemoveFromRegistry->Chan.once->Promise.flatMap(() => Registry.destroy(fileName))->ignore

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

  // on trigger command

  let registerCommand = (name, callback) =>
    VSCode.Commands.registerCommand("agda-mode." ++ name, () =>
      VSCode.Window.activeTextEditor->Option.map(editor => {
        let fileName =
          editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
        callback(editor, fileName)
      })
    )

  Command.names->Array.forEach(((command, name)) =>
    registerCommand(name, (editor, fileName) =>
      if isAgda(fileName) {
        Js.log("[ command ] " ++ name)
        // Commands like "Load", "InputMethod", "Quit", and "Restart" act on the Registry
        switch command {
        | Load
        | InputMethod(Activate) =>
          switch Registry.get(fileName) {
          | None => makeAndAddToRegistry(editor, fileName)
          | Some(_) => // already in the Registry, do nothing
            ()
          }
          Promise.resolved()
        | Quit => Registry.destroy(fileName)
        | Restart =>
          Registry.destroy(fileName)->Promise.map(() => makeAndAddToRegistry(editor, fileName))
        | _ => Promise.resolved()
        }->Promise.flatMap(() =>
          switch Registry.get(fileName) {
          | None => Promise.resolved()
          | Some(state) => State__Command.dispatchCommand(state, command)
          }
        )
      } else {
        Promise.resolved()
      }
    )
    ->Js.Array.push(disposables)
    ->ignore
  )

  // for testing
  chan
}

// this function is the entry point of the whole extension
let activate = context => {
  let disposables = context->VSCode.ExtensionContext.subscriptions
  activateWithoutContext(disposables)
}
