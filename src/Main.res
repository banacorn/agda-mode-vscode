open Belt

// if end with '.agda' or '.lagda'
let isAgda = (fileName): bool => {
  let fileName = fileName->Parser.filepath
  Js.Re.test_(%re("/\\.agda$|\\.lagda/i"), fileName)
}

module PanelSingleton: {
  // methods
  let make: string => ViewController.t
  let destroy: unit => unit
} = {
  let handle: ref<option<ViewController.t>> = ref(None)
  let make = extensionPath =>
    switch handle.contents {
    | None =>
      let panel = ViewController.make(extensionPath)
      handle := Some(panel)
      // free the handle when the view has been forcibly destructed
      ViewController.onceDestroyed(panel)->Promise.get(() => {
        handle := None
      })
      panel
    | Some(panel) => panel
    }

  let destroy = () => {
    handle.contents->Option.forEach(ViewController.destroy)
    handle := None
  }
}

module Inputs: {
  let onOpenEditor: (VSCode.TextEditor.t => unit) => VSCode.Disposable.t
  let onCloseDocument: (VSCode.TextDocument.t => unit) => VSCode.Disposable.t
  let onTriggerCommand: ((Command.t, VSCode.TextEditor.t) => Promise.t<unit>) => array<
    VSCode.Disposable.t,
  >
} = {
  let onOpenEditor = callback => {
    VSCode.Window.activeTextEditor->Option.forEach(callback)
    VSCode.Window.onDidChangeActiveTextEditor(.next => next->Option.forEach(callback))
  }
  let onCloseDocument = callback => VSCode.Workspace.onDidCloseTextDocument(. callback)
  // invoke the callback when:
  //  1. the triggered command has prefix "agda-mode."
  //  2. there's an active text edtior
  //  3. the active text editor is an Agda file
  let onTriggerCommand = callback => {
    Command.names->Array.map(((command, name)) =>
      VSCode.Commands.registerCommand("agda-mode." ++ name, () => {
        VSCode.Window.activeTextEditor->Option.map(editor => {
          let fileName =
            editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName->Parser.filepath
          if isAgda(fileName) {
            callback(command, editor)
          } else {
            Promise.resolved()
          }
        })
      })
    )
  }
}

let initialize = (debugChan, extensionPath, globalStoragePath, editor, fileName) => {
  if Registry.isEmpty() {
    // keybinding: so that most of the commands will work only after agda-mode:load
    VSCode.Commands.setContext("agdaMode", true)->ignore
  }

  let view = PanelSingleton.make(extensionPath)
  ViewController.onceDestroyed(view)->Promise.get(() => Registry.removeAndDestroyAll()->ignore)

  // not in the Registry, instantiate a State
  let state = State.make(debugChan, globalStoragePath, editor, view)

  // remove it from the Registry if it requests to be destroyed
  state.onRemoveFromRegistry->Chan.once->Promise.get(() => Registry.remove(fileName))

  ////////////////////////////////////////////////////////////////
  // input events
  ////////////////////////////////////////////////////////////////

  let subscribe = disposable => disposable->Js.Array.push(state.subscriptions)->ignore

  let getCurrentEditor = () =>
    switch VSCode.Window.activeTextEditor {
    | Some(editor) => Some(editor)
    | None => VSCode.Window.visibleTextEditors[0]
    }

  state.view
  ->ViewController.onEvent(event => {
    switch getCurrentEditor() {
    | Some(editor') =>
      let fileName' = editor'->VSCode.TextEditor.document->VSCode.TextDocument.fileName
      if fileName' == fileName {
        State__Command.dispatchCommand(state, Command.EventFromView(event))->ignore
      }
    | None => ()
    }
  })
  ->subscribe

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
  // Editor.Provider.registerHoverProvider((fileName, point) => {
  //   // only provide source location, when the filename matched
  //   let currentFileName = state.document->VSCode.TextDocument.fileName->Parser.filepath

  //   if fileName == currentFileName {
  //     let range = VSCode.Range.make(point, point)
  //     Some(Promise.resolved(([""], range)))
  //   } else {
  //     None
  //   }
  // })->subscribe

  // these two arrays are called "legends"
  // let tokenTypes = Highlighting.Aspect.TokenType.enumurate
  // let tokenModifiers = Highlighting.Aspect.TokenModifier.enumurate

  // Editor.Provider.registerSemnaticTokenProvider((fileName, pushToken) => {
  //   let useSemanticHighlighting = Config.getSemanticHighlighting()
  //   let document = VSCode.TextEditor.document(editor)
  //   let currentFileName = document->VSCode.TextDocument.fileName->Parser.filepath

  //   if useSemanticHighlighting && fileName == currentFileName {
  //     Some(Decoration.generateSemanticTokens(state.decoration, state.editor, pushToken))
  //   } else {
  //     None
  //   }
  // }, (tokenTypes, tokenModifiers))->subscribe

  // add this state to the Registry
  state
}

// TODO: rename `finalize`
let finalize = () => {
  // after the last Agda file has benn closed
  if Registry.isEmpty() {
    // keybinding: disable most of the command bindings
    VSCode.Commands.setContext("agdaMode", false)->ignore
    // deactivate the view accordingly
    PanelSingleton.destroy()
  }
  Promise.resolved()
}

let activateWithoutContext = (subscriptions, extensionPath, globalStoragePath) => {
  let subscribe = x => x->Js.Array.push(subscriptions)->ignore
  let subscribeMany = xs => xs->Js.Array.pushMany(subscriptions)->ignore
  // Channel for testing, emits events when something has been completed,
  // for example, when the input method has translated a key sequence into a symbol
  let debugChan = Chan.make()

  // on open editor
  Inputs.onOpenEditor(editor => {
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    // filter out ".agda.git" files
    if isAgda(fileName) {
      Registry.get(fileName)->Option.forEach(state => {
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        state.editor = editor
        state.document = editor->VSCode.TextEditor.document
        //
        State__Command.dispatchCommand(state, Refresh)->ignore
      })
    }
  })->subscribe

  // on close editor
  Inputs.onCloseDocument(document => {
    let fileName = VSCode.TextDocument.fileName(document)
    if isAgda(fileName) {
      Registry.removeAndDestroy(fileName)->ignore
      finalize()->ignore
    }
  })->subscribe

  // on triggering commands
  Inputs.onTriggerCommand((command, editor) => {
    // Js.log("[ command ] " ++ Command.toString(command))
    let fileName = editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    // destroy
    switch command {
    | Quit
    | Restart =>
      Registry.removeAndDestroy(fileName)->Promise.flatMap(finalize)
    | _ => Promise.resolved()
    }
    // make
    ->Promise.tap(() =>
      switch command {
      | Load
      | Restart
      | InputMethod(Activate) =>
        switch Registry.get(fileName) {
        | None =>
          let state = initialize(debugChan, extensionPath, globalStoragePath, editor, fileName)
          Registry.add(fileName, state)
        | Some(_) => () // already in the Registry, do nothing
        }
      | _ => ()
      }
    )
    // dispatch
    ->Promise.flatMap(() => {
      switch Registry.get(fileName) {
      | None => Promise.resolved()
      | Some(state) => State__Command.dispatchCommand(state, command)
      }
    })
  })->subscribeMany

  // expose the channel for testing
  debugChan
}

// this function is the entry point of the whole extension
let activate = context => {
  let subscriptions = VSCode.ExtensionContext.subscriptions(context)
  let extensionPath = VSCode.ExtensionContext.extensionPath(context)
  let globalStoragePath = VSCode.ExtensionContext.globalStoragePath(context)

  activateWithoutContext(subscriptions, extensionPath, globalStoragePath)
}

let deactivate = () => ()
