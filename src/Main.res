// The entry module of the whole extension
open Belt

// if end with '.agda' or '.lagda'
let isAgda = (filepath): bool => {
  let filepath = filepath->Parser.filepath
  Js.Re.test_(%re("/\\.agda$|\\.lagda/i"), filepath)
}

// invoked by `activate` below, with parameters for mocking the context when testing the extension
let activateWithoutContext = (disposables, extensionPath) => {
  // expose an EventEmitter for testing, emits events when something has been completed,
  // for example, when the input method has translated a key sequence into a symbol
  Js.log("[ extention ] activate")
  let eventEmitter = Chan.make()

  // when a TextEditor gets closed, destroy the corresponding State
  VSCode.Workspace.onDidCloseTextDocument(.textDoc =>
    Registry.forceDestroy(textDoc->VSCode.TextDocument.fileName)->ignore
  )
  ->Js.Array.push(disposables)
  ->ignore
  // when a file got renamed, destroy the corresponding State if it becomes something else than Agda file
  VSCode.Workspace.onDidRenameFiles(.event =>
    event->VSCode.FileRenameEvent.files->Array.forEach(file => {
      let oldName = file["oldUri"]->VSCode.Uri.path
      let newName = file["newUri"]->VSCode.Uri.path
      if Registry.contains(oldName) {
        if isAgda(newName) {
          Registry.rename(oldName, newName)
        } else {
          Registry.forceDestroy(oldName)->ignore
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
  onDidChangeActivation((prev, next) => {
    prev
    ->Option.flatMap(Registry.getByEditor)
    ->Option.forEach(dispatcher => State.hide(dispatcher.state))
    next
    ->Option.flatMap(Registry.getByEditor)
    ->Option.forEach(dispatcher => next->Option.forEach(editor => {
        // Issue #8
        // after switching tabs, the old editor would be "_disposed"
        // we need to replace it with this new one
        dispatcher.state.editor = editor
        State.show(dispatcher.state)
        Dispatcher.dispatchCommand(dispatcher, Refresh)->ignore
      })->ignore)
  })->Js.Array.push(disposables)->ignore

  // helper function for initializing a Dispatcher
  let makeAndAddToRegistry = (editor, fileName) => {
    // not in the Registry, instantiate a Dispatcher
    let dispatcher = Dispatcher.make(
      extensionPath,
      editor,
      () => Registry.forceDestroy(fileName)->ignore,
      eventEmitter,
    )
    // add this dispatcher to the Registry
    Registry.add(fileName, dispatcher)
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

  Command.names->Array.forEach(((command, name)) => registerCommand(name, (editor, fileName) =>
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
        | Quit => Registry.forceDestroy(fileName)
        | Restart =>
          Registry.destroy(fileName)->Promise.map(() => makeAndAddToRegistry(editor, fileName))
        | _ => Promise.resolved()
        }->Promise.flatMap(() =>
          switch // dispatch Tasks
          Registry.get(fileName) {
          | None => Promise.resolved()
          | Some(dispatcher) => Dispatcher.dispatchCommand(dispatcher, command)
          }
        )
      } else {
        Promise.resolved()
      }
    )->Js.Array.push(disposables)->ignore)

  // for testing
  eventEmitter
}

// this function is the entry point of the whole extension
let activate = context => {
  let disposables = context->VSCode.ExtensionContext.subscriptions
  let extensionPath = context->VSCode.ExtensionContext.extensionPath
  activateWithoutContext(disposables, extensionPath)
}
