// The entry module of the whole extension
module Impl = (Editor: Sig.Editor) => {
  module Registry = Registry.Impl(Editor);
  module State = State.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);
  open Belt;

  // if end with '.agda' or '.lagda'
  let isAgda = (filepath): bool => {
    let filepath = filepath->Parser.filepath;
    Js.Re.test_([%re "/\\.agda$|\\.lagda/i"], filepath);
  };

  // invoked by `activate` below, with parameters for mocking the context when testing the extension
  let activateWithoutContext = (disposables, extensionPath) => {
    // expose an EventEmitter for testing, emits events when something has been completed,
    // for example, when the input method has translated a key sequence into a symbol
    Js.log("[ extention ] activate");
    let eventEmitter = Event.make();

    // when a TextEditor gets closed, destroy the corresponding State
    Editor.onDidCloseEditor(fileName =>
      Registry.forceDestroy(fileName)->ignore
    )
    ->Js.Array.push(disposables)
    ->ignore;
    // when a file got renamed, destroy the corresponding State if it becomes something else than Agda file
    Editor.onDidChangeFileName((oldName, newName) =>
      oldName->Option.forEach(oldName =>
        newName->Option.forEach(newName =>
          if (Registry.contains(oldName)) {
            if (isAgda(newName)) {
              Registry.rename(oldName, newName);
            } else {
              Registry.forceDestroy(oldName)->ignore;
            };
          }
        )
      )
    )
    ->Js.Array.push(disposables)
    ->ignore;

    // on editor activation, reveal the corresponding Panel (if any)
    Editor.onDidChangeActivation((prev, next) => {
      prev
      ->Option.flatMap(Registry.getByEditor)
      ->Option.forEach(dispatcher => {State.hide(dispatcher.state)});
      next
      ->Option.flatMap(Registry.getByEditor)
      ->Option.forEach(dispatcher => {
          next
          ->Option.forEach(editor => {
              // Issue #8
              // after switching tabs, the old editor would be "_disposed"
              // we need to replace it with this new one
              dispatcher.state.editor = editor;
              State.show(dispatcher.state);
              Dispatcher.dispatchCommand(dispatcher, Refresh)->ignore;
            })
          ->ignore
        });
    })
    ->Js.Array.push(disposables)
    ->ignore;

    // helper function for initializing a Dispatcher
    let makeAndAddToRegistry = (editor, fileName) => {
      // not in the Registry, instantiate a Dispatcher
      let dispatcher =
        Dispatcher.make(
          extensionPath,
          editor,
          () => {Registry.forceDestroy(fileName)->ignore},
          eventEmitter,
        );
      // add this dispatcher to the Registry
      Registry.add(fileName, dispatcher);
    };

    // on trigger command
    Command.names->Array.forEach(((command, name)) => {
      Editor.registerCommand(name, (editor, fileName) =>
        if (isAgda(fileName)) {
          Js.log("[ command ] " ++ name);
          // Commands like "Load", "InputMethod", "Quit", and "Restart" act on the Registry
          (
            switch (command) {
            | Load
            | InputMethod(Activate) =>
              switch (Registry.get(fileName)) {
              | None => makeAndAddToRegistry(editor, fileName)
              | Some(_) =>
                // already in the Registry, do nothing
                ()
              };
              Promise.resolved();
            | Quit => Registry.forceDestroy(fileName)
            | Restart =>
              Registry.destroy(fileName)
              ->Promise.map(() => makeAndAddToRegistry(editor, fileName))
            | _ => Promise.resolved()
            }
          )
          ->Promise.flatMap(() => {
              // dispatch Tasks
              switch (Registry.get(fileName)) {
              | None => Promise.resolved()
              | Some(dispatcher) =>
                Dispatcher.dispatchCommand(dispatcher, command)
              }
            });
        } else {
          Promise.resolved();
        }
      )
      ->Js.Array.push(disposables)
      ->ignore
    });

    // for testing
    eventEmitter;
  };

  // this function is the entry point of the whole extension
  let activate = context => {
    let disposables = Editor.getDisposables(context);
    let extensionPath = Editor.getExtensionPath(context);
    activateWithoutContext(disposables, extensionPath);
  };

  let deactivate = () => {
    Js.log("[ extention ] deactivate");
    Registry.destroyAll();
  };
};

include Impl(SigImpl);
