// The entry module of the whole extension
module Impl = (Editor: Sig.Editor) => {
  module Registry = Registry.Impl(Editor);
  module State = State.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);
  open Belt;

  let activate = context => {
    Js.log("[ extention ] activate");
    // when a TextEditor gets closed, destroy the corresponding State
    Editor.onDidCloseEditor(fileName =>
      Registry.forceDestroy(fileName)->ignore
    )
    ->Editor.addToSubscriptions(context);
    // when a file got renamed, destroy the corresponding State if it becomes something else than Agda file
    Editor.onDidChangeFileName((oldName, newName) =>
      oldName->Option.forEach(oldName =>
        newName->Option.forEach(newName =>
          if (Registry.contains(oldName)) {
            if (Editor.isAgda(newName)) {
              Registry.rename(oldName, newName);
            } else {
              Registry.forceDestroy(oldName)->ignore;
            };
          }
        )
      )
    )
    ->Editor.addToSubscriptions(context);

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
    ->Editor.addToSubscriptions(context);
    Editor.getExtensionPath(context)->Js.log;
    // on trigger command
    Command.names->Array.forEach(((command, name)) => {
      Editor.registerCommand(
        name,
        editor => {
          Js.log("[ command ] " ++ name);
          // special treatments on commands like "Load", "Quit" and "Restart"
          (
            switch (command) {
            | Load =>
              editor
              ->Editor.getFileName
              ->Option.forEach(fileName => {
                  switch (Registry.get(fileName)) {
                  | None =>
                    let extentionPath = Editor.getExtensionPath(context);
                    // not in the Registry, instantiate a Dispatcher
                    let dispatcher =
                      Dispatcher.make(extentionPath, editor, () => {
                        Registry.forceDestroy(fileName)->ignore
                      });
                    // add this dispatcher to the Registry
                    Registry.add(fileName, dispatcher);
                  | Some(_) =>
                    // already in the Registry, do nothing
                    ()
                  }
                });
              Promise.resolved();
            | Quit =>
              editor
              ->Editor.getFileName
              ->Option.mapWithDefault(Promise.resolved(), fileName => {
                  Registry.forceDestroy(fileName)
                })
            | Restart =>
              editor
              ->Editor.getFileName
              ->Option.mapWithDefault(Promise.resolved(), fileName => {
                  Registry.destroy(fileName)
                  ->Promise.map(() => {
                      let extentionPath = Editor.getExtensionPath(context);
                      // not in the Registry, instantiate a Dispatcher
                      let dispatcher =
                        Dispatcher.make(extentionPath, editor, () => {
                          Registry.forceDestroy(fileName)->ignore
                        });
                      // add this dispatcher to the Registry
                      Registry.add(fileName, dispatcher);
                    })
                })
            | _ => Promise.resolved()
            }
          )
          ->Promise.get(() => {
              // dispatch Tasks
              editor
              ->Registry.getByEditor
              ->Option.forEach(dispatcher => {
                  Dispatcher.dispatchCommand(dispatcher, command)->ignore
                })
            });
        },
      )
      ->Editor.addToSubscriptions(context)
    });
  };

  let deactivate = () => {
    Js.log("[ extention ] deactivate");
    Registry.destroyAll();
  };
};

include Impl(Editor);
