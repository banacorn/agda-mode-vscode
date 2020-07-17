module Impl = (Editor: Sig.Editor) => {
  module States = States.Impl(Editor);
  module State = State.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);
  open Belt;

  let activate = context => {
    Js.log("[ states ] activate");
    // when a TextEditor gets closed, destroy the corresponding State
    Editor.onDidCloseEditor(fileName => States.forceDestroy(fileName)->ignore)
    ->Editor.addToSubscriptions(context);
    // when a file got renamed, destroy the corresponding State if it becomes non-GCL
    Editor.onDidChangeFileName((oldName, newName) =>
      oldName->Option.forEach(oldName =>
        newName->Option.forEach(newName =>
          if (States.contains(oldName)) {
            if (Editor.isAgda(newName)) {
              States.rename(oldName, newName);
            } else {
              States.forceDestroy(oldName)->ignore;
            };
          }
        )
      )
    )
    ->Editor.addToSubscriptions(context);

    // on editor activation, reveal the corresponding Panel (if any)
    Editor.onDidChangeActivation((prev, next) => {
      prev
      ->Option.flatMap(States.getByEditor)
      ->Option.forEach(dispatcher => {State.hide(dispatcher.state)});
      next
      ->Option.flatMap(States.getByEditor)
      ->Option.forEach(dispatcher => {
          next
          ->Option.forEach(editor => {
              // Issue #8
              // after switching tabs, the old editor would be "_disposed"
              // we need to replace it with this new one
              dispatcher.state.editor = editor;
              State.show(dispatcher.state);
              Dispatcher.dispatchCommand(dispatcher, Refresh);
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
                  switch (States.get(fileName)) {
                  | None =>
                    let extentionPath = Editor.getExtensionPath(context);
                    // not in the States dict, instantiate a pair of (State, Dispatcher)
                    let pair =
                      Dispatcher.make(extentionPath, editor, () => {
                        States.forceDestroy(fileName)->ignore
                      });
                    // add this (State, Dispatcher) pair to the dict
                    States.add(fileName, pair);
                  | Some(_pair) =>
                    // already in the States dict, do nothing
                    ()
                  }
                });
              Promise.resolved();
            | Quit =>
              editor
              ->Editor.getFileName
              ->Option.mapWithDefault(Promise.resolved(), fileName => {
                  States.forceDestroy(fileName)
                })
            | Restart =>
              editor
              ->Editor.getFileName
              ->Option.mapWithDefault(Promise.resolved(), fileName => {
                  States.destroy(fileName)
                  ->Promise.map(() => {
                      let extentionPath = Editor.getExtensionPath(context);
                      // not in the States dict, instantiate a pair of (State, Dispatcher)
                      let pair =
                        Dispatcher.make(extentionPath, editor, () => {
                          States.forceDestroy(fileName)->ignore
                        });
                      // add this (State, Dispatcher) pair to the dict
                      States.add(fileName, pair);
                    })
                })
            | _ => Promise.resolved()
            }
          )
          ->Promise.get(() => {
              // dispatch Tasks
              editor
              ->States.getByEditor
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
    States.destroyAll();
  };
};

include Impl(Editor);
