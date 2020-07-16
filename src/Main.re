module Impl = (Editor: Sig.Editor) => {
  module StateDispatcherPair = States.StateDispatcherPair.Impl(Editor);
  module States = States.Dict.Impl(Editor);
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
      ->Option.forEach(((state, _)) => {State.hide(state)});
      next
      ->Option.flatMap(States.getByEditor)
      ->Option.forEach(((state, dispatcher)) => {
          next
          ->Option.forEach(editor => {
              // Issue #8
              // after switching tabs, the old editor would be "_disposed"
              // we need to replace it with this new one
              state.editor = editor;
              State.show(state);
              Dispatcher.dispatchCommand(dispatcher, Refresh);
            })
          ->ignore
        });
    })
    ->Editor.addToSubscriptions(context);

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
                    // not in the States dict, instantiate a pair of (State, Dispatcher)
                    let pair =
                      StateDispatcherPair.make(context, editor, () => {
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
                      // not in the States dict, instantiate a pair of (State, Dispatcher)
                      let pair =
                        StateDispatcherPair.make(context, editor, () => {
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
              ->Option.forEach(((_state, dispatcher)) => {
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
