open Belt;
open VSCode;

module StateDispatcherPair = {
  module Impl = (Editor: Sig.Editor) => {
    module Dispatcher = Dispatcher.Impl(Editor);
    module State = State.Impl(Editor);
    type t = (State.t, Dispatcher.t);

    let make = (context, editor, onDestroy) => {
      // not in the States dict, instantiate one new
      let state = State.make(context, editor);
      let dispatcher = Dispatcher.make();

      // listens to events from the view
      state.view
      ->Editor.View.on(
          fun
          | Event(event) => {
              Dispatcher.dispatchCommand(
                dispatcher,
                state,
                EventFromView(event),
              )
              ->ignore;
            }
          | Response(_) => (),
        )
      ->Editor.addToSubscriptions(context);

      // listens to events from the input method
      state.inputMethod.onAction.on(action => {
        Dispatcher.dispatchCommand(
          dispatcher,
          state,
          Command.InputMethod(action),
        )
        ->ignore
      })
      ->Editor.Disposable.make
      ->Editor.addToSubscriptions(context);

      // remove it from the States dict if it request to be killed
      state->State.onKillMePlz->Promise.get(onDestroy);

      // return the pair
      (state, dispatcher);
    };

    let destroy = ((state, dispatcher)) =>
      Dispatcher.destroy(dispatcher)
      ->Promise.flatMap(() => {State.destroy(state)});

    let forceDestroy = ((state, dispatcher)) =>
      Dispatcher.forceDestroy(dispatcher)
      ->Promise.flatMap(() => {State.destroy(state)});
  };
};

// a dictionary of FileName-StateDispatcherPair entries
module StateDict = {
  module Impl = (Editor: Sig.Editor) => {
    module StateDispatcherPair = StateDispatcherPair.Impl(Editor);
    let dict: Js.Dict.t(StateDispatcherPair.t) = Js.Dict.empty();

    let get = fileName => dict->Js.Dict.get(fileName);

    let getByEditor = (editor: Editor.editor) =>
      editor->Editor.getFileName->Option.flatMap(get);

    // do nothing if the state already exists
    let add = (fileName, state) => {
      switch (get(fileName)) {
      | Some(_) => ()
      | None => dict->Js.Dict.set(fileName, state)
      };
    };

    let rename = (oldName, newName) => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      get(oldName)
      ->Option.forEach(state => {
          Js.log3("[ states ][ rename ]", oldName, newName);
          delete_(dict, oldName);
          add(newName, state);
        });
    };

    // remove the entry (without triggering .destroy() )
    let remove = fileName => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      delete_(dict, fileName);
    };

    let destroy = fileName => {
      switch (get(fileName)) {
      | None => Promise.resolved()
      | Some(pair) =>
        remove(fileName);
        StateDispatcherPair.destroy(pair);
      };
    };

    let forceDestroy = fileName => {
      switch (get(fileName)) {
      | None => Promise.resolved()
      | Some(pair) =>
        remove(fileName);
        StateDispatcherPair.forceDestroy(pair);
      };
    };

    let contains = fileName => get(fileName)->Option.isSome;

    let destroyAll = () => {
      dict
      ->Js.Dict.entries
      ->Array.map(((_, pair), ()) => StateDispatcherPair.destroy(pair))
      ->Util.oneByOne;
    };
  };
};

module Impl = (Editor: Sig.Editor) => {
  module StateDispatcherPair = StateDispatcherPair.Impl(Editor);
  module States = StateDict.Impl(Editor);
  module State = State.Impl(Editor);
  module Dispatcher = Dispatcher.Impl(Editor);

  let addToSubscriptions = (f, context) =>
    f->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;

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
      ->Option.flatMap(States.get)
      ->Option.forEach(((state, _)) => {State.hide(state)});
      next
      ->Option.flatMap(States.get)
      ->Option.forEach(_ => {
          next
          ->Option.forEach(fileName => {
              States.destroy(fileName)
              ->Promise.flatMap(() => {fileName->Editor.getEditor})
              ->Promise.get(editor => {
                  Js.log(editor);
                  // not in the States dict, instantiate a pair of (State, Dispatcher)
                  let pair =
                    StateDispatcherPair.make(context, editor, () => {
                      States.forceDestroy(fileName)->ignore
                    });
                  // add this (State, Dispatcher) pair to the dict
                  States.add(fileName, pair);

                  // dispatch Tasks
                  editor
                  ->States.getByEditor
                  ->Option.forEach(((state, dispatcher)) => {
                      Dispatcher.dispatchCommand(dispatcher, state, Load)
                      ->ignore
                    });
                })
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
              ->Option.forEach(((state, dispatcher)) => {
                  Dispatcher.dispatchCommand(dispatcher, state, command)
                  ->ignore
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
