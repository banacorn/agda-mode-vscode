open Belt;
open VSCode;

// a dictionary of FileName-State entries
module StateDict = {
  module Impl = (Editor: Sig.Editor) => {
    module Scheduler = Scheduler.Impl(Editor);
    module State = State.Impl(Editor);
    let dict: Js.Dict.t((State.t, Scheduler.t)) = Js.Dict.empty();

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

    // remove the entry (but without triggering .destroy() )
    let remove = fileName => {
      let delete_: (Js.Dict.t('a), string) => unit = [%raw
        "function (dict, key) {delete dict[key]}"
      ];
      delete_(dict, fileName);
    };
    let destroy = fileName => {
      get(fileName)
      ->Option.forEach(((state, runner)) => {
          Js.log("[ states ][ destroy ]");
          State.destroy(state) |> ignore;
          Scheduler.destroy(runner) |> ignore;
        });
      remove(fileName);
    };

    let contains = fileName => get(fileName)->Option.isSome;

    let destroyAll = () => {
      dict
      ->Js.Dict.entries
      ->Array.forEach(((_, (state, runner))) => {
          State.destroy(state) |> ignore;
          Scheduler.destroy(runner) |> ignore;
        });
    };
  };
};

module Impl = (Editor: Sig.Editor) => {
  module States = StateDict.Impl(Editor);
  module State = State.Impl(Editor);
  module TaskRunner = Scheduler.Impl(Editor);

  let addToSubscriptions = (f, context) =>
    f->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;

  let activate = context => {
    Js.log("[ states ] activate");
    // when a TextEditor gets closed, destroy the corresponding State
    Editor.onDidCloseEditor(States.destroy)
    ->Editor.addToSubscriptions(context);
    // when a file got renamed, destroy the corresponding State if it becomes non-GCL
    Editor.onDidChangeFileName((oldName, newName) =>
      oldName->Option.forEach(oldName =>
        newName->Option.forEach(newName =>
          if (States.contains(oldName)) {
            if (Editor.isAgda(newName)) {
              States.rename(oldName, newName);
            } else {
              States.destroy(oldName);
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
      ->Option.forEach(((state, _)) => State.hide(state));
      next
      ->Option.flatMap(States.get)
      ->Option.forEach(((state, _)) => State.show(state));
    })
    ->Editor.addToSubscriptions(context);

    // on trigger command
    Command.names->Array.forEach(((command, name)) => {
      Editor.registerCommand(
        name,
        editor => {
          Js.log("[ command ] " ++ name);
          // special treatments on commands like "Load" and "Quit"
          switch (command) {
          | Load =>
            editor
            ->Editor.getFileName
            ->Option.forEach(fileName => {
                switch (States.get(fileName)) {
                | None =>
                  // not in the States dict, instantiate one new
                  let state = State.make(context, editor);
                  let taskRunner = TaskRunner.make(state);

                  // listens to events from the view
                  state.view
                  ->Editor.View.on(event => {
                      TaskRunner.dispatchCommand(
                        taskRunner,
                        ViewEvent(event),
                      )
                      ->ignore
                    })
                  ->Editor.addToSubscriptions(context);

                  // listens to events from the input method
                  state.onInputMethodAction.on(action => {
                    TaskRunner.dispatchCommand(
                      taskRunner,
                      Command.InputSymbol(action),
                    )
                    ->ignore
                  })
                  ->Editor.Disposable.make
                  ->Editor.addToSubscriptions(context);

                  // remove it from the States dict if it got destroyed
                  state
                  ->State.onceDestroyed
                  ->Promise.get(() => {States.destroy(fileName)});

                  // add this new constructed State and TaskRunner to the dict
                  States.add(fileName, (state, taskRunner));
                | Some(_state) =>
                  // already in the States dict, do nothing
                  ()
                }
              })
          | _ => ()
          };
          // dispatch Tasks
          editor
          ->States.getByEditor
          ->Option.forEach(((_state, runner)) => {
              switch (command) {
              | Escape => TaskRunner.interrupt(runner, Escape)->ignore
              | others => TaskRunner.dispatchCommand(runner, others)->ignore
              }
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