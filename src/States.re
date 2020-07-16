open Belt;

module StateDispatcherPair = {
  module Impl = (Editor: Sig.Editor) => {
    module Dispatcher = Dispatcher.Impl(Editor);
    module State = State.Impl(Editor);
    type t = (State.t, Dispatcher.t);

    let make = (extentionPath, editor, onDestroy) => {
      // not in the States dict, instantiate one new
      let state = State.make(extentionPath, editor);
      let dispatcher = Dispatcher.make(state);

      // listens to events from the view
      state.view
      ->Editor.View.on(
          fun
          | Event(event) => {
              Dispatcher.dispatchCommand(dispatcher, EventFromView(event))
              ->ignore;
            }
          | Response(_) => (),
        )
      ->Js.Array.push(state.subscriptions)
      ->ignore;

      // listens to events from the input method
      state.inputMethod.onAction.on(action => {
        Dispatcher.dispatchCommand(dispatcher, Command.InputMethod(action))
        ->ignore
      })
      ->Editor.Disposable.make
      ->Js.Array.push(state.subscriptions)
      ->ignore;

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
module Dict = {
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
