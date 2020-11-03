open Belt;

// FileName-Dispatcher bookkeeping
module Impl = (Editor: Sig.Editor) => {
  module Dispatcher = Dispatcher.Impl(Editor);
  let dict: Js.Dict.t(Dispatcher.t) = Js.Dict.empty();

  let get = fileName => dict->Js.Dict.get(fileName);

  let getByEditor = (editor: Editor.editor) =>
    editor->Editor.getDocument->Editor.getFileName->Option.flatMap(get);

  // do nothing if the state already exists
  let add = (fileName, dispatcher) => {
    switch (get(fileName)) {
    | Some(_) => ()
    | None => dict->Js.Dict.set(fileName, dispatcher)
    };
  };

  let rename = (oldName, newName) => {
    let delete_: (Js.Dict.t('a), string) => unit = [%raw
      "function (dict, key) {delete dict[key]}"
    ];
    get(oldName)
    ->Option.forEach(dispatcher => {
        Js.log3("[ states ][ rename ]", oldName, newName);
        delete_(dict, oldName);
        add(newName, dispatcher);
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
    | Some(dispatcher) =>
      remove(fileName);
      Dispatcher.destroy(dispatcher);
    };
  };

  let forceDestroy = fileName => {
    switch (get(fileName)) {
    | None => Promise.resolved()
    | Some(dispatcher) =>
      remove(fileName);
      Dispatcher.forceDestroy(dispatcher);
    };
  };

  let contains = fileName => get(fileName)->Option.isSome;
};
