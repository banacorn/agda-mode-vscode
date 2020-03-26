open Belt;
open Vscode;

module States = {
  let dict: Js.Dict.t(State.t) = Js.Dict.empty();

  let get = editor => {
    dict->Js.Dict.get(editor.TextEditor.document.fileName);
  };

  let getByFileName = fileName => {
    dict->Js.Dict.get(fileName);
  };

  let set = (fileName, state) => {
    // update the dict
    dict->Js.Dict.set(fileName, state);
  };

  // see if an TextEditor has been loaded
  let isLoaded = editor => {
    dict->Js.Dict.get(editor.TextEditor.document.fileName)->Option.isSome;
  };

  let dispose = () => {
    dict->Js.Dict.entries->Array.map(((_, state)) => State.dispose(state));
  };
};

let getState = context =>
  Window.activeTextEditor->Option.map(editor =>
    switch (States.get(editor)) {
    | None =>
      let state = State.make(context, editor);
      States.set(editor.document.fileName, state);
      state;
    | Some(state) => state
    }
  );

let activate = (context: ExtensionContext.t) => {
  // when a TextEditor gets activated, reveal the corresponding Panel (if any)
  Window.onDidChangeActiveTextEditor(editor => {
    editor->Option.flatMap(States.get)->Option.forEach(View.activate)
  })
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  // when a TextEditor gets closed, delete the corresponding Panel (if any)
  Workspace.onDidCloseTextDocument(textDoc => {
    textDoc
    ->TextDocument.fileName
    ->States.getByFileName
    ->Option.forEach(State.dispose)
  })
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  Commands.registerCommand("extension.load", () =>
    getState(context)->Option.forEach(Command.load)
  )
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;
};

let deactive = () => {
  States.dispose();
};