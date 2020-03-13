open Belt;
// AgdaMode.Process.PathSearch.run("agda")
// ->Promise.get(
//     fun
//     | Error(e) => {
//         let (_, msg) = AgdaMode.Process.PathSearch.Error.toString(e);
//         Window.showInformationMessage(msg);
//       }
//     | Ok(_path) => {},
//   );
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

let load = () => {
  Window.activeTextEditor->Option.forEach(editor =>
    switch (States.get(editor)) {
    | None =>
      let state = State.make(editor);
      States.set(editor.document.fileName, state);
      View.activate(state);
    // looking for Agda
    // AgdaMode.Process.PathSearch.run("agda")
    // ->Promise.get(
    //     fun
    //     | Error(e) => {
    //         let (_, msg) = AgdaMode.Process.PathSearch.Error.toString(e);
    //         Window.showInformationMessage(msg);
    //       }
    //     | Ok(_path) => {},
    //   );
    | Some(state) =>
      Js.log("already loaded");
      View.activate(state);
    }
  );
};

let activate = (context: ExtensionContext.t) => {
  // when a TextEditor gets activated, reveal the corresponding Panel (if any)
  Window.onDidChangeActiveTextEditor(editor => {
    editor->Option.flatMap(States.get)->Option.forEach(View.activate)
  })
  ->Js.Array.push(context.subscriptions)
  ->ignore;

  // when a TextEditor gets closed, delete the corresponding Panel (if any)
  Workspace.onDidCloseTextDocument(textDoc => {
    textDoc.fileName->States.getByFileName->Option.forEach(State.dispose)
  })
  ->Js.Array.push(context.subscriptions)
  ->ignore;

  Commands.registerCommand("extension.load", load)
  ->Js.Array.push(context.subscriptions)
  ->ignore;
};

let deactive = () => {
  States.dispose();
};