open Belt;

// let load = (context, ()) => {
//   Window.activeTextEditor->Option.forEach(editor =>
//     switch (States.get(editor)) {
//     | None =>
//       let state = State.make(context, editor);
//       States.set(editor.document.fileName, state);
//       View.activate(state);
//     // looking for Agda
//     // AgdaMode.Process.PathSearch.run("agda")
//     // ->Promise.get(
//     //     fun
//     //     | Error(e) => {
//     //         let (_, msg) = AgdaMode.Process.PathSearch.Error.toString(e);
//     //         Window.showInformationMessage(msg);
//     //       }
//     //     | Ok(_path) => {},
//     //   );
//     | Some(state) =>
//       // already loaded, open it
let load = state => {
  Js.log("load!");
  View.activate(state);
  // looking for Agda
  AgdaMode.Process.PathSearch.run("agda")
  ->Promise.get(
      fun
      | Error(_e) => {
          //   let (header, msg) = AgdaMode.Process.PathSearch.Error.toString(e);
          state.panel
          ->Option.forEach(View.postMessage(_, View.CannotFindAgda));
        }
      | Ok(_path) => {
          state.panel->Option.forEach(View.postMessage(_, View.Success));
        },
    );
};