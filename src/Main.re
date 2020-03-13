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

let moveToBottom = () => {
  Commands.(
    executeCommand(
      `setEditorLayout({
        orientation: 1,
        groups:
          Layout.(
            [|
              sized({groups: [|simple|], size: 0.8}),
              sized({groups: [|simple|], size: 0.2}),
            |]
          ),
      }),
    )
  );
};

let createPanel = () => {
  let panel =
    Window.createWebviewPanel(
      "panel",
      "Agda Mode",
      {preserveFocus: true, viewColumn: 3},
    );
  panel.webview.html = "<h1>hi</h1>";

  panel;
};

let load = () => {
  let panel = createPanel();
  panel->WebviewPanel.onDidDispose(() => Js.log("DISPOSE!")) |> ignore;
  moveToBottom() |> ignore;
};

let activate = (context: ExtensionContext.t) => {
  let disposable = Commands.registerCommand("extension.load", load);
  Js.Array.push(disposable, context.subscriptions) |> ignore;
};

let deactive = () => ();