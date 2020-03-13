// open Belt;
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

let createPanel = (state: State.t) => {
  let fileName =
    Node.Path.basename_ext(state.editor.document.fileName, ".agda");
  let panel =
    Window.createWebviewPanel(
      "panel",
      "Agda [" ++ fileName ++ "]",
      {preserveFocus: true, viewColumn: 3},
    );
  panel.webview.html = "<h1>hi</h1>";

  panel->WebviewPanel.onDidDispose(() => {state.panel = None})->ignore;

  state.panel = Some(panel);
};
let activate = state =>
  switch (state.State.panel) {
  | None =>
    // intantiate the panel
    createPanel(state);
    moveToBottom() |> ignore;
  | Some(panel) => panel->WebviewPanel.reveal(~preserveFocus=true, ())
  };