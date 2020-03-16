// open Belt;
open Vscode;

let html = (extensionPath, styleUri, scriptUri, nonce) => {
  let metaContent =
    "default-src 'none'; img-src vscode-resource: https:; script-src 'nonce-"
    ++ nonce
    ++ "';style-src vscode-resource: 'unsafe-inline' http: https: data:;";

  let styleUri =
    Uri.file(Node.Path.join2(extensionPath, styleUri))
    ->Uri.with_(Uri.makeChange(~scheme="vscode-resource", ()));
  let scriptUri =
    Uri.file(Node.Path.join2(extensionPath, scriptUri))
    ->Uri.with_(Uri.makeChange(~scheme="vscode-resource", ()));
  Js.log(styleUri);
  Js.log(scriptUri);
  {j|
<!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="utf-8">
				<meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
				<meta name="theme-color" content="#000000">
				<title>React App</title>
				<link rel="stylesheet" type="text/css" href="$styleUri">
				<meta http-equiv="Content-Security-Policy" content="$metaContent">
			</head>

			<body>
				<noscript>You need to enable JavaScript to run this app.</noscript>
				<div id="root">Yo</div>
				<script nonce="$nonce" src="$scriptUri"></script>
			</body>
			</html>
|j};
};

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
    Window.createWebviewPanel'(
      "panel",
      "Agda [" ++ fileName ++ "]",
      {preserveFocus: true, viewColumn: 3},
      // None,
      Some(
        Window.WebviewAndWebviewPanelOptions.make(~enableScripts=true, ()),
      ),
      // Some(
      //   Window.WebviewAndWebviewPanelOptions.make(
      //     ~enableScripts=true,
      //     ~localResourceRoots=[|Uri.file("Some path")|],
      //     (),
      //   ),
      // ),
    );
  panel.webview.html =
    html(
      state.context.extensionPath,
      "style/style.css",
      "dist/bundled-view.js",
      "1234",
    );

  // Webapi.Document

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