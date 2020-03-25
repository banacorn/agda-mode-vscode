// open Belt;
open Vscode;

type message =
  | Display(string, string);

let html = (distPath, styleUri, scriptUri) => {
  let nonce = {
    let text = ref("");
    let charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    let cardinality = Js.String.length(charaterSet);
    for (_ in 0 to 32) {
      text :=
        text^
        ++ Js.String.charAt(
             Js.Math.floor(Js.Math.random() *. float_of_int(cardinality)),
             charaterSet,
           );
    };
    text^;
  };

  let styleUri =
    Uri.file(Node.Path.join2(distPath, styleUri))
    ->Uri.with_(Uri.makeChange(~scheme="vscode-resource", ()));

  let scriptUri =
    Uri.file(Node.Path.join2(distPath, scriptUri))
    ->Uri.with_(Uri.makeChange(~scheme="vscode-resource", ()));

  let metaContent =
    "default-src 'none'; img-src vscode-resource: https:; script-src 'nonce-"
    ++ nonce
    ++ "';style-src vscode-resource: 'unsafe-inline' http: https: data:;";

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
				<div id="root"></div>
        <script nonce="$nonce" src="$scriptUri"></script>
			</body>
			</html>
|j};
};

let createPanel = (state: State.t) => {
  let fileName =
    Node.Path.basename_ext(state.editor.document.fileName, ".agda");

  let distPath =
    Node.Path.join2(state.context->ExtensionContext.extensionPath, "dist");

  let panel =
    Window.createWebviewPanel(
      "panel",
      "Agda [" ++ fileName ++ "]",
      {preserveFocus: true, viewColumn: 3},
      // None,
      Some(
        WebviewAndWebviewPanelOptions.make(
          ~enableScripts=true,
          // And restric the webview to only loading content from our extension's `media` directory.
          ~localResourceRoots=[|Uri.file(distPath)|],
          (),
        ),
      ),
    );

  // panel.webview->Webview.postMessage(C(3)) |> ignore;
  // // ->Js.Array.push(state.context.subscriptions)
  // // ->ignore;

  // panel.webview
  // ->Webview.onDidReceiveMessage(message => {Js.log(message)})
  // ->Js.Array.push(state.context.subscriptions)
  // ->ignore;

  panel.webview
  ->Webview.setHtml(html(distPath, "style.css", "bundled-view.js"));

  panel->WebviewPanel.onDidDispose(() => {state.panel = None}) |> ignore;

  state.panel = Some(panel);
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
let activate = state =>
  switch (state.State.panel) {
  | None =>
    // intantiate the panel
    createPanel(state);
    moveToBottom() |> ignore;
  | Some(panel) => panel->WebviewPanel.reveal(~preserveFocus=true, ())
  };

let postMessage = (panel: WebviewPanel.t, message: message): unit => {
  panel.webview->Webview.postMessage(message) |> ignore;
};