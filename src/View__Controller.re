open Guacamole.Vscode;

type status =
  | Initialized
  | Uninitialized(array(View.Request.t));

type t = {
  panel: WebviewPanel.t,
  onResponse: Event.t(View.Response.t),
  mutable status,
};

// messaging
let send = (view, req) =>
  switch (view.status) {
  | Uninitialized(queued) =>
    Js.Array.push(req, queued)->ignore;
    Promise.resolved(false);
  | Initialized =>
    let stringified = Js.Json.stringify(View.Request.encode(req));
    view.panel->WebviewPanel.webview->Webview.postMessage(stringified);
  };

let recv = (view, callback) => {
  // Handle messages from the webview
  view.onResponse.on(callback)
  ->Disposable.make;
};

let make = (getExtensionPath, context, editor) => {
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

  let createPanel = (context, editor) => {
    let distPath = Node.Path.join2(context->getExtensionPath, "dist");
    let fileName =
      Node.Path.basename_ext(
        editor->TextEditor.document->TextDocument.fileName,
        ".agda",
      );

    let panel =
      Window.createWebviewPanel(
        "panel",
        "Agda [" ++ fileName ++ "]",
        {preserveFocus: true, viewColumn: 3},
        // None,
        Some(
          WebviewAndWebviewPanelOptions.make(
            ~enableScripts=true,
            // So that the view don't get wiped out when it's not in the foreground
            ~retainContextWhenHidden=true,
            // And restrict the webview to only loading content from our extension's `media` directory.
            ~localResourceRoots=[|Uri.file(distPath)|],
            (),
          ),
        ),
      );

    panel
    ->WebviewPanel.webview
    ->Webview.setHtml(html(distPath, "style.css", "bundled-view.js"));

    panel;
  };

  let moveToBottom = () => {
    Commands.(
      executeCommand(
        `setEditorLayout({
          orientation: 1,
          groups:
            Layout.(
              [|
                sized({groups: [|simple|], size: 0.5}),
                sized({groups: [|simple|], size: 0.5}),
              |]
            ),
        }),
      )
    );
  };

  // intantiate the panel
  let panel = createPanel(context, editor);
  moveToBottom() |> ignore;

  // on message
  // relay Webview.onDidReceiveMessage => onResponse
  let onResponse = Event.make();
  panel
  ->WebviewPanel.webview
  ->Webview.onDidReceiveMessage(json => {
      switch (View.Response.decode(json)) {
      | result => onResponse.emit(result)
      | exception e =>
        Js.log2(
          "[ panic ][ Webview.onDidReceiveMessage JSON decode error ]",
          e,
        )
      }
    })
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  // on destroy
  panel
  ->WebviewPanel.onDidDispose(() => onResponse.emit(View.Response.Destroyed))
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  let view = {panel, onResponse, status: Uninitialized([||])};

  // on initizlied
  view.onResponse.on(
    fun
    | Initialized => {
        switch (view.status) {
        | Uninitialized(queued) =>
          view.status = Initialized;
          queued->Belt.Array.forEach(req => send(view, req)->ignore);
        | Initialized => ()
        };
      }
    | _ => (),
  )
  ->Disposable.make
  ->Js.Array.push(context->ExtensionContext.subscriptions)
  ->ignore;

  view;
};

let destroy = view => {
  view.panel->WebviewPanel.dispose;
  view.onResponse.destroy();
};

// show/hide
let show = view => view.panel->WebviewPanel.reveal(~preserveFocus=true, ());
let hide = _view => ();