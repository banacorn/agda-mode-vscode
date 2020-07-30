open VSCode;

type status =
  | Initialized
  | Uninitialized(
      array((View.Request.t, View.ResponseOrEventFromView.t => unit)),
      array(View.EventToView.t),
    );

type t = {
  panel: WebviewPanel.t,
  onResponseFromView: Event.t(View.Response.t),
  onEventFromView: Event.t(View.EventFromView.t),
  subscriptions: array(VSCode.Disposable.t),
  mutable status,
};

// messaging
let send = (view, requestOrEvent) =>
  switch (view.status) {
  | Uninitialized(queuedRequests, queuedEvents) =>
    View.RequestOrEventToView.(
      switch (requestOrEvent) {
      | Request(req) =>
        let (promise, resolve) = Promise.pending();
        Js.Array.push(
          (
            req,
            fun
            | View.ResponseOrEventFromView.Event(_) => resolve(None)
            | Response(res) => resolve(Some(res)),
          ),
          queuedRequests,
        )
        ->ignore;
        promise;
      | Event(event) =>
        Js.Array.push(event, queuedEvents)->ignore;
        Promise.resolved(None);
      }
    )
  | Initialized =>
    open View.RequestOrEventToView;
    let stringified =
      Js.Json.stringify(View.RequestOrEventToView.encode(requestOrEvent));

    switch (requestOrEvent) {
    | Request(_) =>
      let promise = view.onResponseFromView.once();
      view.panel
      ->WebviewPanel.webview
      ->Webview.postMessage(stringified)
      ->Promise.flatMap(_ => promise)
      ->Promise.map(res => Some(res));
    | Event(_) =>
      view.panel
      ->WebviewPanel.webview
      ->Webview.postMessage(stringified)
      ->Promise.map(_ => None)
    };
  };

let onEvent = (view, callback) => {
  // Handle events from the webview
  view.onEventFromView.on(callback)
  ->Disposable.make;
};

let make = (extensionPath, editor) => {
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

  let createPanel = editor => {
    let distPath = Node.Path.join2(extensionPath, "dist");
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
    ->Webview.setHtml(html(distPath, "style.css", "view.bundle.js"));

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
  let panel = createPanel(editor);
  moveToBottom() |> ignore;

  // array of Disposable.t
  let subscriptions = [||];

  // on message
  // relay Webview.onDidReceiveMessage => onResponseFromView or onEventFromView
  let onResponseFromView = Event.make();
  let onEventFromView = Event.make();
  panel
  ->WebviewPanel.webview
  ->Webview.onDidReceiveMessage(json => {
      switch (View.ResponseOrEventFromView.decode(json)) {
      | Response(res) => onResponseFromView.emit(res)
      | Event(ev) => onEventFromView.emit(ev)
      | exception e =>
        Js.log2(
          "[ panic ][ Webview.onDidReceiveMessage JSON decode error ]",
          e,
        )
      }
    })
  ->Js.Array.push(subscriptions)
  ->ignore;

  // on destroy
  panel
  ->WebviewPanel.onDidDispose(() => onEventFromView.emit(Destroyed))
  ->Js.Array.push(subscriptions)
  ->ignore;

  let view = {
    panel,
    subscriptions,
    onResponseFromView,
    onEventFromView,
    status: Uninitialized([||], [||]),
  };

  // when initialized, send the queued Requests & Events
  view.onEventFromView.on(
    fun
    | Initialized => {
        switch (view.status) {
        | Uninitialized(queuedRequests, queuedEvents) =>
          view.status = Initialized;
          queuedRequests->Belt.Array.forEach(((req, resolve)) =>
            send(view, View.RequestOrEventToView.Request(req))
            ->Promise.get(
                fun
                | None => ()
                | Some(res) =>
                  resolve(View.ResponseOrEventFromView.Response(res)),
              )
          );
          queuedEvents->Belt.Array.forEach(event =>
            send(view, View.RequestOrEventToView.Event(event))->ignore
          );
        | Initialized => ()
        };
      }
    | _ => (),
  )
  ->Disposable.make
  ->Js.Array.push(view.subscriptions)
  ->ignore;

  view;
};

let destroy = view => {
  // if we invoke `view.panel->WebviewPanel.dispose` first,
  // this would trigger `View.ResponseOrEventFromView.Event(Destroyed)`
  // and in turns would trigger this function AGAIN

  // destroy the EventEmitter first, to prevent the aforementioned from happening
  view.onResponseFromView.destroy();
  view.onEventFromView.destroy();
  view.panel->WebviewPanel.dispose;
  view.subscriptions->Belt.Array.forEach(Disposable.dispose);
};

// show/hide
let show = view => view.panel->WebviewPanel.reveal(~preserveFocus=true, ());
let focus = view => view.panel->WebviewPanel.reveal();
let hide = _view => ();
