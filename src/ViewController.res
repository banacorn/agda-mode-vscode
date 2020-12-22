module type Module = {
  type t

  let make: (string, VSCode.TextEditor.t) => t
  let destroy: t => unit

  let sendEvent: (t, View.EventToView.t) => Promise.t<unit>
  let sendRequest: (t, View.Request.t, View.Response.t => Promise.t<unit>) => Promise.t<unit>
  let onEvent: (t, View.EventFromView.t => unit) => VSCode.Disposable.t

  let show: t => unit
  let hide: t => unit
  let focus: t => unit
}

module Module: Module = {
  type status =
    | Initialized
    | Uninitialized(
        array<(View.Request.t, View.ResponseOrEventFromView.t => unit)>,
        array<View.EventToView.t>,
      )

  type t = {
    panel: VSCode.WebviewPanel.t,
    onResponseFromView: Chan.t<View.Response.t>,
    onEventFromView: Chan.t<View.EventFromView.t>,
    subscriptions: array<VSCode.Disposable.t>,
    mutable status: status,
  }

  // messaging
  let send = (view, requestOrEvent) =>
    switch view.status {
    | Uninitialized(queuedRequests, queuedEvents) =>
      open View.RequestOrEventToView
      switch requestOrEvent {
      | Request(req) =>
        let (promise, resolve) = Promise.pending()
        Js.Array.push(
          (
            req,
            x =>
              switch x {
              | View.ResponseOrEventFromView.Event(_) => resolve(None)
              | Response(res) => resolve(Some(res))
              },
          ),
          queuedRequests,
        )->ignore
        promise
      | Event(event) =>
        Js.Array.push(event, queuedEvents)->ignore
        Promise.resolved(None)
      }
    | Initialized =>
      open View.RequestOrEventToView
      let stringified = Js.Json.stringify(View.RequestOrEventToView.encode(requestOrEvent))

      switch requestOrEvent {
      | Request(_) =>
        let promise = view.onResponseFromView->Chan.once
        view.panel
        ->VSCode.WebviewPanel.webview
        ->VSCode.Webview.postMessage(stringified)
        ->Promise.flatMap(_ => promise)
        ->Promise.map(res => Some(res))
      | Event(_) =>
        view.panel
        ->VSCode.WebviewPanel.webview
        ->VSCode.Webview.postMessage(stringified)
        ->Promise.map(_ => None)
      }
    }

  let sendEvent = (view, event) =>
    send(view, View.RequestOrEventToView.Event(event))->Promise.map(_ => ())
  let sendRequest = (view, request, callback) =>
    send(view, View.RequestOrEventToView.Request(request))->Promise.flatMap(x =>
      switch x {
      | None => Promise.resolved()
      | Some(response) => callback(response)
      }
    )

  let onEvent = (view, callback) =>
    // Handle events from the webview
    view.onEventFromView->Chan.on(callback)->VSCode.Disposable.make

  let make = (extensionPath, editor) => {
    let html = (distPath, styleUri, scriptUri, codiconUri) => {
      let nonce = {
        let text = ref("")
        let charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        let cardinality = Js.String.length(charaterSet)
        for _ in 0 to 32 {
          text :=
            text.contents ++
            Js.String.charAt(
              Js.Math.floor(Js.Math.random() *. float_of_int(cardinality)),
              charaterSet,
            )
        }
        text.contents
      }

      let styleUri =
        VSCode.Uri.file(Node.Path.join2(distPath, styleUri))->VSCode.Uri.with_(
          VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
        )

      let scriptUri =
        VSCode.Uri.file(Node.Path.join2(distPath, scriptUri))->VSCode.Uri.with_(
          VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
        )

      let codiconUri =
        VSCode.Uri.file(Node.Path.join2(distPath, codiconUri))->VSCode.Uri.with_(
          VSCode.Uri.makeChange(~scheme="vscode-resource", ()),
        )

      let metaContent =
        "font-src vscode-resource: ;default-src 'none'; img-src vscode-resource: https:; script-src 'nonce-" ++
        (nonce ++
        "';style-src vscode-resource: 'unsafe-inline' http: https: data:;")

      j`
        <!DOCTYPE html>
              <html lang="en">
              <head>
                <meta charset="utf-8">
                <meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
                <meta name="theme-color" content="#000000">
                <title>React App</title>
                <link rel="stylesheet" type="text/css" href="$styleUri">
                <link rel="stylesheet" type="text/css" href="$codiconUri">
                <meta http-equiv="Content-Security-Policy" content="$metaContent">
              </head>
              <body>
                <noscript>You need to enable JavaScript to run this app.</noscript>
                <div id="root"></div>
                <script nonce="$nonce" src="$scriptUri"></script>
              </body>
              </html>
        `
    }

    let createPanel = editor => {
      let distPath = Node.Path.join2(extensionPath, "dist")
      let fileName = Node.Path.basename_ext(
        editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
        ".agda",
      )

      let panel = VSCode.Window.createWebviewPanel(
        "panel",
        "Agda [" ++ (fileName ++ "]"),
        {preserveFocus: true, viewColumn: 3},
        // None,
        Some(
          VSCode.WebviewAndWebviewPanelOptions.make(
            ~enableScripts=true,
            // So that the view don't get wiped out when it's not in the foreground
            ~retainContextWhenHidden=true,
            // And restrict the webview to only loading content from our extension's `media` directory.
            ~localResourceRoots=[VSCode.Uri.file(distPath)],
            (),
          ),
        ),
      )

      panel
      ->VSCode.WebviewPanel.webview
      ->VSCode.Webview.setHtml(html(distPath, "style.css", "view.bundle.js", "codicon/codicon.css"))

      panel
    }

    let moveToBottom = () => {
      open VSCode.Commands
      executeCommand(
        #setEditorLayout({
          orientation: 1,
          groups: {
            open Layout
            [sized({groups: [simple], size: 0.7}), sized({groups: [simple], size: 0.3})]
          },
        }),
      )
    }

    // intantiate the panel
    let panel = createPanel(editor)
    moveToBottom() |> ignore

    // array of Disposable.t
    let subscriptions = []

    // on message
    // relay Webview.onDidReceiveMessage => onResponseFromView or onEventFromView
    let onResponseFromView = Chan.make()
    let onEventFromView = Chan.make()
    panel
    ->VSCode.WebviewPanel.webview
    ->VSCode.Webview.onDidReceiveMessage(json =>
      switch View.ResponseOrEventFromView.decode(json) {
      | Response(res) => onResponseFromView->Chan.emit(res)
      | Event(ev) => onEventFromView->Chan.emit(ev)
      | exception e => Js.log2("[ panic ][ Webview.onDidReceiveMessage JSON decode error ]", e)
      }
    )
    ->Js.Array.push(subscriptions)
    ->ignore

    // on destroy
    panel
    ->VSCode.WebviewPanel.onDidDispose(() => onEventFromView->Chan.emit(Destroyed))
    ->Js.Array.push(subscriptions)
    ->ignore

    let view = {
      panel: panel,
      subscriptions: subscriptions,
      onResponseFromView: onResponseFromView,
      onEventFromView: onEventFromView,
      status: Uninitialized([], []),
    }

    // when initialized, send the queued Requests & Events
    view.onEventFromView
    ->Chan.on(x =>
      switch x {
      | Initialized =>
        switch view.status {
        | Uninitialized(queuedRequests, queuedEvents) =>
          view.status = Initialized
          queuedRequests->Belt.Array.forEach(((req, resolve)) =>
            send(view, View.RequestOrEventToView.Request(req))->Promise.get(x =>
              switch x {
              | None => ()
              | Some(res) => resolve(View.ResponseOrEventFromView.Response(res))
              }
            )
          )
          queuedEvents->Belt.Array.forEach(event =>
            send(view, View.RequestOrEventToView.Event(event))->ignore
          )
        | Initialized => ()
        }
      | _ => ()
      }
    )
    ->VSCode.Disposable.make
    ->Js.Array.push(view.subscriptions)
    ->ignore

    view
  }

  let destroy = view => {
    // if we invoke `view.panel->WebviewPanel.dispose` first,
    // this would trigger `View.ResponseOrEventFromView.Event(Destroyed)`
    // and in turns would trigger this function AGAIN

    // destroy the chan first, to prevent the aforementioned from happening
    view.onResponseFromView->Chan.destroy
    view.onEventFromView->Chan.destroy
    view.panel->VSCode.WebviewPanel.dispose
    view.subscriptions->Belt.Array.forEach(VSCode.Disposable.dispose)
  }

  // show/hide
  let show = view => view.panel->VSCode.WebviewPanel.reveal(~preserveFocus=true, ())
  let hide = _view => ()
  let focus = view => view.panel->VSCode.WebviewPanel.reveal()
}
include Module
