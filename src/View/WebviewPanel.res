// abstraction of a VS Code WebviewPanel
module WebviewPanel: {
  type t
  // constructor / destructor
  let make: (string, string) => t
  let destroy: t => unit
  // messaging
  let send: (t, string) => promise<bool>
  let recv: (t, JSON.t => unit) => VSCode.Disposable.t
  // events
  let onDestroyed: (t, unit => unit) => VSCode.Disposable.t
  // methods
  let reveal: t => unit

  // move the panel around
  let getEditorLayout: unit => promise<Obj.t>
  let moveToBottom: unit => unit
  let moveToRight: unit => unit
} = {
  type t = VSCode.WebviewPanel.t

  let makeHTML = (webview, extensionPath) => {
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - STARTING ===")
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - extensionPath:", extensionPath)
    
    let extensionUri = VSCode.Uri.file(extensionPath)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - extensionUri:", extensionUri)
    
    // Log current location info if available (only in webview context)
    try {
      let location = Webapi.Dom.Window.location(Webapi.Dom.window)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - window.location.href:", Webapi.Dom.Location.href(location))
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - window.location.origin:", Webapi.Dom.Location.origin(location))
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - window.location.hostname:", Webapi.Dom.Location.hostname(location))
    } catch {
    | _ => Js.Console.log("[AGDA-MODE] WebviewPanel.makeHTML - window not available (extension context)")
    }
    // generates gibberish
    let nonce = {
      let text = ref("")
      let charaterSet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      let cardinality = String.length(charaterSet)
      for _ in 0 to 32 {
        text :=
          text.contents ++
          String.charAt(
            charaterSet,
            Int.fromFloat(Math.floor(Math.random() *. float_of_int(cardinality))),
          )
      }
      text.contents
    }

    // Check if we're in VS Code Web and use direct HTTP URLs
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - ENVIRONMENT DETECTION ===")
    
    let startsWithStatic = String.startsWith(extensionPath, "/static")
    let includesGitHubDev = String.includes(extensionPath, "github.dev")
    let includesVscodeCdn = String.includes(extensionPath, "vscode-cdn")
    let includesDevExtensions = String.includes(extensionPath, "devextensions")
    
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - startsWithStatic:", startsWithStatic)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - includesGitHubDev:", includesGitHubDev)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - includesVscodeCdn:", includesVscodeCdn)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - includesDevExtensions:", includesDevExtensions)
    
    let isLocalWeb = startsWithStatic
    let isGitHubDev = includesGitHubDev || includesVscodeCdn
    let isWeb = isLocalWeb || isGitHubDev
    
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL isLocalWeb:", isLocalWeb)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL isGitHubDev:", isGitHubDev)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL isWeb:", isWeb)

    let (scriptUri, styleUri, codiconsUri) = if isLocalWeb {
      Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - LOCAL WEB PATH ===")
      // Use direct HTTP URLs for local VS Code Web
      let baseUrl = "http://localhost:3000/static/devextensions/dist/"
      let scriptUri = baseUrl ++ "bundled-view.js"
      let styleUri = baseUrl ++ "style.css"
      let codiconsUri = baseUrl ++ "codicon/codicon.css"
      
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL baseUrl:", baseUrl)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL scriptUri:", scriptUri)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL styleUri:", styleUri)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL codiconsUri:", codiconsUri)
      
      (scriptUri, styleUri, codiconsUri)
    } else if isGitHubDev {
      Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - GITHUB.DEV PATH ===")
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev extensionPath:", extensionPath)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev extensionUri:", extensionUri)
      
      // Try webview URIs first and log what we get
      let scriptPath = VSCode.Uri.joinPath(extensionUri, ["dist", "bundled-view.js"])
      let stylePath = VSCode.Uri.joinPath(extensionUri, ["dist", "style.css"])
      let codiconsPath = VSCode.Uri.joinPath(extensionUri, ["dist", "codicon/codicon.css"])
      
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev scriptPath BEFORE asWebviewUri:", scriptPath)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev stylePath BEFORE asWebviewUri:", stylePath)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev codiconsPath BEFORE asWebviewUri:", codiconsPath)
      
      let scriptUri = VSCode.Webview.asWebviewUri(webview, scriptPath)->VSCode.Uri.toString
      let styleUri = VSCode.Webview.asWebviewUri(webview, stylePath)->VSCode.Uri.toString
      let codiconsUri = VSCode.Webview.asWebviewUri(webview, codiconsPath)->VSCode.Uri.toString
      
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev scriptUri AFTER asWebviewUri:", scriptUri)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev styleUri AFTER asWebviewUri:", styleUri)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GitHub.dev codiconsUri AFTER asWebviewUri:", codiconsUri)
      
      (scriptUri, styleUri, codiconsUri)
    } else {
      Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - DESKTOP PATH ===")
      // Use webview URIs for desktop
      let scriptPath = VSCode.Uri.joinPath(extensionUri, ["dist", "bundled-view.js"])
      let stylePath = VSCode.Uri.joinPath(extensionUri, ["dist", "style.css"])
      let codiconsPath = VSCode.Uri.joinPath(extensionUri, ["dist", "codicon/codicon.css"])
      
      let scriptUri = VSCode.Webview.asWebviewUri(webview, scriptPath)->VSCode.Uri.toString
      let styleUri = VSCode.Webview.asWebviewUri(webview, stylePath)->VSCode.Uri.toString
      let codiconsUri = VSCode.Webview.asWebviewUri(webview, codiconsPath)->VSCode.Uri.toString
      
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - DESKTOP scriptUri:", scriptUri)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - DESKTOP styleUri:", styleUri)
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - DESKTOP codiconsUri:", codiconsUri)
      
      (scriptUri, styleUri, codiconsUri)
    }
    
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - FINAL URIS ===")
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL scriptUri:", scriptUri)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL styleUri:", styleUri)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL codiconsUri:", codiconsUri)

    let cspSourceUri = VSCode.Webview.cspSource(webview)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - cspSourceUri:", cspSourceUri)

    // Content-Security-Policy - allow web URLs for different environments
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - CSP GENERATION ===")
    let defaultSrc = "default-src 'none'; "
    let scriptSrc = if isLocalWeb {
      let src = "script-src 'nonce-" ++ nonce ++ "' http://localhost:3000; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL scriptSrc:", src)
      src
    } else if isGitHubDev {
      let src = "script-src 'nonce-" ++ nonce ++ "' https://*.github.dev https://*.vscode-cdn.net; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GITHUB.DEV scriptSrc:", src)
      src
    } else {
      let src = "script-src 'nonce-" ++ nonce ++ "'; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - DESKTOP scriptSrc:", src)
      src
    }
    let styleSrc = if isLocalWeb {
      let src = "style-src " ++ cspSourceUri ++ " http://localhost:3000; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL styleSrc:", src)
      src
    } else if isGitHubDev {
      let src = "style-src " ++ cspSourceUri ++ " https://*.github.dev https://*.vscode-cdn.net; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GITHUB.DEV styleSrc:", src)
      src
    } else {
      let src = "style-src " ++ cspSourceUri ++ "; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - DESKTOP styleSrc:", src)
      src
    }
    let fontSrc = if isLocalWeb {
      let src = "font-src " ++ cspSourceUri ++ " http://localhost:3000; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - LOCAL fontSrc:", src)
      src
    } else if isGitHubDev {
      let src = "font-src " ++ cspSourceUri ++ " https://*.github.dev https://*.vscode-cdn.net; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - GITHUB.DEV fontSrc:", src)
      src
    } else {
      let src = "font-src " ++ cspSourceUri ++ "; "
      Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - DESKTOP fontSrc:", src)
      src
    }
    let scp = defaultSrc ++ fontSrc ++ scriptSrc ++ styleSrc
    
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - FINAL CSP:", scp)
    
    let html = `
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
        <meta name="theme-color" content="#000000">

        <!--
					Use a content security policy to only allow loading images from https or from our extension directory,
					and only allow scripts that have a specific nonce.
				-->
        <meta http-equiv="Content-Security-Policy" content="${scp}">

        <title>React App</title>
        <link href="${styleUri}"    rel="stylesheet" type="text/css" >
        <link href="${codiconsUri}" rel="stylesheet" />
      </head>
      <body>
        <noscript>You need to enable JavaScript to run this app.</noscript>
        <div id="root"></div>
        <script nonce="${nonce}" src="${scriptUri}"></script>
        
        <!-- Debug info embedded in HTML -->
        <script>
          console.log("[AGDA-MODE] HTML - Environment Detection:");
          console.log("[AGDA-MODE] HTML - extensionPath: ${extensionPath}");
          console.log("[AGDA-MODE] HTML - isLocalWeb: ${string_of_bool(isLocalWeb)}");
          console.log("[AGDA-MODE] HTML - isGitHubDev: ${string_of_bool(isGitHubDev)}");
          console.log("[AGDA-MODE] HTML - scriptUri: ${scriptUri}");
          console.log("[AGDA-MODE] HTML - styleUri: ${styleUri}");
          console.log("[AGDA-MODE] HTML - codiconsUri: ${codiconsUri}");
          console.log("[AGDA-MODE] HTML - CSP: ${scp}");
        </script>
      </body>
      </html>
    `
    
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.makeHTML - HTML GENERATION COMPLETE ===")
    Js.Console.log2("[AGDA-MODE] WebviewPanel.makeHTML - Generated HTML preview (first 500 chars):", String.substring(html, ~start=0, ~end=500))
    
    html
  }

  let make = (title, extensionPath) => {
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.make - STARTING ===")
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - title:", title)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - extensionPath:", extensionPath)
    
    let distPath = NodeJs.Path.join2(extensionPath, "dist")
    let distUri = VSCode.Uri.file(distPath)
    
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - distPath:", distPath)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - distUri:", distUri)
    
    let webviewOptions = VSCode.WebviewAndWebviewPanelOptions.make(
      ~enableScripts=true,
      // So that the view don't get wiped out when it's not in the foreground
      ~retainContextWhenHidden=true,
      // And restrict the webview to only loading content from our extension's `media` directory.
      ~localResourceRoots=[distUri],
      (),
    )
    
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - webviewOptions:", webviewOptions)
    
    let panel = VSCode.Window.createWebviewPanel(
      "panel",
      title,
      {"preserveFocus": true, "viewColumn": 3},
      Some(webviewOptions),
    )
    
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - panel created:", panel)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - panel.webview:", VSCode.WebviewPanel.webview(panel))

    Js.Console.log("[AGDA-MODE] WebviewPanel.make - About to call makeHTML...")
    let html = makeHTML(VSCode.WebviewPanel.webview(panel), extensionPath)
    Js.Console.log2("[AGDA-MODE] WebviewPanel.make - Generated HTML length:", String.length(html))
    
    Js.Console.log("[AGDA-MODE] WebviewPanel.make - Setting HTML on webview...")
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.setHtml(html)
    
    Js.Console.log("=== [AGDA-MODE] WebviewPanel.make - COMPLETED ===")
    panel
  }

  let destroy = VSCode.WebviewPanel.dispose

  let send = (panel, message) =>
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.postMessage(message)

  let recv = (panel, callback) =>
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.onDidReceiveMessage(callback)

  let onDestroyed = (panel, callback) => panel->VSCode.WebviewPanel.onDidDispose(callback)

  let reveal = panel => panel->VSCode.WebviewPanel.reveal(~preserveFocus=true)

  let moveToBottom = () => {
    open VSCode.Commands
    executeCommand(
      #setEditorLayout(
        %raw(`{
          orientation: 1,
          groups: [{ size: 0.7 }, { size: 0.3 }]
        }`),
        // {
        // orientation: 1,
        // groups: {
        //   open Layout
        //   [sized({groups: [simple], size: 0.7}), sized({groups: [simple], size: 0.3})]
        // },
      ),
    )->ignore
  }

  let moveToRight = () => {
    open VSCode.Commands
    executeCommand(
      #setEditorLayout(
        %raw(`{
          orientation: 0,
          groups: [ {size: 0.5}, {size: 0.5} ]
        }`),
      ),
    )->ignore
  }

  let getEditorLayout = async () => await VSCode.Commands.executeCommand0("vscode.getEditorLayout")
}

// a thin layer on top of WebviewPanel
module type Module = {
  type t

  let make: (string, string) => t
  let destroy: t => unit

  let sendEvent: (t, View.EventToView.t) => promise<unit>
  let sendRequest: (t, View.Request.t, View.Response.t => promise<unit>) => promise<unit>
  let onEvent: (t, View.EventFromView.t => unit) => VSCode.Disposable.t

  let onceDestroyed: t => promise<unit>

  let reveal: t => unit
}

module Module: Module = {
  type status =
    | Initialized
    | Uninitialized(
        array<(View.Request.t, View.ResponseOrEventFromView.t => unit)>,
        array<View.EventToView.t>,
      )

  type t = {
    panel: WebviewPanel.t,
    onResponse: Chan.t<View.Response.t>,
    onEvent: Chan.t<View.EventFromView.t>,
    subscriptions: array<VSCode.Disposable.t>,
    mutable status: status,
  }

  // messaging
  let send = async (view, requestOrEvent) =>
    switch view.status {
    | Uninitialized(queuedRequests, queuedEvents) =>
      open View.RequestOrEventToView
      switch requestOrEvent {
      | Request(req) =>
        await Promise.make((resolve, _) => {
          queuedRequests->Array.push((
            req,
            x =>
              switch x {
              | View.ResponseOrEventFromView.Event(_) => resolve(None)
              | Response(res) => resolve(Some(res))
              },
          ))
        })
      | Event(event) =>
        queuedEvents->Array.push(event)
        None
      }
    | Initialized =>
      open View.RequestOrEventToView
      let stringified = JSON.stringify(View.RequestOrEventToView.encode(requestOrEvent))

      switch requestOrEvent {
      | Request(_) =>
        let promise = view.onResponse->Chan.once
        let _ = await view.panel->WebviewPanel.send(stringified)
        let res = await promise
        Some(res)
      | Event(_) =>
        let _ = await view.panel->WebviewPanel.send(stringified)
        None
      }
    }

  let sendEvent = async (view, event) => {
    let _ = await send(view, View.RequestOrEventToView.Event(event))
  }

  let sendRequest = async (view, request, callback) =>
    switch await send(view, View.RequestOrEventToView.Request(request)) {
    | None => ()
    | Some(response) => await callback(response)
    }
  let onEvent = (view, callback) =>
    // Handle events from the webview
    view.onEvent->Chan.on(callback)->VSCode.Disposable.make

  let make = (title, extensionPath) => {
    let view = {
      panel: WebviewPanel.make(title, extensionPath),
      subscriptions: [],
      onResponse: Chan.make(),
      onEvent: Chan.make(),
      status: Uninitialized([], []),
    }

    // Move the created panel to the bottom row
    switch Config.View.getPanelMountingPosition() {
    | Bottom => WebviewPanel.moveToBottom()
    | Right => WebviewPanel.moveToRight()
    }

    // on message
    // relay Webview.onDidReceiveMessage => onResponse or onEvent
    view.panel
    ->WebviewPanel.recv(json =>
      switch JsonCombinators.Json.decode(json, View.ResponseOrEventFromView.decode) {
      | Ok(Response(res)) => view.onResponse->Chan.emit(res)
      | Ok(Event(ev)) => view.onEvent->Chan.emit(ev)
      | Error(e) => Js.log2("[ panic ][ Webview.onDidReceiveMessage JSON decode error ]", e)
      }
    )
    ->Js.Array.push(view.subscriptions)
    ->ignore

    // on destroy
    view.panel
    ->WebviewPanel.onDestroyed(() => view.onEvent->Chan.emit(Destroyed))
    ->Js.Array.push(view.subscriptions)
    ->ignore

    // when initialized, send the queued Requests & Events
    view.onEvent
    ->Chan.on(x =>
      switch x {
      | Initialized =>
        switch view.status {
        | Uninitialized(queuedRequests, queuedEvents) =>
          view.status = Initialized
          queuedRequests->Array.forEach(((req, resolve)) =>
            send(view, View.RequestOrEventToView.Request(req))
            ->Promise.thenResolve(
              x =>
                switch x {
                | None => ()
                | Some(res) => resolve(View.ResponseOrEventFromView.Response(res))
                },
            )
            ->Promise.done
          )
          queuedEvents->Array.forEach(event =>
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
    view.onResponse->Chan.destroy
    view.onEvent->Chan.destroy
    view.panel->WebviewPanel.destroy
    view.subscriptions->Array.forEach(VSCode.Disposable.dispose)
  }

  // resolves the returned promise once the view has been destroyed
  let onceDestroyed = (view: t): promise<unit> => {
    let (promise, resolve, _) = Util.Promise_.pending()

    let disposable = view.onEvent->Chan.on(response =>
      switch response {
      | Destroyed => resolve()
      | _ => ()
      }
    )

    promise->Promise.thenResolve(disposable)
  }

  // show/focus
  let reveal = view => view.panel->WebviewPanel.reveal
}

include Module
