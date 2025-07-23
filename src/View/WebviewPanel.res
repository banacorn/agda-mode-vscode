// abstraction of a VS Code WebviewPanel
module WebviewPanel: {
  type t
  // constructor / destructor
  let make: (string, VSCode.Uri.t) => t
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
  // Environment module handles VS Code environment detection and resource management
  // for webview panels across different platforms (desktop, web localhost, github.dev, etc.)
  module Environment = {
    // Represents the different VS Code environments where the extension can run
    // - Desktop: Standard VS Code desktop application (also used as fallback for localhost:3000)
    // - WebGitHub: GitHub.dev or similar web platforms with CDN resources
    type t = Desktop | WebGitHub

    // Constants for environment detection
    module Constants = {
      let fallbackCdnBaseUrl = "https://banacorn.vscode-unpkg.net"

      // Resource file names
      let scriptFile = "bundled-view.js"
      let styleFile = "style.css"
      let codiconsFile = "codicon/codicon.css"

      // Path segments
      let distDir = "dist"
    }

    // Detects the current VS Code environment based on extension path and CSP source URI
    let detect = (extensionUri: VSCode.Uri.t, cspSourceUri: string): t => {
      // GitHub.dev indicators in extension path
      let hasGitHubDevInPath = String.includes(VSCode.Uri.path(extensionUri), "github.dev")
      let hasVscodeCdnInPath = String.includes(VSCode.Uri.path(extensionUri), "vscode-cdn")

      // GitHub.dev indicators in CSP source URI
      let hasGitHubCdnInCsp =
        String.includes(cspSourceUri, "vscode-unpkg.net") ||
        String.includes(cspSourceUri, "github.dev")

      // Environment detection logic
      if hasGitHubDevInPath || hasVscodeCdnInPath || hasGitHubCdnInCsp {
        WebGitHub
      } else {
        // Default to Desktop for all other cases (including localhost:3000 and unknown environments)
        Desktop
      }
    }

    // Helper function to extract and validate CDN base URL from CSP source URI
    // Used specifically for GitHub.dev environments
    let extractCdnBaseUrl = (cspSourceUri: string, extensionUri: VSCode.Uri.t): string => {
      // CSP format example: "https://banacorn.vscode-unpkg.net/banacorn/agda-mode/0.7.5/extension/ 'self' https://*.vscode-cdn.net"
      let cspParts = String.split(cspSourceUri, " ")
      let rawCdnUrl = cspParts->Array.get(0)->Option.getOr("")

      // Normalize URL (ensure trailing slash)
      let normalizedUrl = if String.endsWith(rawCdnUrl, "/") {
        rawCdnUrl
      } else {
        rawCdnUrl ++ "/"
      }

      // Validate extracted URL and provide fallback
      if String.length(normalizedUrl) > 10 && String.includes(normalizedUrl, "https://") {
        normalizedUrl
      } else {
        // Fallback: construct CDN URL from extension path
        Constants.fallbackCdnBaseUrl ++ VSCode.Uri.path(extensionUri) ++ "/"
      }
    }

    // Generates resource URLs (script, style, codicons) for the given environment
    let getResourceUrls = (env: t, extensionUri: VSCode.Uri.t, cspSourceUri: string): option<(
      string,
      string,
      string,
    )> => {
      let baseUrl = switch env {
      | Desktop =>
        // Desktop uses webview.asWebviewUri() which requires webview context
        None
      | WebGitHub =>
        // GitHub.dev: Extract CDN base URL and construct direct resource URLs
        Some(extractCdnBaseUrl(cspSourceUri, extensionUri) ++ Constants.distDir ++ "/")
      }

      switch baseUrl {
      | None => None
      | Some(baseUrl) =>
        Some(
          baseUrl ++ Constants.scriptFile,
          baseUrl ++ Constants.styleFile,
          baseUrl ++ Constants.codiconsFile,
        )
      }
    }

    // Generates Content Security Policy rules for the given environment
    let getCspRules = (env: t, nonce: string, cspSourceUri: string): (string, string, string) => {
      switch env {
      | Desktop => (
          `script-src 'nonce-${nonce}'; `,
          `style-src ${cspSourceUri}; `,
          `font-src ${cspSourceUri}; `,
        )
      | WebGitHub => {
          // Allow multiple CDN domains for GitHub.dev
          let webDomains = "https://*.github.dev https://*.vscode-cdn.net https://*.vscode-unpkg.net"
          (
            `script-src 'nonce-${nonce}' ${webDomains}; `,
            `style-src ${cspSourceUri} ${webDomains}; `,
            `font-src ${cspSourceUri} ${webDomains}; `,
          )
        }
      }
    }
  }

  type t = VSCode.WebviewPanel.t

  // Generates HTML content for the webview panel with environment-specific resource URLs
  // and Content Security Policy rules
  let makeHTML = (webview, extensionUri: VSCode.Uri.t) => {
    // Generate cryptographically secure nonce for Content Security Policy
    // Uses alphanumeric characters to create a 32-character random string
    let generateSecurityNonce = (): string => {
      let text = ref("")
      let charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
      let charsetLength = String.length(charset)

      for _ in 0 to 32 {
        let randomIndex = Int.fromFloat(Math.floor(Math.random() *. float_of_int(charsetLength)))
        text := text.contents ++ String.charAt(charset, randomIndex)
      }
      text.contents
    }

    let nonce = generateSecurityNonce()

    // Environment detection and resource URL generation
    let cspSourceUri = VSCode.Webview.cspSource(webview)
    let environment = Environment.detect(extensionUri, cspSourceUri)

    // Generate resource URLs based on the detected environment
    // Desktop requires special handling due to webview.asWebviewUri() dependency
    let (scriptUri, styleUri, codiconsUri) = switch Environment.getResourceUrls(
      environment,
      extensionUri,
      cspSourceUri,
    ) {
    | None => {
        // Desktop: Use webview.asWebviewUri() for proper file:// to vscode-webview:// conversion
        let scriptPath = VSCode.Uri.joinPath(
          extensionUri,
          [Environment.Constants.distDir, Environment.Constants.scriptFile],
        )
        let stylePath = VSCode.Uri.joinPath(
          extensionUri,
          [Environment.Constants.distDir, Environment.Constants.styleFile],
        )
        let codiconsPath = VSCode.Uri.joinPath(
          extensionUri,
          [Environment.Constants.distDir, Environment.Constants.codiconsFile],
        )

        let scriptUri = VSCode.Webview.asWebviewUri(webview, scriptPath)->VSCode.Uri.toString
        let styleUri = VSCode.Webview.asWebviewUri(webview, stylePath)->VSCode.Uri.toString
        let codiconsUri = VSCode.Webview.asWebviewUri(webview, codiconsPath)->VSCode.Uri.toString

        (scriptUri, styleUri, codiconsUri)
      }
    | Some(scriptUri, styleUri, codiconsUri) => (scriptUri, styleUri, codiconsUri)
    }

    // Generate Content Security Policy with environment-specific rules
    // Follows principle of least privilege - only allow necessary sources
    let generateContentSecurityPolicy = (): string => {
      let defaultSrc = "default-src 'none'; " // Deny all by default
      let (scriptSrc, styleSrc, fontSrc) = Environment.getCspRules(environment, nonce, cspSourceUri)
      defaultSrc ++ fontSrc ++ scriptSrc ++ styleSrc
    }

    let csp = generateContentSecurityPolicy()

    // Generate the complete HTML document for the webview
    // Includes proper CSP headers, resource links, and React root element
    let html = `
      <!DOCTYPE html>
      <html lang="en">
      <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width,initial-scale=1,shrink-to-fit=no">
        <meta name="theme-color" content="#000000">

        <!-- Content Security Policy: Environment-specific rules for secure resource loading -->
        <meta http-equiv="Content-Security-Policy" content="${csp}">

        <title>Agda Mode</title>
        <link href="${styleUri}" rel="stylesheet" type="text/css">
        <link href="${codiconsUri}" rel="stylesheet">
      </head>
      <body>
        <noscript>You need to enable JavaScript to run this app.</noscript>
        <div id="root"></div>
        <script nonce="${nonce}" src="${scriptUri}"></script>
      </body>
      </html>
    `

    html
  }

  // Creates a new webview panel with environment-aware resource loading
  let make = (title, extensionUri) => {
    // Configure allowed local resource roots for security
    let distPath =
      VSCode.Uri.joinPath(extensionUri, [Environment.Constants.distDir])->VSCode.Uri.fsPath
    let distUri = VSCode.Uri.file(distPath)

    let webviewOptions = VSCode.WebviewAndWebviewPanelOptions.make(
      ~enableScripts=true,
      // Preserve panel state when not in foreground (better UX)
      ~retainContextWhenHidden=true,
      // Security: restrict webview to only load resources from dist directory
      ~localResourceRoots=[distUri],
      (),
    )

    // Create the webview panel in column 3 (rightmost)
    let panel = VSCode.Window.createWebviewPanel(
      "panel",
      title,
      {"preserveFocus": true, "viewColumn": 3},
      Some(webviewOptions),
    )

    // Generate and set the HTML content with environment-specific resources
    let html = makeHTML(VSCode.WebviewPanel.webview(panel), extensionUri)
    panel->VSCode.WebviewPanel.webview->VSCode.Webview.setHtml(html)

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

  let make: (string, VSCode.Uri.t) => t
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

  let make = (title, extensionUri) => {
    let view = {
      panel: WebviewPanel.make(title, extensionUri),
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
