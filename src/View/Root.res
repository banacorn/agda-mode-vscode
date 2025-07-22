// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
Js.Console.log("[AGDA-MODE] Root.res - Starting webview initialization")
let vscode = VSCode.Api.acquireVsCodeApi()
Js.Console.log2("[AGDA-MODE] Root.res - Acquired VSCode API:", vscode)

// relay VSCode.Api.onMessage => onRequest or onEvent;
let onRequest = Chan.make()
let onEventToView = Chan.make()
VSCode.Api.onMessage(stringifiedJSON => {
  let json = stringifiedJSON->JSON.parseExn
  switch JsonCombinators.Json.decode(json, View.RequestOrEventToView.decode) {
  | Ok(Event(event)) => onEventToView->Chan.emit(event)
  | Ok(Request(req)) => onRequest->Chan.emit(req)
  | Error(_) => () // TODO: handle this decode error
  }
})

// relay onResponse => VSCode.Api.postMessage
let onResponse = Chan.make()
let _ = onResponse->Chan.on(response => {
  open View.ResponseOrEventFromView
  vscode->VSCode.Api.postMessage(encode(Response(response)))
})

// relay onEventFromView => VSCode.Api.postMessage
let onEventFromView = Chan.make()
let _ = onEventFromView->Chan.on(event => {
  open View.ResponseOrEventFromView
  vscode->VSCode.Api.postMessage(encode(Event(event)))
})

// mount the view at the "root" element
Js.Console.log("[AGDA-MODE] Root.res - Looking for root element...")
let rootElement = Webapi.Dom.Document.getElementById(Webapi.Dom.document, "root")
Js.Console.log2("[AGDA-MODE] Root.res - Found root element:", rootElement)

switch rootElement {
| Some(element) => {
    Js.Console.log("[AGDA-MODE] Root.res - Mounting React component...")
    let root = ReactDOM.Client.createRoot(element)
    Js.Console.log2("[AGDA-MODE] Root.res - Created React root:", root)
    root->ReactDOM.Client.Root.render(<Panel onRequest onEventToView onResponse onEventFromView />)
    Js.Console.log("[AGDA-MODE] Root.res - React component mounted successfully")
  }
| None => Js.Console.log("[AGDA-MODE] Root.res - ERROR: Root element not found!")
}
