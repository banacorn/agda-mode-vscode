open Belt

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = VSCode.Api.acquireVsCodeApi()

// relay VSCode.Api.onMessage => onRequest or onEvent;
let onRequest = Chan.make()
let onEventToView = Chan.make()
VSCode.Api.onMessage(stringifiedJSON => {
  let json = stringifiedJSON->Js.Json.parseExn
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
Webapi.Dom.Document.getElementById(Webapi.Dom.document, "root")->Option.forEach(element =>
  ReactDOM.render(<Panel onRequest onEventToView onResponse onEventFromView />, element)
)
