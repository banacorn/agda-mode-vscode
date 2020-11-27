open Belt

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = VSCode.Api.acquireVsCodeApi()

// relay VSCode.Api.onMessage => onRequest or onEvent;
let onRequest = Chan.make()
let onEventToView = Chan.make()
VSCode.Api.onMessage(stringifiedJSON => {
  let requestOrEvent = stringifiedJSON->Js.Json.parseExn->View.RequestOrEventToView.decode

  switch requestOrEvent {
  | Event(event) => onEventToView->Chan.emit(event)
  | Request(req) => onRequest->Chan.emit(req)
  }
})

// relay onResponse => VSCode.Api.postMessage
let onResponse = Chan.make()
onResponse->Chan.on(response => {
  open View.ResponseOrEventFromView
  vscode->VSCode.Api.postMessage(encode(Response(response)))
})

// relay onEventFromView => VSCode.Api.postMessage
let onEventFromView = Chan.make()
onEventFromView->Chan.on(event => {
  open View.ResponseOrEventFromView
  vscode->VSCode.Api.postMessage(encode(Event(event)))
})

// mount the view at the "root" element
Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)->Option.forEach(element =>
  ReactDOMRe.render(<Panel onRequest onEventToView onResponse onEventFromView />, element)
)
