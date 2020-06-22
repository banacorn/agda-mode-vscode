open VSCode;
open! Belt;

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = Api.acquireVsCodeApi();

// relay VSCode.Api.onMessage => onRequest;
let onRequest = Event.make();
Api.onMessage(stringifiedJSON => {
  let request = stringifiedJSON->Js.Json.parseExn->View.Request.decode;
  onRequest.emit(request);
});

// relay onResponse => VSCode.Api.postMessage
let onResponse = Event.make();
onResponse.on(response => {
  View.ResponseOrEventFromView.(
    vscode->Api.postMessage(encode(Response(response)))
  )
});

// relay onEventFromView => VSCode.Api.postMessage
let onEventFromView = Event.make();
onEventFromView.on(event => {
  View.ResponseOrEventFromView.(
    vscode->Api.postMessage(encode(Event(event)))
  )
});

// mount the view at the "root" element
Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)
->Option.forEach(element => {
    ReactDOMRe.render(<Panel onRequest onResponse onEventFromView />, element)
  });