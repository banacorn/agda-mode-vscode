open VSCode;
open! Belt;

// As this so called "WebView" is isolated and independent from the Extension
// this is the only way to send messages back to the extension
let vscode = Api.acquireVsCodeApi();

// relay VSCode.Api.onMessage => onRequest or onEvent;
let onRequest = Event.make();
let onEventToView = Event.make();
Api.onMessage(stringifiedJSON => {
  let requestOrEvent =
    stringifiedJSON->Js.Json.parseExn->View.RequestOrEventToView.decode;

  switch (requestOrEvent) {
  | Event(event) => onEventToView.emit(event)
  | Request(req) => onRequest.emit(req)
  };
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
    ReactDOMRe.render(
      <Panel onRequest onEventToView onResponse onEventFromView />,
      element,
    )
  });
