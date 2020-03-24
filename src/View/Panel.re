open Belt;

let vscode = Vscode.Api.acquireVsCodeApi();

vscode->Vscode.Api.postMessage("from view");
Vscode.Api.onMessage((msg: View.message) => {
  switch (msg) {
  | View.CannotFindAgda => Js.log("Cannot find agda")
  | View.Success => Js.log("Success")
  }
});

Webapi.Dom.Document.getElementById("root", Webapi.Dom.document)
->Option.forEach(element => {
    ReactDOMRe.render(
      <section className="agda-mode">
        <div className="agda-mode-header">
          {ReasonReact.string("head!!")}
        </div>
        <div className="agda-mode-body">
          {ReasonReact.string("body :D")}
        </div>
      </section>,
      element,
    )
  });