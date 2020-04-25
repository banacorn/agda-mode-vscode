let vscode = Guacamole.Vscode.Api.acquireVsCodeApi();

[@react.component]
let make = () => {
  let (header, setHeader) = React.useState(() => "");
  let (body, setBody) = React.useState(() => "");

  // React.useEffect1(
  //   () => {
  //     Js.log("init");
  //     vscode->Guacamole.Vscode.Api.postMessage("from view");
  //     Guacamole.Vscode.Api.onMessage(msg => {
  //       Js.log2(" >>> ", msg);
  //       switch (msg) {
  //       | View.Display(header, body) =>
  //         setHeader(_ => header);
  //         setBody(_ => body);
  //       };
  //     });
  //     None;
  //   },
  //   [||],
  // );

  <section className="agda-mode native-key-bindings" tabIndex=(-1)>
    <div className="agda-mode-header"> {ReasonReact.string(header)} </div>
    <div className="agda-mode-body"> {ReasonReact.string(body)} </div>
  </section>;
};