// let vscode = Guacamole.Vscode.Api.acquireVsCodeApi();

open ReasonReact;

[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onResponse: Event.t(View.Response.t),
    ) => {
  let (header, setHeader) = React.useState(() => ":D");
  let (body, setBody) = React.useState(() => ":(");

  // response with Initialized on mount
  React.useEffect1(
    () => {
      onResponse.emit(View.Response.Initialized);
      None;
    },
    [||],
  );

  // receiving View Requests
  Hook.on(onRequest, msg =>
    switch (msg) {
    | Plain(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
    | _ => ()
    }
  );

  <section className="agda-mode native-key-bindings" tabIndex=(-1)>
    <div className="agda-mode-header"> {string(header)} </div>
    <div className="agda-mode-body"> {string(body)} </div>
  </section>;
};