// let vscode = Guacamole.Vscode.Api.acquireVsCodeApi();

open ReasonReact;

[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onResponse: Event.t(View.Response.t),
    ) => {
  let (header, setHeader) =
    React.useState(() => View.Request.Header.Plain("Loading ..."));
  let (body, setBody) = React.useState(() => None);

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
    <Header header />
    <Body body />
  </section>;
};