[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onResponse: Event.t(View.Response.t),
      ~onEventFromView: Event.t(View.EventFromView.t),
    ) => {
  let (header, setHeader) =
    React.useState(() => View.Request.Header.Plain("Loading ..."));
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);
  let (inputMethodState, runInputMethodAction) =
    React.useReducer(Keyboard.reducer, None);

  // emit event Initialized on mount
  React.useEffect1(
    () => {
      onEventFromView.emit(Initialized);
      None;
    },
    [||],
  );

  let resolver = React.useRef(None);
  let onSubmit = result =>
    switch (resolver.current) {
    | None => ()
    | Some(resolve) =>
      resolve(result);
      resolver.current = None;
    };

  // receiving View Requests
  Hook.on(onRequest, onResponse, msg =>
    switch (msg) {
    | Plain(header, Query(placeholder, value)) =>
      let (promise, resolve) = Promise.pending();
      resolver.current = Some(resolve);
      setHeader(_ => header);
      setBody(_ => Query(placeholder, value));
      promise->Promise.map(
        fun
        | None => {
            View.Response.QueryInterrupted;
          }
        | Some(result) => {
            View.Response.QuerySuccess(result);
          },
      );
    | Plain(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
      Promise.resolved(View.Response.Success);
    | InterruptQuery =>
      onSubmit(None);
      Promise.resolved(View.Response.QueryInterrupted);
    | InputMethod(action) =>
      runInputMethodAction(action);
      Promise.resolved(View.Response.Success);
    | _ => Promise.resolved(View.Response.Success)
    }
  );

  <section className="agda-mode native-key-bindings" tabIndex=(-1)>
    <Keyboard
      state=inputMethodState
      onInsertChar={char => {
        Js.log("onInsertChar " ++ char);
        onEventFromView.emit(InputMethod(InsertChar(char)));
      }}
      onChooseSymbol={symbol => {
        Js.log("onChooseSymbol " ++ symbol);
        onEventFromView.emit(InputMethod(ChooseSymbol(symbol)));
      }}
    />
    <Header header />
    <Body body onSubmit />
  </section>;
};