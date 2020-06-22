[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onEventToView: Event.t(View.EventToView.t),
      ~onResponse: Event.t(View.Response.t),
      ~onEventFromView: Event.t(View.EventFromView.t),
    ) => {
  let (header, setHeader) =
    React.useState(() => View.Header.Plain("Loading ..."));
  let (body, setBody) = React.useState(() => View.Body.Nothing);
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

  // on receiving View Requests
  Hook.recv(onRequest, onResponse, msg =>
    switch (msg) {
    | Query(header, placeholder, value) =>
      let (promise, resolve) = Promise.pending();
      resolver.current = Some(resolve);
      setHeader(_ => Plain(header));
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
    }
  );

  // on receiving Events to View
  Hook.on(onEventToView, event =>
    switch (event) {
    | InputMethod(action) => runInputMethodAction(action)
    | InterruptQuery => onSubmit(None)
    | Display(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
    | Show => ()
    | Hide => ()
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