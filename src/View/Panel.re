open Belt;

[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onEventToView: Event.t(View.EventToView.t),
      ~onResponse: Event.t(View.Response.t),
      ~onEventFromView: Event.t(View.EventFromView.t),
    ) => {
  let (header, setHeader) =
    React.useState(() => View.Header.Plain("File not loaded yet"));
  let (body, setBody) = React.useState(() => View.Body.Nothing);
  let (prompt, setPrompt) = React.useState(() => None);
  let prompting = prompt->Option.isSome;

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

  let queryResponseResolver = React.useRef(None);
  let onSubmit = result =>
    queryResponseResolver.current
    ->Option.forEach(resolve => {
        resolve(result);
        queryResponseResolver.current = None;
      });
  let onChange = string => onEventFromView.emit(QueryChange(string));

  // on receiving View Requests
  Hook.recv(onRequest, onResponse, msg =>
    switch (msg) {
    | Prompt(header, body, placeholder, value) =>
      let (promise, resolve) = Promise.pending();
      queryResponseResolver.current = Some(resolve);
      setHeader(_ => Plain(header));
      setPrompt(_ => Some((body, placeholder, value)));
      promise->Promise.map(
        fun
        | None => View.Response.QueryInterrupted
        | Some(result) => View.Response.QuerySuccess(result),
      );
    }
  );

  // on receiving Events to View
  Hook.on(onEventToView, event =>
    switch (event) {
    | InputMethod(action) => runInputMethodAction(action)
    | PromptInterrupt => onSubmit(None)
    | PromptUpdate(text) =>
      setPrompt(
        fun
        | Some((body, placeholder, _)) =>
          Some((body, placeholder, Some(text)))
        | None => None,
      )
    | Display(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
    }
  );

  <Component__Link.Provider value=onEventFromView>
    <section className="agda-mode native-key-bindings" tabIndex=(-1)>
      <div className="agda-mode-header-container">
        <Header header />
        <Prompt prompt onChange onSubmit />
        <Keyboard
          state=inputMethodState
          onInsertChar={char => {
            onEventFromView.emit(InputMethod(InsertChar(char)))
          }}
          onChooseSymbol={symbol => {
            onEventFromView.emit(InputMethod(ChooseSymbol(symbol)))
          }}
          prompting
        />
      </div>
      <Body body />
    </section>
  </Component__Link.Provider>;
};
