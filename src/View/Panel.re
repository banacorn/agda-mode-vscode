open Belt;

[@react.component]
let make =
    (
      ~imageUri: option(string),
      ~onRequest: Event.t(View.Request.t),
      ~onEventToView: Event.t(View.EventToView.t),
      ~onResponse: Event.t(View.Response.t),
      ~onEventFromView: Event.t(View.EventFromView.t),
    ) => {
  let (shouldPrank, setShouldPrank) = React.useState(() => None);
  let (header, setHeader) =
    React.useState(() => View.Header.Plain("File not loaded yet"));
  let (body, setBody) = React.useState(() => View.Body.Nothing);
  let (inputMethodState, runInputMethodAction) =
    React.useReducer(Keyboard.reducer, None);
  let querying =
    switch (body) {
    | Query(_, _) => true
    | _ => false
    };

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
    | Query(header, placeholder, value) =>
      let (promise, resolve) = Promise.pending();
      queryResponseResolver.current = Some(resolve);
      setHeader(_ => Plain(header));
      setBody(_ => Query(placeholder, value));
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
    | QueryInterrupt => onSubmit(None)
    | QueryUpdate(text) =>
      setBody(
        fun
        | Query(placeholder, _) => Query(placeholder, Some(text))
        | others => others,
      )
    | Display(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
    | Prank =>
      let quotes = [|
        {j|Why would you do that?|j},
        {j|Tut, tut, tut. Naughty, naughty, you’ll get caughty.|j},
      |];
      let quoteNumber = Array.length(quotes);
      let selected = Js.Math.random_int(0, quoteNumber);
      let quote =
        quotes[selected]->Option.getWithDefault("Why would you do that?");
      switch (imageUri) {
      | None => ()
      | Some(imageUri) => setShouldPrank(_ => Some((imageUri, quote)))
      };
    }
  );

  <Component__Link.Provider value=onEventFromView>
    <section className="agda-mode native-key-bindings" tabIndex=(-1)>
      <Keyboard
        state=inputMethodState
        onInsertChar={char => {
          onEventFromView.emit(InputMethod(InsertChar(char)))
        }}
        onChooseSymbol={symbol => {
          onEventFromView.emit(InputMethod(ChooseSymbol(symbol)))
        }}
        querying
      />
      {switch (shouldPrank) {
       | None => <> <Header header /> <Body body onSubmit onChange /> </>
       | Some((imageUri, quote)) =>
         <div className="agda-mode-body">
           <div id="prank">
             <blockquote> {React.string(quote)} </blockquote>
             <img src=imageUri />
           </div>
         </div>
       }}
    </section>
  </Component__Link.Provider>;
};
