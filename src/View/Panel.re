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
  // save Header & Body up
  // so that we can restore them if the prompt is interrupted
  let savedHeaderAndBody = React.useRef(None);
  let saveHeaderAndBody = (header, body) =>
    savedHeaderAndBody.current = Some((header, body));
  let restoreHeaderAndBody = () =>
    savedHeaderAndBody.current
    ->Option.forEach(((header, body)) => {
        setHeader(_ => header);
        setBody(_ => body);
        // clear the saved Header & Body
        savedHeaderAndBody.current = None;
      });

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

  let promptResponseResolver = React.useRef(None);
  let onSubmit = result =>
    promptResponseResolver.current
    ->Option.forEach(resolve => {
        setPrompt(_ => None);
        resolve(result);
        promptResponseResolver.current = None;
      });
  let onChange = string => onEventFromView.emit(PromptChange(string));

  // on receiving View Requests
  Hook.recv(onRequest, onResponse, msg =>
    switch (msg) {
    | Prompt(header', {body: body', placeholder, value}) =>
      // set the view
      setHeader(_ => header');
      setBody(_ => Nothing);
      setPrompt(_ => Some((body', placeholder, value)));

      let (promise, resolve) = Promise.pending();
      promptResponseResolver.current = Some(resolve);
      promise->Promise.map(
        fun
        | None => {
            Js.log("NOTHING BACK");
            restoreHeaderAndBody();
            View.Response.PromptInterrupted;
          }
        | Some(result) => View.Response.PromptSuccess(result),
      );
    }
  );

  // on receiving Events to View
  Hook.on(onEventToView, event =>
    switch (event) {
    | InputMethod(action) => runInputMethodAction(action)
    | PromptIMUpdate(text) =>
      setPrompt(
        fun
        | Some((body, placeholder, _)) =>
          Some((body, placeholder, Some(text)))
        | None => None,
      )
    | PromptInterrupt =>
      onSubmit(None);
      setPrompt(_ => None);
      restoreHeaderAndBody();
    | Display(header, body) =>
      onSubmit(None);
      saveHeaderAndBody(header, body);
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
