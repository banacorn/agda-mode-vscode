[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onResponse: Event.t(View.Response.t),
    ) => {
  let (header, setHeader) =
    React.useState(() => View.Request.Header.Plain("Loading ..."));
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);
  let (inputMethodState, runInputMethodAction) =
    React.useReducer(
      (state, action) =>
        switch (state, action) {
        | (_, View.Request.InputMethod.Activate) =>
          let initTranslation = Translator.translate("");
          Some(
            Keyboard.{
              sequence: "",
              suggestions: initTranslation.keySuggestions,
              candidates: initTranslation.candidateSymbols,
              candidateIndex: 0,
            },
          );
        | (_, Deactivate) => None
        | (None, _) => None
        | (Some(state), Update(sequence, suggestions, candidates)) =>
          Some({
            sequence,
            suggestions,
            candidates,
            candidateIndex: state.candidateIndex,
          })
        | (
            Some({Keyboard.sequence, suggestions, candidates, candidateIndex}),
            MoveUp,
          ) =>
          Some({
            sequence,
            suggestions,
            candidates,
            candidateIndex: max(0, candidateIndex - 10),
          })
        | (
            Some({Keyboard.sequence, suggestions, candidates, candidateIndex}),
            MoveRight,
          ) =>
          Some({
            sequence,
            suggestions,
            candidates,
            candidateIndex:
              min(Array.length(candidates) - 1, candidateIndex + 1),
          })
        | (
            Some({Keyboard.sequence, suggestions, candidates, candidateIndex}),
            MoveDown,
          ) =>
          Some({
            sequence,
            suggestions,
            candidates,
            candidateIndex:
              min(Array.length(candidates) - 1, candidateIndex + 10),
          })
        | (
            Some({Keyboard.sequence, suggestions, candidates, candidateIndex}),
            MoveLeft,
          ) =>
          Some({
            sequence,
            suggestions,
            candidates,
            candidateIndex: max(0, candidateIndex - 1),
          })
        },
      None,
    );

  // emit event Initialized on mount
  React.useEffect1(
    () => {
      onResponse.emit(View.Response.EventPiggyBack(Initialized));
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
        onResponse.emit(
          View.Response.EventPiggyBack(InputMethod(InsertChar(char))),
        );
      }}
    />
    <Header header />
    <Body body onSubmit />
  </section>;
};