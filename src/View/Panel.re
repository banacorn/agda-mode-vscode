[@react.component]
let make =
    (
      ~onRequest: Event.t(View.Request.t),
      ~onResponse: Event.t(View.Response.t),
    ) => {
  let (header, setHeader) =
    React.useState(() => View.Request.Header.Plain("Loading ..."));
  let (body, setBody) = React.useState(() => View.Request.Body.Nothing);

  // emit event Initialized on mount
  React.useEffect1(
    () => {
      onResponse.emit(View.Response.Event(Initialized));
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
  Hook.on(onRequest, msg =>
    switch (msg) {
    | Plain(header, Query(placeholder, value)) =>
      Js.log("[ view ] >>> Query");
      let (promise, resolve) = Promise.pending();
      resolver.current = Some(resolve);
      setHeader(_ => header);
      setBody(_ => Query(placeholder, value));
      promise->Promise.get(
        fun
        | None => {
            Js.log("[ view ] <<< Query Interrupted");
            onResponse.emit(View.Response.QueryInterrupted);
          }
        | Some(result) => {
            Js.log("[ view ] <<< QuerySuccess");
            onResponse.emit(View.Response.QuerySuccess(result));
          },
      );
    | Plain(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
      onResponse.emit(View.Response.Success);
    | InterruptQuery =>
      Js.log("[ view ] >>> Interrupt Query");
      onSubmit(None);
    | _ => onResponse.emit(View.Response.Success)
    }
  );

  <section className="agda-mode native-key-bindings" tabIndex=(-1)>
    <Header header />
    <Body body onSubmit />
  </section>;
};