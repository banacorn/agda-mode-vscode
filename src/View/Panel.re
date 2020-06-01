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

  // receiving View Requests
  Hook.on(onRequest, msg =>
    switch (msg) {
    | Plain(header, Inquire(placeholder, value)) =>
      let (promise, resolve) = Promise.pending();
      resolver.current = Some(resolve);
      setHeader(_ => header);
      setBody(_ => Inquire(placeholder, value));
      promise->Promise.get(result =>
        onResponse.emit(View.Response.QuerySuccess(result))
      );
    // View.Response.InquiryResult();
    | Plain(header, body) =>
      setHeader(_ => header);
      setBody(_ => body);
      onResponse.emit(View.Response.Success);
    | _ => onResponse.emit(View.Response.Success)
    }
  );

  let onSubmit = result =>
    switch (resolver.current) {
    | None => ()
    | Some(resolve) => resolve(result)
    };

  <section className="agda-mode native-key-bindings" tabIndex=(-1)>
    <Header header />
    <Body body onSubmit />
  </section>;
};