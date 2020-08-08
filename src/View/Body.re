open Belt;
open Component;
[@react.component]
let make =
    (
      ~body: View.Body.t,
      ~onSubmit: option(string) => unit,
      ~onChange: string => unit,
    ) => {
  switch (body) {
  | Nothing => <> </>
  | Plain(payload) =>
    <div className="agda-mode-body"> <Emacs__PlainText payload /> </div>
  | AllGoalsWarnings(header, body) =>
    <div className="agda-mode-body">
      <Emacs__AllGoalsWarnings header body />
    </div>
  | GoalType(payload) =>
    <div className="agda-mode-body"> <Emacs__GoalType payload /> </div>
  | Error(payload) =>
    let labeledItems = Emacs__Parser2.parseError(payload);
    <div className="agda-mode-body">
      <ul>
        {labeledItems
         ->Array.mapWithIndex((i, payload) =>
             <LabeledItem key={string_of_int(i)} payload />
           )
         ->React.array}
      </ul>
    </div>;
  | Query(placeholder, value) =>
    let placeholder = placeholder->Option.getWithDefault("");
    let value = value->Option.getWithDefault("");

    let onChange = event => {
      let value = event->ReactEvent.Form.target##value;
      onChange(value);
    };

    let onSubmit = _event => onSubmit(Some(value));

    <div className="agda-mode-query">
      <form onSubmit>
        <input
          type_="text"
          placeholder
          onChange
          value
          ref={ReactDOMRe.Ref.callbackDomRef(ref => {
            // HACK
            // somehow focus() won't work on some machines (?)
            // delay focus() 100ms to regain focus
            Js.Global.setTimeout(
              () => {
                ref
                ->Js.Nullable.toOption
                ->Option.flatMap(Webapi.Dom.Element.asHtmlElement)
                ->Option.forEach(Webapi.Dom.HtmlElement.focus);
                ();
              },
              100,
            )
            ->ignore;
            ();
          })}
        />
      </form>
    </div>;
  };
};
