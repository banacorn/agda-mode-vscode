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
    let items = [|Item.PlainText(Text.parse(payload))|];
    <div className="agda-mode-body">
      <ul>
        {items
         ->Array.mapWithIndex((i, item) =>
             <Item key={string_of_int(i)} item />
           )
         ->React.array}
      </ul>
    </div>;
  | Emacs(kind, header, body) =>
    let items =
      switch (kind) {
      | ContextOrConstraints => Emacs__Parser2.parseContextOrConstraints(body)
      | AllGoalsWarnings => Emacs__Parser2.parseAllGoalsWarnings(header, body)
      | GoalType => Emacs__Parser2.parseGoalType(body)
      | SearchAbout => Emacs__Parser2.parseSearchAbout(body)
      | Error => Emacs__Parser2.parseError(body)
      | Others => Emacs__Parser2.parseOthers(body)
      };
    <div className="agda-mode-body">
      <ul>
        {items
         ->Array.mapWithIndex((i, item) =>
             <Item key={string_of_int(i)} item />
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
