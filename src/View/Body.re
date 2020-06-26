open ReasonReact;
open Belt;
[@react.component]
let make = (~body: View.Body.t, ~onSubmit: option(string) => unit) => {
  switch (body) {
  | Nothing => <> </>
  | Plain(text) => <div className="agda-mode-body"> {string(text)} </div>
  | Query(placeholder, value) =>
    let placeholder = placeholder->Option.getWithDefault("");
    let (value, setValue) =
      React.useState(_ => value->Option.getWithDefault(""));
    let onChange = event => {
      let value = event->ReactEvent.Form.target##value;
      setValue(_ => value);
    };
    let onSubmit = _ => onSubmit(Some(value));

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