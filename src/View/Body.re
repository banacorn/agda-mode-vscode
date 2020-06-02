open ReasonReact;
open Belt;
[@react.component]
let make = (~body: View.Request.Body.t, ~onSubmit: option(string) => unit) => {
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

    <div className="agda-mode-body">
      <form onSubmit>
        <input
          type_="text"
          placeholder
          onChange
          value
          ref={ReactDOMRe.Ref.callbackDomRef(ref =>
            ref
            ->Js.Nullable.toOption
            ->Option.flatMap(Webapi.Dom.Element.asHtmlElement)
            ->Option.forEach(Webapi.Dom.HtmlElement.focus)
          )}
        />
      </form>
    </div>;
  };
};