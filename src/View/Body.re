open ReasonReact;
open Belt;
[@react.component]
let make = (~body: View.Request.Body.t, ~onSubmit: string => unit) => {
  switch (body) {
  | Nothing => <> </>
  | Plain(text) => <div className="agda-mode-body"> {string(text)} </div>
  | Inquire(placeholder, value) =>
    let placeholder = placeholder->Option.getWithDefault("");
    let (value, setValue) =
      React.useState(_ => value->Option.getWithDefault(""));
    let onChange = event => {
      let value = event->ReactEvent.Form.target##value;
      setValue(_ => value);
    };
    let onSubmit = _ => onSubmit(value);

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
            ->Option.forEach(element => {
                Js.log(element);
                Js.Global.setTimeout(
                  () => Webapi.Dom.HtmlElement.focus(element),
                  1000,
                )
                ->ignore;
              })
          )}
        />
      </form>
    </div>;
  };
};