open ReasonReact;
open Belt;
[@react.component]
let make = (~body: View.Request.Body.t, ~onSubmit: option(string) => unit) => {
  switch (body) {
  | Nothing => <> </>
  | Plain(text) => <div className="agda-mode-body"> {string(text)} </div>
  | Inquire(placeholder, value) =>
    let placeholder = placeholder->Option.getWithDefault("");
    let (value, setValue) = React.useState(_ => value);
    let onChange = event => {
      let value = event->ReactEvent.Form.target##value;
      setValue(_ => Some(value));
    };
    let onSubmit = _ => onSubmit(value);

    <div className="agda-mode-body">
      <form onSubmit> <input type_="text" placeholder onChange /> </form>
    </div>;
  };
};