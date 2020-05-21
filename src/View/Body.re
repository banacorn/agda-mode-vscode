open ReasonReact;

[@react.component]
let make = (~body: option(string)) => {
  switch (body) {
  | None => <> </>
  | Some(text) => <div className="agda-mode-body"> {string(text)} </div>
  };
};