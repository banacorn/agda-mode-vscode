open ReasonReact;

[@react.component]
let make = (~body: View.Request.Body.t) => {
  switch (body) {
  | Nothing => <> </>
  | Plain(text) => <div className="agda-mode-body"> {string(text)} </div>
  | Inquire(placeholder, value) =>
    <div className="agda-mode-body"> {string("INPUT BOX")} </div>
  };
};