open ReasonReact

@react.component
let make = (~header: View.Header.t) =>
  switch header {
  | Plain(text) => <div className="agda-mode-header"> {string(text)} </div>
  | Success(text) => <div className="agda-mode-header success"> {string(text)} </div>
  | Warning(text) => <div className="agda-mode-header warning"> {string(text)} </div>
  | Error(text) => <div className="agda-mode-header error"> {string(text)} </div>
  }
