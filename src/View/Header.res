open React

@react.component
let make = (~header: View.Header.t, ~status: string) => {
  let status = <div className="agda-mode-header-status"> {string(status)} </div>
  switch header {
  | Plain(text) => <div className="agda-mode-header"> {string(text)} {status} </div>
  | Success(text) => <div className="agda-mode-header success"> {string(text)} {status} </div>
  | Warning(text) => <div className="agda-mode-header warning"> {string(text)} {status} </div>
  | Error(text) => <div className="agda-mode-header error"> {string(text)} {status} </div>
  }
}
