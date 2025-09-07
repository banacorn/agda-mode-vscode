open React

@react.component
let make = (~header: View.Header.t, ~connectionStatus: string) => {
  let connectionStatus = <div className="agda-mode-header-connection-status"> {string(connectionStatus)} </div>
  switch header {
  | Plain(text) => <div className="agda-mode-header"> {string(text)} {connectionStatus} </div>
  | Success(text) => <div className="agda-mode-header success"> {string(text)} {connectionStatus} </div>
  | Warning(text) => <div className="agda-mode-header warning"> {string(text)} {connectionStatus} </div>
  | Error(text) => <div className="agda-mode-header error"> {string(text)} {connectionStatus} </div>
  }
}
