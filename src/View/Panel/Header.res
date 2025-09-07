open React

@react.component
let make = (~header: View.Header.t, ~connectionStatus: string, ~onConnectionStatusClick: unit => unit) => {
  let connectionStatusElement = 
    <div 
      className="agda-mode-header-connection-status clickable" 
      onClick={_ => onConnectionStatusClick()}
      title="Click to switch Agda version"
    > 
      {string(connectionStatus)} 
    </div>
  switch header {
  | Plain(text) => <div className="agda-mode-header"> {string(text)} {connectionStatusElement} </div>
  | Success(text) => <div className="agda-mode-header success"> {string(text)} {connectionStatusElement} </div>
  | Warning(text) => <div className="agda-mode-header warning"> {string(text)} {connectionStatusElement} </div>
  | Error(text) => <div className="agda-mode-header error"> {string(text)} {connectionStatusElement} </div>
  }
}
