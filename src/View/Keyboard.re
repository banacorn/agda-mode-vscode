open ReasonReact;

[@react.component]
let make = (~activated: bool, ~sequence: string) => {
  let activated = activated ? " activated" : " deactivated";
  <div className={"agda-mode-keyboard" ++ activated}>
    <div className="agda-mode-keyboard-sequence-container">
      <div className="agda-mode-keyboard-sequence"> {string(sequence)} </div>
    </div>
  </div>;
};