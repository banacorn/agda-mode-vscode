open ReasonReact;

[@react.component]
let make = (~activated: bool, ~sequence: string) => {
  let activated = activated ? " activated" : " deactivated";
  <div className={"agda-mode-keyboard" ++ activated}>
    {string(sequence)}
  </div>;
};