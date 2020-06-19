open ReasonReact;

[@react.component]
let make = (~activated: bool) => {
  let activated = activated ? " activated" : " deactivated";
  <div className={"agda-mode-keyboard" ++ activated}>
    {string("deactivated")}
  </div>;
};