open ReasonReact;

type state =
  | Activated(string)
  | Deactivated;

[@react.component]
let make = (~state: state) => {
  let (activated, sequence) =
    switch (state) {
    | Activated(sequence) => (" activated", sequence)
    | Deactivated => (" deactivated", "")
    };

  <div className={"agda-mode-keyboard" ++ activated}>
    <div className="agda-mode-keyboard-sequence-container">
      <div className="agda-mode-keyboard-sequence"> {string(sequence)} </div>
    </div>
  </div>;
};