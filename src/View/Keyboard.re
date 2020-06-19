open ReasonReact;

type state =
  | Activated(string, array(string))
  | Deactivated;

[@react.component]
let make = (~state: state) => {
  let (activated, sequence, suggestions) =
    switch (state) {
    | Activated(sequence, suggestions) => (
        " activated",
        sequence,
        suggestions,
      )
    | Deactivated => (" deactivated", "", [||])
    };

  <div className={"agda-mode-keyboard" ++ activated}>
    <div className="agda-mode-keyboard-sequence-container">
      <div className="agda-mode-keyboard-sequence"> {string(sequence)} </div>
    </div>
    <div className="agda-mode-keyboard-suggestions">
      {string(Util.Pretty.array(suggestions))}
    </div>
  </div>;
};