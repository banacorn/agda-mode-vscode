open ReasonReact;
open Belt;

type state =
  | Activated(string, array(string), array(string))
  | Deactivated;

[@react.component]
let make = (~state: state, ~onInsertChar: string => unit) => {
  let (activated, sequence, suggestions, candidates) =
    switch (state) {
    | Activated(sequence, suggestions, candidates) => (
        " activated",
        sequence,
        suggestions,
        candidates,
      )
    | Deactivated => (" deactivated", "", [||], [||])
    };

  <div className={"agda-mode-keyboard" ++ activated}>
    <div className="agda-mode-keyboard-sequence-and-candidates">
      <div className="agda-mode-keyboard-sequence"> {string(sequence)} </div>
      <CandidateSymbols candidates />
    </div>
    <div className="agda-mode-keyboard-suggestions">
      {suggestions
       ->Array.map(key => {
           <button
             className="agda-mode-key" onClick={_ => onInsertChar(key)} key>
             {string(key)}
           </button>
         })
       ->array}
    </div>
  </div>;
  // {string(Util.Pretty.array(suggestions))}
};