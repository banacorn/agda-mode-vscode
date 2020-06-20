open ReasonReact;
open Belt;

type state = {
  sequence: string,
  suggestions: array(string),
  candidates: array(string),
  candidateIndex: int,
};

// let (index, move) = React.useReducer(reducer(Array.length(candidates)), 0);
type action =
  | Up
  | Down
  | Right
  | Left;

let reducer = (totalSize, index, action) =>
  switch (action) {
  | Up => max(0, index - 10)
  | Right => min(totalSize - 1, index + 1)
  | Down => min(totalSize - 1, index + 10)
  | Left => max(0, index - 1)
  };

[@react.component]
let make = (~state: option(state), ~onInsertChar: string => unit) => {
  switch (state) {
  | None => <div className="agda-mode-keyboard deactivated" />
  | Some({sequence, suggestions, candidates, candidateIndex}) =>
    <div className="agda-mode-keyboard">
      <div className="agda-mode-keyboard-sequence-and-candidates">
        <div className="agda-mode-keyboard-sequence">
          {string(sequence)}
        </div>
        <CandidateSymbols candidates index=candidateIndex />
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
    </div>
  };
};