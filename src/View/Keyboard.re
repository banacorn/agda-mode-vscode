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

let reducer = (state, action) =>
  switch (state, action) {
  | (_, View.Request.InputMethod.Activate) =>
    let initTranslation = Translator.translate("");
    Some({
      sequence: "",
      suggestions: initTranslation.keySuggestions,
      candidates: initTranslation.candidateSymbols,
      candidateIndex: 0,
    });
  | (_, Deactivate) => None
  | (None, _) => None
  | (Some(state), Update(sequence, suggestions, candidates)) =>
    Some({
      sequence,
      suggestions,
      candidates,
      candidateIndex: state.candidateIndex,
    })
  | (Some(state), MoveUp) =>
    Some({...state, candidateIndex: max(0, state.candidateIndex - 10)})
  | (Some(state), MoveRight) =>
    Some({
      ...state,
      candidateIndex:
        min(Array.length(state.candidates) - 1, state.candidateIndex + 1),
    })
  | (Some(state), MoveDown) =>
    Some({
      ...state,
      candidateIndex:
        min(Array.length(state.candidates) - 1, state.candidateIndex + 10),
    })
  | (Some(state), MoveLeft) =>
    Some({...state, candidateIndex: max(0, state.candidateIndex - 1)})
  };

[@react.component]
let make =
    (
      ~state: option(state),
      ~onInsertChar: string => unit,
      ~onChooseSymbol: string => unit,
    ) => {
  switch (state) {
  | None => <div className="agda-mode-keyboard deactivated" />
  | Some({sequence, suggestions, candidates, candidateIndex}) =>
    <div className="agda-mode-keyboard">
      <div className="agda-mode-keyboard-sequence-and-candidates">
        <div className="agda-mode-keyboard-sequence">
          {string(sequence)}
        </div>
        <CandidateSymbols candidates index=candidateIndex onChooseSymbol />
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