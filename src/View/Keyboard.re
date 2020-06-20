open ReasonReact;
open Belt;

type state = {
  sequence: string,
  translation: Translator.translation,
  candidateIndex: int,
};

let reducer = (state, action) =>
  switch (state, action) {
  | (_, View.Request.InputMethod.Activate) =>
    let translation = Translator.translate("");
    Some({sequence: "", translation, candidateIndex: 0});
  | (_, Deactivate) => None
  | (None, _) => None
  | (Some(state), Update(sequence, translation)) =>
    Some({sequence, translation, candidateIndex: state.candidateIndex})
  | (Some(state), MoveUp) =>
    Some({...state, candidateIndex: max(0, state.candidateIndex - 10)})
  | (Some(state), MoveRight) =>
    Some({
      ...state,
      candidateIndex:
        min(
          Array.length(state.translation.candidateSymbols) - 1,
          state.candidateIndex + 1,
        ),
    })
  | (Some(state), MoveDown) =>
    Some({
      ...state,
      candidateIndex:
        min(
          Array.length(state.translation.candidateSymbols) - 1,
          state.candidateIndex + 10,
        ),
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
  | Some({sequence, translation, candidateIndex}) =>
    <div className="agda-mode-keyboard">
      <div className="agda-mode-keyboard-sequence-and-candidates">
        <div className="agda-mode-keyboard-sequence">
          {string(sequence)}
        </div>
        <CandidateSymbols
          candidates={translation.candidateSymbols}
          index=candidateIndex
          onChooseSymbol
        />
      </div>
      <div className="agda-mode-keyboard-suggestions">
        {translation.keySuggestions
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