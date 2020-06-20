open ReasonReact;
open Belt;

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
let make = (~candidates: array(string)) => {
  let (index, move) =
    React.useReducer(reducer(Array.length(candidates)), 0);

  let rowStart = index / 10 * 10;
  let row = candidates->Array.slice(~offset=rowStart, ~len=10);

  <div className="agda-mode-keyboard-candidates">
    {switch (candidates[index]) {
     | None => <> </>
     | Some(_) =>
       row
       ->Array.mapWithIndex((i, key) => {
           let isSelected = rowStart + i === index;
           <button
             className={"agda-mode-key " ++ (isSelected ? "selected" : "")}
             onClick={_ => Js.log(key)}
             key>
             {string(key)}
           </button>;
         })
       ->array
     }}
  </div>;
};