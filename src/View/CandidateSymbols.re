open ReasonReact;
open Belt;

[@react.component]
let make = (~candidates: array(string), ~index: int) => {
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