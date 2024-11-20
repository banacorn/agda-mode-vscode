open React

@react.component
let make = (~candidates: array<string>, ~index: int, ~onChooseSymbol: string => unit) => {
  let rowStart = index / 10 * 10
  let row = candidates->Array.slice(~start=rowStart, ~end=rowStart + 10)

  <div className="agda-mode-keyboard-candidates">
    {switch candidates[index] {
    | None => <> </>
    | Some(_) =>
      row
      ->Array.mapWithIndex((key, i) => {
        let isSelected = rowStart + i === index
        <button
          className={"agda-mode-key " ++ (isSelected ? "selected" : "")}
          onClick={_ => onChooseSymbol(key)}
          key>
          {string(key)}
        </button>
      })
      ->array
    }}
  </div>
}
