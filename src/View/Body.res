open Belt
open Component
@react.component
let make = (~body: View.Body.t) =>
  switch body {
  | Nothing => <> </>
  | RichText => <> </>
  | Plain(payload) =>
    let items = [Item.PlainText(Text.parse(payload))]
    <div className="agda-mode-body">
      <ul>
        {items->Array.mapWithIndex((i, item) => <Item key={string_of_int(i)} item />)->React.array}
      </ul>
    </div>
  | Emacs(kind, header, body) =>
    let items = switch kind {
    | Outputs => Emacs__Parser2.parseOutputs(body)
    | AllGoalsWarnings => Emacs__Parser2.parseAllGoalsWarnings(header, body)
    | GoalType => Emacs__Parser2.parseGoalType(body)
    | SearchAbout => Emacs__Parser2.parseSearchAbout(body)
    | Error => Emacs__Parser2.parseError(body)
    | Text => Emacs__Parser2.parseText(body)
    }
    <div className="agda-mode-body">
      <ul>
        {items->Array.mapWithIndex((i, item) => <Item key={string_of_int(i)} item />)->React.array}
      </ul>
    </div>
  }
