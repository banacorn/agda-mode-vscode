@react.component
let make = (~items: View.Body.t) =>
  switch items {
  | [] => <> </>
  | items =>
    <div className="agda-mode-body">
      <ul>
        {items->Array.mapWithIndex((item, i) => <Item key={string_of_int(i)} item />)->React.array}
      </ul>
    </div>
  }
