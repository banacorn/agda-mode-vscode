let chan: Chan.t<Common.EventFromView.t> = Chan.make()
let eventContext = React.createContext(chan)

module Provider = {
  let makeProps = (~value, ~children, ()) =>
    {
      "value": value,
      "children": children,
    }

  let make = React.Context.provider(eventContext)
}

@react.component
let make = (
  ~target=Common.Link.SrcLoc(NoRange),
  ~jump=false,
  ~hover=false,
  ~className=[],
  ~children,
) => {
  let sanitizedTarget = switch target {
  | SrcLoc(NoRange)
  | SrcLoc(Range(_, [])) =>
    None
  | Hole(index) => Some(Common.Link.Hole(index))
  | SrcLoc(range) => Some(Common.Link.SrcLoc(range))
  }

  let link = React.useContext(eventContext)
  let className = Belt.List.fromArray(className)

  switch sanitizedTarget {
  | None => <span className={String.concat(" ", className)} > children </span>
  | Some(t) =>
    <span
      className={String.concat(" ", list{"component-link", ...className})}
      onClick={_ => {
        if jump {
          link->Chan.emit(Common.EventFromView.JumpToTarget(t))
        }
      }}
      onMouseOver={_ =>
        if hover {
          link->Chan.emit(Common.EventFromView.MouseOver(t))
        }}
      onMouseOut={_ =>
        if hover {
          link->Chan.emit(Common.EventFromView.MouseOut(t))
        }}>
      children
    </span>
  }
}
