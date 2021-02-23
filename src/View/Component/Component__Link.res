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
  ~target=Common.Link.ToLocation(NoLocation),
  ~jump=false,
  ~hover=false,
  ~className=list{},
  ~children,
) => {
  let sanitizedTarget = switch target {
  | ToLocation(NoLocation)
  | ToLocation(Location(_, [])) =>
    None
  | ToHole(index) => Some(Common.Link.ToHole(index))
  | ToLocation(location) => Some(Common.Link.ToLocation(location))
  }

  let link = React.useContext(eventContext)

  switch sanitizedTarget {
  | None =>
    <span className={String.concat(" ", list{"component-link", ...className})}> children </span>
  | Some(t) =>
    <span
      className={String.concat(" ", list{"component-link", ...className})}
      onClick={_ =>
        if jump {
          link->Chan.emit(Common.EventFromView.JumpToTarget(t))
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
