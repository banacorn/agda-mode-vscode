let emitter: Event.t(View.EventFromView.t) = Event.make();
let eventContext = React.createContext(emitter);

module Provider = {
  let makeProps = (~value, ~children, ()) => {
    "value": value,
    "children": children,
  };

  let make = React.Context.provider(eventContext);
};

[@react.component]
let make =
    (
      ~target=View.Link.ToRange(NoRange),
      ~jump=false,
      ~hover=false,
      ~className=[],
      ~children,
    ) => {
  let sanitizedTarget =
    switch (target) {
    | ToRange(NoRange)
    | ToRange(Range(_, [||])) => None
    | ToHole(index) => Some(View.Link.ToHole(index))
    | ToRange(range) => Some(View.Link.ToRange(range))
    };

  let link = React.useContext(eventContext);

  switch (sanitizedTarget) {
  | None =>
    <span className={String.concat(" ", ["link", ...className])}>
      children
    </span>
  | Some(t) =>
    <span
      className={String.concat(" ", ["link", ...className])}
      onClick={_ =>
        if (jump) {
          link.emit(View.EventFromView.JumpToTarget(t));
        }
      }
      onMouseOver={_ =>
        if (hover) {
          link.emit(View.EventFromView.MouseOver(t));
        }
      }
      onMouseOut={_ =>
        if (hover) {
          link.emit(View.EventFromView.MouseOut(t));
        }
      }>
      children
    </span>
  };
};
