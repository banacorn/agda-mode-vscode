open React

@react.component
let make = (~location, ~abbr=false) =>
  if abbr {
    <Component__Link jump=true target=View.Link.ToLocation(location)>
      <div className="codicon codicon-link" />
    </Component__Link>
  } else {
    <Component__Link jump=true target=View.Link.ToLocation(location)>
      <div className="codicon codicon-link" /> {string(Common.Agda.Location.toString(location))}
    </Component__Link>
  }
