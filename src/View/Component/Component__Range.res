open React

@react.component
let make = (~range, ~abbr=false) =>
  if abbr {
    <Component__Link jump=true target=View.Link.ToRange(range)>
      <div className="codicon codicon-link" />
    </Component__Link>
  } else {
    <Component__Link jump=true target=View.Link.ToRange(range)>
      <div className="codicon codicon-link" /> {string(View.Range.toString(range))}
    </Component__Link>
  }
