open React;

[@react.component]
let make = (~range, ~abbr=false) =>
  if (abbr) {
    <Component__Link jump=true target={View.Link.ToRange(range)}>
      <span className="codicon codicon-link" />
    </Component__Link>;
  } else {
    <Component__Link jump=true target={View.Link.ToRange(range)}>
      // <span className="text-subtle range icon icon-link">

        <span className="codicon codicon-link">
          {string(View.Range.toString(range))}
        </span>
      </Component__Link>;
  };
