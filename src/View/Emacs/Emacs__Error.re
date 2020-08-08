open Belt;

let parse: string => array(Component.WarningError.t) =
  raw => {
    let lines = raw |> Js.String.split("\n");
    lines
    ->Emacs__Parser.Dict.partite(((_, i)) =>
        i === 0 ? Some("errors") : None
      )
    ->Emacs__Parser2.partiteWarningsOrErrors("errors")
    ->Js.Dict.get("errors")
    ->Option.mapWithDefault([||], metas =>
        metas->Array.map(Component.WarningError.parseError)
      );
  };
[@react.component]
let make = (~payload: string) =>
  <ul>
    {parse(payload)
     ->Array.mapWithIndex((i, value) =>
         <Component.WarningError key={string_of_int(i)} value />
       )
     ->React.array}
  </ul>;
