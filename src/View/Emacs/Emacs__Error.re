open Belt;
open Component;

let parse: string => array(LabeledItem.t) =
  raw => {
    let lines = raw |> Js.String.split("\n");
    lines
    ->Emacs__Parser.Dict.partite(((_, i)) =>
        i === 0 ? Some("errors") : None
      )
    ->Emacs__Parser2.partiteWarningsOrErrors("errors")
    ->Js.Dict.get("errors")
    ->Option.mapWithDefault([||], entries =>
        entries->Array.map(entry => LabeledItem.Error(Text.parse(entry)))
      );
  };
[@react.component]
let make = (~payload: string) =>
  <ul>
    {parse(payload)
     ->Array.mapWithIndex((i, payload) =>
         <LabeledItem key={string_of_int(i)} payload />
       )
     ->React.array}
  </ul>;
