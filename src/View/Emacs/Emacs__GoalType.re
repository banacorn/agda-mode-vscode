open Belt;

open Component;
type t = {
  goal: option(Expr.t),
  have: option(Expr.t),
  interactionMetas: array(Output.t),
  hiddenMetas: array(Output.t),
};

let parse = raw => {
  let markGoal = ((line, _)) =>
    Js.String.match([%re "/^Goal:/"], line)->Option.map(_ => "goal");
  let markHave = ((line, _)) =>
    Js.String.match([%re "/^Have:/"], line)->Option.map(_ => "have");
  let markMetas = ((line, _)) =>
    Js.String.match([%re "/\\u2014{60}/g"], line)->Option.map(_ => "metas");
  let partiteGoalTypeContext = xs =>
    xs->Emacs__Parser.Dict.partite(line =>
      switch (markGoal(line)) {
      | Some(v) => Some(v)
      | None =>
        switch (markHave(line)) {
        | Some(v) => Some(v)
        | None =>
          switch (markMetas(line)) {
          | Some(v) => Some(v)
          | None => None
          }
        }
      }
    );
  let removeDelimeter = xs =>
    xs->Emacs__Parser.Dict.update("metas", Js.Array.sliceFrom(1));
  let lines = Js.String.split("\n", raw);
  let dictionary =
    lines
    ->partiteGoalTypeContext
    ->removeDelimeter
    ->Emacs__Parser2.partiteMetas;
  /* extract entries from the dictionary */
  let goal =
    Js.Dict.get(dictionary, "goal")
    ->Option.flatMap(line =>
        Js.Array.joinWith("\n", line)
        |> Js.String.sliceToEnd(~from=5)
        |> Expr.parse
      );
  let have =
    Js.Dict.get(dictionary, "have")
    ->Option.flatMap(line =>
        Js.Array.joinWith("\n", line)
        |> Js.String.sliceToEnd(~from=5)
        |> Expr.parse
      );
  let interactionMetas =
    Js.Dict.get(dictionary, "interactionMetas")
    ->Option.mapWithDefault([||], metas =>
        metas
        ->Array.map(Output.parseOutputWithoutRange)
        ->Array.keepMap(x => x)
      );
  let hiddenMetas =
    Js.Dict.get(dictionary, "hiddenMetas")
    ->Option.mapWithDefault([||], metas =>
        metas->Array.map(Output.parseOutputWithRange)->Array.keepMap(x => x)
      );
  {goal, have, interactionMetas, hiddenMetas};
};
open React;
[@react.component]
let make = (~payload: string) => {
  let parsed = parse(payload);
  <>
    <ul>
      {parsed.goal
       ->Option.mapWithDefault(null, expr => <Labeled label="Goal " expr />)}
      {parsed.have
       ->Option.mapWithDefault(null, expr => <Labeled label="Have " expr />)}
    </ul>
    {<ul>
       {parsed.interactionMetas
        ->Array.mapWithIndex((i, value) =>
            <Output key={string_of_int(i)} value />
          )
        ->React.array}
     </ul>}
    {<ul>
       {parsed.hiddenMetas
        ->Array.mapWithIndex((i, value) =>
            <Output key={string_of_int(i)} value />
          )
        ->React.array}
     </ul>}
  </>;
};
