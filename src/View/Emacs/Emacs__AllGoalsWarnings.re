open Component;
open Belt;

type t = {
  title: string,
  interactionMetas: array(Output.t),
  hiddenMetas: array(Output.t),
  warnings: array(WarningError.t),
  errors: array(WarningError.t),
};

let toString = self =>
  Util.Pretty.(
    "Metas "
    ++ self.title
    ++ "\n"
    ++ array(Belt.Array.map(self.interactionMetas, Output.toString))
    ++ "\n"
    ++ array(Belt.Array.map(self.hiddenMetas, Output.toString))
    ++ "\n"
    ++ array(Belt.Array.map(self.warnings, WarningError.toString))
    ++ "\n"
    ++ array(Belt.Array.map(self.errors, WarningError.toString))
  );

let parse = (title, body): t => {
  let partiteAllGoalsWarnings: (string, string) => Js.Dict.t(array(string)) =
    (title, body) => {
      let lines = Js.String.split("\n", body);
      /* examine the header to see what's in the body */
      let hasMetas =
        title->Js.String.match([%re "/Goals/"], _)->Option.isSome;
      let hasWarnings =
        title->Js.String.match([%re "/Warnings/"], _)->Option.isSome;
      let hasErrors =
        title->Js.String.match([%re "/Errors/"], _)->Option.isSome;
      /* predicates for partitioning the body */
      let markMetas = ((_, i)) =>
        hasMetas && i === 0 ? Some("metas") : None;
      let markWarnings = ((line, i)) =>
        hasWarnings
          ? hasMetas
              /* Has both warnings and metas */
              ? line
                ->Js.String.slice(~from=5, ~to_=13, _)
                ->Js.String.match([%re "/Warnings/"], _)
                ->Option.map(_ => "warnings")
              /* Has only warnings */
              : i === 0 ? Some("warnings") : None
          /* Has no warnings */
          : None;
      let markErrors = ((line, i)) =>
        hasErrors
          /* Has both warnings or metas and errors */
          ? hasMetas || hasWarnings
              ? line
                ->Js.String.slice(~from=5, ~to_=11, _)
                ->Js.String.match([%re "/Errors/"], _)
                ->Option.map(_ => "errors")
              /* Has only errors */
              : i === 0 ? Some("errors") : None
          : None;
      lines->Emacs__Parser.Dict.partite(line =>
        switch (markMetas(line)) {
        | Some(value) => Some(value)
        | None =>
          switch (markWarnings(line)) {
          | Some(value) => Some(value)
          | None =>
            switch (markErrors(line)) {
            | Some(value) => Some(value)
            | None => None
            }
          }
        }
      );
    };
  let dictionary: Js.Dict.t(array(string)) =
    partiteAllGoalsWarnings(title, body)
    ->Emacs__Parser2.partiteMetas
    ->Emacs__Parser2.partiteWarningsOrErrors("warnings")
    ->Emacs__Parser2.partiteWarningsOrErrors("errors");
  /* extract entries from the dictionary */
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
  let warnings =
    Js.Dict.get(dictionary, "warnings")
    ->Option.mapWithDefault([||], entries =>
        entries->Array.map(WarningError.parseWarning)
      );
  let errors =
    Js.Dict.get(dictionary, "errors")
    ->Option.mapWithDefault([||], entries =>
        entries->Array.map(WarningError.parseError)
      );
  {title, interactionMetas, hiddenMetas, warnings, errors};
};

[@react.component]
let make = (~header: string, ~body: string) => {
  let {interactionMetas, hiddenMetas, warnings, errors} =
    parse(header, body);
  <>
    <ul>
      {interactionMetas
       ->Array.mapWithIndex((i, value) =>
           <Output key={string_of_int(i)} value />
         )
       ->React.array}
    </ul>
    <ul>
      {hiddenMetas
       ->Array.mapWithIndex((i, value) =>
           <Output key={string_of_int(i)} value />
         )
       ->React.array}
    </ul>
    <ul>
      {warnings
       ->Array.mapWithIndex((i, value) =>
           <WarningError key={string_of_int(i)} value />
         )
       ->React.array}
    </ul>
    <ul>
      {errors
       ->Array.mapWithIndex((i, value) =>
           <WarningError key={string_of_int(i)} value />
         )
       ->React.array}
    </ul>
  </>;
};
