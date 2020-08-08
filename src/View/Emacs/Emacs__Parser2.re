open Emacs__Parser;
open Belt;
open Component;

let partiteMetas = xs =>
  xs->Dict.split("metas", (rawMetas: array(string)) => {
    let metas = unindent(rawMetas);
    let indexOfHiddenMetas =
      metas->Array.getIndexBy(s =>
        Component.Output.parseOutputWithRange(s)->Option.isSome
      );
    metas->Dict.partite(((_, i)) =>
      switch (indexOfHiddenMetas) {
      | Some(n) =>
        if (i === n) {
          Some("hiddenMetas");
        } else if (i === 0) {
          Some("interactionMetas");
        } else {
          None;
        }
      | None =>
        /* All interaction metas */
        if (i === 0) {
          Some("interactionMetas");
        } else {
          None;
        }
      }
    );
  });

let partiteWarningsOrErrors = (xs, key) =>
  xs->Dict.update(
    key,
    (raw: array(string)) => {
      let hasDelimeter =
        raw[0]
        ->Option.flatMap(Js.String.match([%re "/^\\u2014{4}/"]))
        ->Option.isSome;
      let lines = hasDelimeter ? raw |> Js.Array.sliceFrom(1) : raw;
      let markWarningStart = line => line->View.Range.parse->Option.isSome;
      /* If the previous warning of error ends with "at", then we have to glue it back */
      let glueBack = xs =>
        xs[Array.length(xs) - 1]
        ->Option.flatMap(Js.String.match([%re "/at$/"]))
        ->Option.isSome;
      lines
      ->Array_.partite(markWarningStart)
      ->Array_.mergeWithNext(glueBack)
      ->Array.map(Js.Array.joinWith("\n"));
    },
  );

let parseError: string => array(Component.LabeledItem.t) =
  raw => {
    let lines = raw |> Js.String.split("\n");
    lines
    ->Emacs__Parser.Dict.partite(((_, i)) =>
        i === 0 ? Some("errors") : None
      )
    ->partiteWarningsOrErrors("errors")
    ->Js.Dict.get("errors")
    ->Option.mapWithDefault([||], entries =>
        entries->Array.map(entry =>
          Component.LabeledItem.Error(Component.Text.parse(entry))
        )
      );
  };
