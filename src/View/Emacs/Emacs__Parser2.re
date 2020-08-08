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

let parseError: string => array(Item.t) =
  raw => {
    let lines = raw |> Js.String.split("\n");
    lines
    ->Emacs__Parser.Dict.partite(((_, i)) =>
        i === 0 ? Some("errors") : None
      )
    ->partiteWarningsOrErrors("errors")
    ->Js.Dict.get("errors")
    ->Option.mapWithDefault([||], entries =>
        entries->Array.map(entry => Item.Error(Text.parse(entry)))
      );
  };

let parseGoalType: string => array(Item.t) =
  raw => {
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
      lines->partiteGoalTypeContext->removeDelimeter->partiteMetas;
    // convert entries in the dictionary to Items for render
    dictionary
    ->Js.Dict.entries
    ->Array.map(((key, lines)) => {
        switch (key) {
        | "goal" =>
          Js.Array.joinWith("\n", lines)
          ->Js.String.sliceToEnd(~from=5, _)
          ->Expr.parse
          ->Option.mapWithDefault([||], expr => [|Item.Goal(expr)|])
        | "have" =>
          Js.Array.joinWith("\n", lines)
          ->Js.String.sliceToEnd(~from=5, _)
          ->Expr.parse
          ->Option.mapWithDefault([||], expr => [|Item.Have(expr)|])
        | "interactionMetas" =>
          lines
          ->Array.map(Output.parseOutputWithoutRange)
          ->Array.keepMap(x => x)
          ->Array.map(output => Item.Output(output))
        | "hiddenMetas" =>
          lines
          ->Array.map(Output.parseOutputWithRange)
          ->Array.keepMap(x => x)
          ->Array.map(output => Item.Output(output))
        | _ => [||]
        }
      })
    ->Js.Array.concatMany([||]);
  };
