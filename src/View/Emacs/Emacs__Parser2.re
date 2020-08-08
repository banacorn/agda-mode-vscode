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

let parseAllGoalsWarnings = (title, body): array(Item.t) => {
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
    ->partiteMetas
    ->partiteWarningsOrErrors("warnings")
    ->partiteWarningsOrErrors("errors");

  // convert entries in the dictionary to Items for render
  dictionary
  ->Js.Dict.entries
  ->Array.map(((key, lines)) => {
      switch (key) {
      | "warnings" =>
        lines->Array.map(line => Item.Warning(Text.parse(line)))
      | "errors" => lines->Array.map(line => Item.Error(Text.parse(line)))
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

let parseContextOrConstraints: string => array(Item.t) =
  raw => {
    let lines = Js.String.split("\n", raw)->Emacs__Parser.unindent;
    lines
    ->Array.map(Output.parse)
    ->Array.keepMap(x => x)
    ->Array.map(output => Item.Output(output));
  };

let parseOthers: string => array(Item.t) =
  raw => {
    [|Item.PlainText(Text.parse(raw))|];
  };

let parseSearchAbout: string => array(Item.t) =
  raw => {
    let lines = Js.String.split("\n", raw);
    let outputs =
      lines
      ->Js.Array.sliceFrom(1, _)
      ->Array.map(Js.String.sliceToEnd(~from=2))
      ->Emacs__Parser.unindent
      ->Array.map(Output.parse)
      ->Array.keepMap(x => x)
      ->Array.map(output => Item.Output(output));

    let target = lines[0]->Option.map(Js.String.sliceToEnd(~from=18));
    switch (target) {
    | None => [|
        Item.PlainText(Text.parse("Don't know what to search about")),
      |]
    | Some(target) =>
      if (Array.length(outputs) == 0) {
        [|
          Item.PlainText(
            Text.parse("There are no definitions about " ++ target),
          ),
        |];
      } else {
        [|
          [|
            Item.PlainText(
              Text.parse("Definitions about " ++ target ++ ":"),
            ),
          |],
          outputs,
        |]
        ->Array.concatMany;
      }
    };
  };
