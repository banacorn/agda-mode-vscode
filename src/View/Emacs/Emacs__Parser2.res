open Emacs__Parser
open Belt
open Component

let partiteMetas = xs =>
  xs->Dict.split("metas", (rawMetas: array<string>) => {
    let metas = unindent(rawMetas)
    let indexOfHiddenMetas =
      metas->Array.getIndexBy(s => Agda.Output.parseOutputWithLocation(s)->Option.isSome)
    metas->Dict.partite(((_, i)) =>
      switch indexOfHiddenMetas {
      | Some(n) =>
        if i === n {
          Some("hiddenMetas")
        } else if i === 0 {
          Some("interactionMetas")
        } else {
          None
        }
      | None =>
        /* All interaction metas */
        if i === 0 {
          Some("interactionMetas")
        } else {
          None
        }
      }
    )
  })

let partiteWarningsOrErrors = (xs, key) =>
  xs->Dict.update(key, (raw: array<string>) => {
    let hasDelimeter = raw[0]->Option.flatMap(Js.String.match_(%re("/^\\u2014{4}/")))->Option.isSome
    let lines = hasDelimeter ? Js.Array.sliceFrom(1, raw) : raw
    let markWarningStart = line => line->Common.AgdaRange.parse->Option.isSome
    /* If the previous warning of error ends with "at", then we have to glue it back */
    let glueBack = xs =>
      xs[Array.length(xs) - 1]->Option.flatMap(Js.String.match_(%re("/at$/")))->Option.isSome
    lines
    ->Array_.partite(markWarningStart)
    ->Array_.mergeWithNext(glueBack)
    ->Array.map(Js.Array.joinWith("\n"))
  })

let parseError: string => array<Item.t> = raw => {
  let lines = Js.String.split("\n", raw)
  lines
  ->Emacs__Parser.Dict.partite(((_, i)) => i === 0 ? Some("errors") : None)
  ->partiteWarningsOrErrors("errors")
  ->Js.Dict.get("errors")
  ->Option.mapWithDefault([], entries =>
    entries->Array.map(entry => Item.error(RichText.parse(entry), None))
  )
}

let parseGoalType: string => array<Item.t> = raw => {
  let markGoal = ((line, _)) => Js.String.match_(%re("/^Goal:/"), line)->Option.map(_ => "goal")
  let markHave = ((line, _)) => Js.String.match_(%re("/^Have:/"), line)->Option.map(_ => "have")
  let markMetas = ((line, _)) =>
    Js.String.match_(%re("/\\u2014{60}/g"), line)->Option.map(_ => "metas")
  let partiteGoalTypeContext = xs =>
    xs->Emacs__Parser.Dict.partite(line =>
      switch markGoal(line) {
      | Some(v) => Some(v)
      | None =>
        switch markHave(line) {
        | Some(v) => Some(v)
        | None =>
          switch markMetas(line) {
          | Some(v) => Some(v)
          | None => None
          }
        }
      }
    )
  let removeDelimeter = xs => xs->Emacs__Parser.Dict.update("metas", Js.Array.sliceFrom(1))
  let lines = Js.String.split("\n", raw)
  let dictionary = lines->partiteGoalTypeContext->removeDelimeter->partiteMetas
  // convert entries in the dictionary to Items for render
  dictionary
  ->Js.Dict.entries
  ->Array.map(((key, lines)) =>
    switch key {
    | "goal" =>
      Js.Array.joinWith("\n", lines)
      ->Js.String.sliceToEnd(~from=5, _)
      ->Agda.Expr.parse
      ->Option.mapWithDefault([], expr => [
        Item.Labeled("Goal", "special", Agda.Expr.render(expr), None, None),
      ])
    | "have" =>
      Js.Array.joinWith("\n", lines)
      ->Js.String.sliceToEnd(~from=5, _)
      ->Agda.Expr.parse
      ->Option.mapWithDefault([], expr => [
        Item.Labeled("Have", "special", Agda.Expr.render(expr), None, None),
      ])
    | "interactionMetas" =>
      lines
      ->Array.map(Agda.Output.parseOutputWithoutLocation)
      ->Array.keepMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | "hiddenMetas" =>
      lines
      ->Array.map(Agda.Output.parseOutputWithLocation)
      ->Array.keepMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | _ => []
    }
  )
  ->Js.Array.concatMany([])
}

let parseAllGoalsWarnings = (title, body): array<Item.t> => {
  let partiteAllGoalsWarnings: (string, string) => Js.Dict.t<array<string>> = (title, body) => {
    let lines = Js.String.split("\n", body)
    /* examine the header to see what's in the body */
    let hasMetas = title->Js.String.match_(%re("/Goals/"), _)->Option.isSome
    let hasWarnings = title->Js.String.match_(%re("/Warnings/"), _)->Option.isSome
    let hasErrors = title->Js.String.match_(%re("/Errors/"), _)->Option.isSome
    /* predicates for partitioning the body */
    let markMetas = ((_, i)) => hasMetas && i === 0 ? Some("metas") : None
    let markWarnings = ((line, i)) =>
      hasWarnings
        ? switch hasMetas {
          /* Has both warnings and metas */
          | true =>
            line
            ->Js.String.slice(~from=5, ~to_=13, _)
            ->Js.String.match_(%re("/Warnings/"), _)
            ->Option.map(_ => "warnings")
          /* Has only warnings */
          | false => i === 0 ? Some("warnings") : None
          }
          /* Has no warnings */
        : None
    let markErrors = ((line, i)) =>
      hasErrors
      /* Has both warnings or metas and errors */
        ? switch hasMetas || hasWarnings {
          | true =>
            line
            ->Js.String.slice(~from=5, ~to_=11, _)
            ->Js.String.match_(%re("/Errors/"), _)
            ->Option.map(_ => "errors")
          /* Has only errors */
          | false => i === 0 ? Some("errors") : None
          }
        : None
    lines->Emacs__Parser.Dict.partite(line =>
      switch markMetas(line) {
      | Some(value) => Some(value)
      | None =>
        switch markWarnings(line) {
        | Some(value) => Some(value)
        | None =>
          switch markErrors(line) {
          | Some(value) => Some(value)
          | None => None
          }
        }
      }
    )
  }
  let dictionary: Js.Dict.t<array<string>> =
    partiteAllGoalsWarnings(title, body)
    ->partiteMetas
    ->partiteWarningsOrErrors("warnings")
    ->partiteWarningsOrErrors("errors")

  // convert entries in the dictionary to Items for rendering
  dictionary
  ->Js.Dict.entries
  ->Array.map(((key, lines)) =>
    switch key {
    | "warnings" => lines->Array.map(line => Item.warning(RichText.parse(line), None))
    | "errors" => lines->Array.map(line => Item.error(RichText.parse(line), None))
    | "interactionMetas" =>
      lines
      ->Array.map(Agda.Output.parseOutputWithoutLocation)
      ->Array.keepMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | "hiddenMetas" =>
      lines
      ->Array.map(Agda.Output.parseOutputWithLocation)
      ->Array.keepMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | _ => []
    }
  )
  ->Js.Array.concatMany([])
}

let parseOutputs: string => array<Item.t> = raw => {
  let lines = Js.String.split("\n", raw)->Emacs__Parser.unindent
  lines
  ->Array.map(Agda.Output.parse)
  ->Array.keepMap(x => x)
  ->Array.map(output => Agda.Output.renderItem(output))
}

let parseTextWithLocation: string => array<Item.t> = raw => [
  Item.Unlabeled(RichText.parse(raw), None, None),
]

let parseSearchAbout: string => array<Item.t> = raw => {
  let lines = Js.String.split("\n", raw)
  let outputs =
    lines
    ->Js.Array.sliceFrom(1, _)
    ->Array.map(Js.String.sliceToEnd(~from=2))
    ->Emacs__Parser.unindent
    ->Array.map(Agda.Output.parse)
    ->Array.keepMap(x => x)
    ->Array.map(output => Agda.Output.renderItem(output))

  let target = lines[0]->Option.map(Js.String.sliceToEnd(~from=18))
  switch target {
  | None => [Item.Unlabeled(RichText.parse("Don't know what to search about"), None, None)]
  | Some(target) =>
    if Array.length(outputs) == 0 {
      [Item.Unlabeled(RichText.parse("There are no definitions about " ++ target), None, None)]
    } else {
      [
        [Item.Unlabeled(RichText.parse("Definitions about " ++ (target ++ ":")), None, None)],
        outputs,
      ]->Array.concatMany
    }
  }
}
