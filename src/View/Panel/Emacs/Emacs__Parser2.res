// See src/full/Agda/Interaction/EmacsTop.hs for the logic behind this parser
open Emacs__Parser
open Belt

let partiteMetas = xs =>
  xs->Dict.split("metas", (rawMetas: array<string>) => {
    let metas = aggregateLines(rawMetas)
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
        // all metas are interaction metas
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
    // RegEx updated to v10.1.4
    let hasDelimeter = raw[0]->Option.flatMap(Js.String.match_(%re("/^\u2014{4}/")))->Option.isSome
    let lines = hasDelimeter ? Js.Array.sliceFrom(1, raw) : raw
    let markWarningStart = line => line->Common.AgdaRange.parse->Option.isSome
    /* If the previous warning of error ends with "at", then we have to glue it back */
    let glueBack = xs =>
      xs[Array.length(xs) - 1]->Option.flatMap(Js.String.match_(%re("/at$/")))->Option.isSome
    lines
    ->Array_.partite(markWarningStart)
    ->Array_.mergeWithNext(glueBack)
    ->Array.map(Util.String.unlines)
  })

let parseError: string => Js.Dict.t<array<string>> = raw => {
  // if the first line has delimeter,
  // then the message has both an error and possibly many warnings
  // all warnings start with a range
  //    ———— Error ————————————————————————————————————————————————————————————————
  //    ...
  //    ———— Warning(s) —————————————————————————————————————————————————————————————
  //    ...
  // else, the message has only an error
  //    ...
  let lines = Parser.splitToLines(raw)
  let hasBothErrorsAndWarnings =
    lines[0]->Option.flatMap(Js.String.match_(%re("/^\u2014{4} Error/")))->Option.isSome
  let markWarningStart = line => line->Common.AgdaRange.parse->Option.isSome
  // If the previous warning of error ends with "at", then we have to glue it back
  let glueBack = xs =>
    xs[Array.length(xs) - 1]->Option.flatMap(Js.String.match_(%re("/at$/")))->Option.isSome

  if hasBothErrorsAndWarnings {
    let isWarning = line => Js.String.match_(%re("/^\u2014{4} Warning\(s\)/"), line)->Option.isSome
    let predicate = ((line, i)) => {
      if i === 0 {
        Some("errors")
      } else if isWarning(line) {
        Some("warnings")
      } else {
        None
      }
    }
    lines
    ->Emacs__Parser.Dict.partite(predicate)
    // remove the delimeter in the first line of errors and unlines the rest
    ->Emacs__Parser.Dict.update("errors", xs => [xs->Js.Array2.sliceFrom(1)->Util.String.unlines])
    // remove the delimeter in the first line of warnings and unlines the rest
    ->Emacs__Parser.Dict.update("warnings", xs =>
      xs
      ->Js.Array2.sliceFrom(1)
      ->Array_.partite(markWarningStart)
      ->Array_.mergeWithNext(glueBack)
      ->Array.map(Util.String.unlines)
    )
  } else {
    lines
    ->Emacs__Parser.Dict.partite(((_, i)) => i === 0 ? Some("errors") : None)
    // unlines
    ->Emacs__Parser.Dict.update("errors", xs => [xs->Util.String.unlines])
  }
}

let parseGoalType: string => Js.Dict.t<array<string>> = raw => {
  let markGoal = ((line, _)) => Js.String.match_(%re("/^Goal:/"), line)->Option.map(_ => "goal")
  let markHave = ((line, _)) => Js.String.match_(%re("/^Have:/"), line)->Option.map(_ => "have")
  let markMetas = ((line, _)) =>
    Js.String.match_(%re("/\u2014{60}/g"), line)->Option.map(_ => "metas")
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
  let lines = Parser.splitToLines(raw)
  lines->partiteGoalTypeContext->removeDelimeter->partiteMetas
}

// convert entries in the dictionary to Items for rendering
let render: Js.Dict.t<array<string>> => array<Item.t> = dictionary => {
  // convert entries in the dictionary to Items for render
  dictionary
  ->Js.Dict.entries
  ->Array.map(((key, lines)) =>
    switch key {
    | "goal" =>
      lines
      ->Util.String.unlines
      ->Js.String.sliceToEnd(~from=5, _)
      ->Agda.Expr.parse
      ->Option.mapWithDefault([], expr => [
        Item.Labeled("Goal", "special", Agda.Expr.render(expr), None, None),
      ])
    | "have" =>
      lines
      ->Util.String.unlines
      ->Js.String.sliceToEnd(~from=5, _)
      ->Agda.Expr.parse
      ->Option.mapWithDefault([], expr => [
        Item.Labeled("Have", "special", Agda.Expr.render(expr), None, None),
      ])
    | "metas" =>
      lines
      ->Array.map(Agda.Output.parse)
      ->Array.keepMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
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
    | "warnings" => lines->Array.map(line => Item.warning(RichText.parse(line), None))
    | "errors" => lines->Array.map(line => Item.error(RichText.parse(line), None))
    | _ => []
    }
  )
  ->Js.Array.concatMany([])
}

let parseAllGoalsWarnings = (title, body): Js.Dict.t<array<string>> => {
  let partiteAllGoalsWarnings: (string, string) => Js.Dict.t<array<string>> = (title, body) => {
    let lines = Parser.splitToLines(body)
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

  partiteAllGoalsWarnings(title, body)
  ->partiteMetas
  ->partiteWarningsOrErrors("warnings")
  ->partiteWarningsOrErrors("errors")
}

let parseOutputs: string => array<Item.t> = raw => {
  let lines = Parser.splitToLines(raw)->Emacs__Parser.aggregateLines
  lines
  ->Array.map(Agda.Output.parse)
  ->Array.keepMap(x => x)
  ->Array.map(output => Agda.Output.renderItem(output))
}

let parseAndRenderTextWithLocation: string => array<Item.t> = raw => [
  Item.Unlabeled(RichText.parse(raw), None, None),
]

let parseAndRenderSearchAbout: string => array<Item.t> = raw => {
  let lines = Parser.splitToLines(raw)
  let outputs =
    lines
    ->Js.Array.sliceFrom(1, _)
    ->Array.map(Js.String.sliceToEnd(~from=2))
    ->Emacs__Parser.aggregateLines
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
