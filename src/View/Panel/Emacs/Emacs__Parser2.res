// See src/full/Agda/Interaction/EmacsTop.hs for the logic behind this parser
open Emacs__Parser

let partiteMetas = xs =>
  xs->Dictionary.split("metas", (rawMetas: array<string>) => {
    let metas = aggregateLines(rawMetas)
    let indexOfHiddenMetas =
      metas->Belt.Array.getIndexBy(s => Agda.Output.parseOutputWithLocation(s)->Option.isSome)
    metas->Dictionary.partite(((_, i)) =>
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
  xs->Dictionary.update(key, (raw: array<string>) => {
    // RegEx updated to v10.1.4
    let hasDelimeter = raw[0]->Option.flatMap(String.match(_, %re("/^\u2014{4}/")))->Option.isSome
    let lines = hasDelimeter ? Array.sliceToEnd(~start=1, raw) : raw
    let markWarningStart = line => line->Common.AgdaRange.parse->Option.isSome
    /* If the previous warning of error ends with "at", then we have to glue it back */
    let glueBack = xs =>
      xs[Array.length(xs) - 1]->Option.flatMap(String.match(_, %re("/at$/")))->Option.isSome
    lines
    ->Array_.partite(markWarningStart)
    ->Array_.mergeWithNext(glueBack)
    ->Array.map(Util.String.unlines)
  })

let parseError: string => Dict.t<array<string>> = raw => {
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
    lines[0]->Option.flatMap(String.match(_, %re("/^\u2014{4} Error/")))->Option.isSome
  let markWarningStart = line => line->Common.AgdaRange.parse->Option.isSome
  // If the previous warning of error ends with "at", then we have to glue it back
  let glueBack = xs =>
    xs[Array.length(xs) - 1]->Option.flatMap(String.match(_, %re("/at$/")))->Option.isSome

  if hasBothErrorsAndWarnings {
    let isWarning = line => line->String.match(%re("/^\u2014{4} Warning\(s\)/"))->Option.isSome
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
    ->Emacs__Parser.Dictionary.partite(predicate)
    // remove the delimeter in the first line of errors and unlines the rest
    ->Emacs__Parser.Dictionary.update("errors", xs => [
      xs->Array.sliceToEnd(~start=1)->Util.String.unlines,
    ])
    // remove the delimeter in the first line of warnings and unlines the rest
    ->Emacs__Parser.Dictionary.update("warnings", xs =>
      xs
      ->Array.sliceToEnd(~start=1)
      ->Array_.partite(markWarningStart)
      ->Array_.mergeWithNext(glueBack)
      ->Array.map(Util.String.unlines)
    )
  } else {
    lines
    ->Emacs__Parser.Dictionary.partite(((_, i)) => i === 0 ? Some("errors") : None)
    // unlines
    ->Emacs__Parser.Dictionary.update("errors", xs => [xs->Util.String.unlines])
  }
}

let parseGoalType: string => Dict.t<array<string>> = raw => {
  let markGoal = ((line, _)) => line->String.match(%re("/^Goal:/"))->Option.map(_ => "goal")
  let markHave = ((line, _)) => line->String.match(%re("/^Have:/"))->Option.map(_ => "have")
  let markMetas = ((line, _)) => line->String.match(%re("/\u2014{60}/g"))->Option.map(_ => "metas")
  let partiteGoalTypeContext = xs =>
    xs->Emacs__Parser.Dictionary.partite(line =>
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
  let removeDelimeter = xs =>
    xs->Emacs__Parser.Dictionary.update("metas", Array.sliceToEnd(_, ~start=1))
  let lines = Parser.splitToLines(raw)
  lines->partiteGoalTypeContext->removeDelimeter->partiteMetas
}

// convert entries in the dictionary to Items for rendering
let render: Dict.t<array<string>> => array<Item.t> = dictionary => {
  // convert entries in the dictionary to Items for render
  dictionary
  ->Dict.toArray
  ->Array.map(((key, lines)) =>
    switch key {
    | "goal" =>
      lines
      ->Util.String.unlines
      ->String.sliceToEnd(~start=5)
      ->Agda.Expr.parse
      ->Option.mapOr([], expr => [
        Item.Labeled("Goal", "special", Agda.Expr.render(expr), None, None),
      ])
    | "have" =>
      lines
      ->Util.String.unlines
      ->String.sliceToEnd(~start=5)
      ->Agda.Expr.parse
      ->Option.mapOr([], expr => [
        Item.Labeled("Have", "special", Agda.Expr.render(expr), None, None),
      ])
    | "metas" =>
      lines
      ->Array.map(Agda.Output.parse)
      ->Array.filterMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | "interactionMetas" =>
      lines
      ->Array.map(Agda.Output.parseOutputWithoutLocation)
      ->Array.filterMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | "hiddenMetas" =>
      lines
      ->Array.map(Agda.Output.parseOutputWithLocation)
      ->Array.filterMap(x => x)
      ->Array.map(output => Agda.Output.renderItem(output))
    | "warnings" => lines->Array.map(line => Item.warning(RichText.parse(line), None))
    | "errors" => lines->Array.map(line => Item.error(RichText.parse(line), None))
    | _ => []
    }
  )
  ->(Array.concatMany([], _))
}

let parseAllGoalsWarnings = (title, body): Dict.t<array<string>> => {
  let partiteAllGoalsWarnings: (string, string) => Dict.t<array<string>> = (title, body) => {
    let lines = Parser.splitToLines(body)
    /* examine the header to see what's in the body */
    let hasMetas = title->String.match(%re("/Goals/"))->Option.isSome
    let hasWarnings = title->String.match(%re("/Warnings/"))->Option.isSome
    let hasErrors = title->String.match(%re("/Errors/"))->Option.isSome
    /* predicates for partitioning the body */
    let markMetas = ((_, i)) => hasMetas && i === 0 ? Some("metas") : None
    let markWarnings = ((line, i)) =>
      hasWarnings
        ? switch hasMetas {
          /* Has both warnings and metas */
          | true =>
            line
            ->String.slice(~start=5, ~end=13)
            ->String.match(%re("/Warnings/"))
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
            ->String.slice(~start=5, ~end=11)
            ->String.match(%re("/Errors/"))
            ->Option.map(_ => "errors")
          /* Has only errors */
          | false => i === 0 ? Some("errors") : None
          }
        : None
    lines->Emacs__Parser.Dictionary.partite(line =>
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
  ->Array.filterMap(x => x)
  ->Array.map(output => Agda.Output.renderItem(output))
}

let parseAndRenderTextWithLocation: string => array<Item.t> = raw => [
  Item.Unlabeled(RichText.parse(raw), None, None),
]

let parseAndRenderSearchAbout: string => array<Item.t> = raw => {
  let lines = Parser.splitToLines(raw)
  let outputs =
    lines
    ->Array.sliceToEnd(~start=1)
    ->Array.map(String.sliceToEnd(_, ~start=2))
    ->Emacs__Parser.aggregateLines
    ->Array.map(Agda.Output.parse)
    ->Array.filterMap(x => x)
    ->Array.map(output => Agda.Output.renderItem(output))

  let target = lines[0]->Option.map(String.sliceToEnd(_, ~start=18))
  switch target {
  | None => [Item.Unlabeled(RichText.parse("Don't know what to search about"), None, None)]
  | Some(target) =>
    if Array.length(outputs) == 0 {
      [Item.Unlabeled(RichText.parse("There are no definitions about " ++ target), None, None)]
    } else {
      [
        [Item.Unlabeled(RichText.parse("Definitions about " ++ (target ++ ":")), None, None)],
        outputs,
      ]->Array.flat
    }
  }
}
