open Belt
module Link = Component__Link
module Range = Component__Range
open React

module Term = {
  type t =
    | Plain(string)
    | QuestionMark(int)
    | Underscore(string)
  let toString = x =>
    switch x {
    | Plain(string) => string
    | QuestionMark(int) => "?" ++ string_of_int(int)
    | Underscore(string) => "_" ++ string
    }

  let jump = true
  let hover = true

  @react.component
  let make = (~term: t) =>
    switch term {
    | Plain(s) => <span className="expr"> {string(s)} </span>
    | QuestionMark(i) =>
      <Link className=list{"expr", "question-mark"} jump hover target=View.Link.ToHole(i)>
        {string("?" ++ string_of_int(i))}
      </Link>
    | Underscore(s) => <span className="expr underscore"> {string(s)} </span>
    }
}

module Expr = {
  type t = array<Term.t>
  let toString = xs => xs->Array.map(Term.toString)->Js.String.concatMany(" ")
  let parse = raw =>
    raw
    ->Js.String.trim
    /* 1         2 */
    ->Js.String.splitByRe(%re("/(\\?\\d+)|(\\_\\d+[^\\}\\)\\s]*)/"), _)
    ->Array.mapWithIndex((i, token) =>
      switch mod(i, 3) {
      | 1 =>
        token
        ->Option.map(Js.String.sliceToEnd(~from=1))
        ->Option.flatMap(int_of_string_opt)
        ->Option.map(x => Term.QuestionMark(x))
      | 2 => token->Option.map(x => Term.Underscore(x))
      | _ => token->Option.map(x => Term.Plain(x))
      }
    )
    ->Array.keepMap(x => x)
    ->(x => Some(x))
  @react.component
  let make = (~expr: t) =>
    <span>
      {expr->Array.mapWithIndex((i, term) => <Term key={string_of_int(i)} term />)->React.array}
    </span>
}

module OutputConstraint = {
  type t =
    | OfType(Expr.t, Expr.t)
    | JustType(Expr.t)
    | JustSort(Expr.t)
    | Others(Expr.t)

  let toString = x =>
    switch x {
    | OfType(e, t) => Expr.toString(e) ++ (" : " ++ Expr.toString(t))
    | JustType(t) => Expr.toString(t)
    | JustSort(t) => Expr.toString(t)
    | Others(t) => Expr.toString(t)
    }

  let parseOfType =
    %re("/^([^\\:]*) \\: ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured
      ->Emacs__Parser.at(2, Expr.parse)
      ->Option.flatMap(type_ =>
        captured->Emacs__Parser.at(1, Expr.parse)->Option.flatMap(term => Some(OfType(term, type_)))
      )
    )
  let parseJustType =
    %re("/^Type ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(type_ => JustType(type_))
    )
  let parseJustSort =
    %re("/^Sort ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(sort => JustSort(sort))
    )
  let parseOthers = raw => raw->Expr.parse->Option.map(raw' => Others(raw'))

  let parse = Emacs__Parser.choice([parseOfType, parseJustType, parseJustSort, parseOthers])

  @react.component
  let make = (~value: t, ~range: option<View.Range.t>) => {
    let range = Option.mapWithDefault(range, null, range => <Range range abbr=true />)
    switch value {
    | OfType(e, t) =>
      <li className="unlabeled-item">
        <div className="item-content"> <Expr expr=e /> {string(" : ")} <Expr expr=t /> range </div>
      </li>
    | JustType(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> {string("Type ")} <Expr expr=e /> range </div>
      </li>
    | JustSort(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> {string("Sort ")} <Expr expr=e /> range </div>
      </li>
    | Others(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> <Expr expr=e /> range </div>
      </li>
    }
  }
}

module Output = {
  type t = Output(OutputConstraint.t, option<View.Range.t>)
  let toString = x =>
    switch x {
    | Output(c, None) => "Output " ++ OutputConstraint.toString(c)
    | Output(c, Some(range)) =>
      "Output " ++ (OutputConstraint.toString(c) ++ (" " ++ View.Range.toString(range)))
    }

  let parseOutputWithoutRange = raw => raw->OutputConstraint.parse->Option.map(x => Output(x, None))
  let parseOutputWithRange =
    %re("/((?:\\n|.)*\\S+)\\s*\\[ at ([^\\]]+) \\]/")->Emacs__Parser.captures(captured =>
      captured[1]
      ->Option.flatMap(x => x)
      ->Option.flatMap(OutputConstraint.parse)
      ->Option.map(oc => {
        let r = captured[2]->Option.flatMap(x => x)->Option.flatMap(View.Range.parse)
        Output(oc, r)
      })
    )
  let parse = raw => {
    let rangeRe = %re("/\\[ at (\\S+\\:(?:\\d+\\,\\d+\\-\\d+\\,\\d+|\\d+\\,\\d+\\-\\d+)) \\]$/")
    let hasRange = Js.Re.test_(rangeRe, raw)
    if hasRange {
      parseOutputWithRange(raw)
    } else {
      parseOutputWithoutRange(raw)
    }
  }

  @react.component
  let make = (~value: t) => {
    let Output(oc, range) = value
    <OutputConstraint value=oc range />
  }
}

// <Text> represents a mixed array of Strings & Ranges
module Text = {
  module Segment = {
    type t =
      | PlainText(string)
      | Range(View.Range.t)

    let toString = x =>
      switch x {
      | PlainText(s) => s
      | Range(r) => View.Range.toString(r)
      }
  }
  type t = Text(array<Segment.t>)
  let toString = (Text(segments)) => segments->Array.map(Segment.toString)->Js.Array.joinWith("", _)

  let parse = raw =>
    raw
    ->Js.String.splitByRe(
      %re("/([^\\(\\)\\s]+\\:(?:\\d+\\,\\d+\\-\\d+\\,\\d+|\\d+\\,\\d+\\-\\d+))/"),
      _,
    )
    ->Array.keepMap(x => x)
    ->Array.mapWithIndex((i, token) =>
      switch mod(i, 2) {
      | 1 =>
        token
        ->View.Range.parse
        ->Option.mapWithDefault(Segment.PlainText(token), x => Segment.Range(x))
      | _ => PlainText(token)
      }
    )
    ->(xs => Text(xs))

  @react.component
  let make = (~text: t) => {
    let Text(segments) = text
    <span> {segments->Array.mapWithIndex((i, x) =>
        switch x {
        | PlainText(plainText) => string(plainText)
        | Range(range) => <Range key={string_of_int(i)} range />
        }
      )->React.array} </span>
  }
}

module Item = {
  type t =
    | PlainText(Text.t)
    | Error(Text.t)
    | Warning(Text.t)
    | Goal(Expr.t)
    | Have(Expr.t)
    | Output(Output.t)

  let toString = x =>
    switch x {
    | PlainText(text) => "Item [PlainText] " ++ Text.toString(text)
    | Error(text) => "Item [Error] " ++ Text.toString(text)
    | Warning(text) => "Item [Warning] " ++ Text.toString(text)
    | Goal(expr) => "Item [Goal] " ++ Expr.toString(expr)
    | Have(expr) => "Item [Have] " ++ Expr.toString(expr)
    | Output(output) => "Item [Output] " ++ Output.toString(output)
    }

  @react.component
  let make = (~item: t) =>
    switch item {
    | PlainText(text) =>
      <li className="labeled-item"> <div className="item-content"> <Text text /> </div> </li>
    | Error(text) =>
      <li className="labeled-item error">
        <div className="item-label"> {string("Error")} </div>
        <div className="item-content"> <Text text /> </div>
      </li>
    | Warning(text) =>
      <li className="labeled-item warning">
        <div className="item-label"> {string("Warning")} </div>
        <div className="item-content"> <Text text /> </div>
      </li>
    | Goal(expr) =>
      <li className="labeled-item special">
        <div className="item-label"> {string("Goal")} </div>
        <div className="item-content"> <Expr expr /> </div>
      </li>
    | Have(expr) =>
      <li className="labeled-item special">
        <div className="item-label"> {string("Have")} </div>
        <div className="item-content"> <Expr expr /> </div>
      </li>
    | Output(value) => <Output value />
    }
}
