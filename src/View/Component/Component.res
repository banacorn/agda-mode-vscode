open Belt
module Link = Component__Link
module Location = Component__Location
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
  let make = (~value: t, ~location: option<Common.Agda.Location.t>) => {
    let location = Option.mapWithDefault(location, null, location =>
      <Location location abbr=true />
    )
    switch value {
    | OfType(e, t) =>
      <li className="unlabeled-item">
        <div className="item-content">
          <Expr expr=e /> {string(" : ")} <Expr expr=t /> location
        </div>
      </li>
    | JustType(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> {string("Type ")} <Expr expr=e /> location </div>
      </li>
    | JustSort(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> {string("Sort ")} <Expr expr=e /> location </div>
      </li>
    | Others(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> <Expr expr=e /> location </div>
      </li>
    }
  }
}

module Output = {
  type t = Output(OutputConstraint.t, option<Common.Agda.Location.t>)
  let toString = x =>
    switch x {
    | Output(c, None) => "Output " ++ OutputConstraint.toString(c)
    | Output(c, Some(location)) =>
      "Output " ++
      (OutputConstraint.toString(c) ++
      (" " ++ Common.Agda.Location.toString(location)))
    }

  let parseOutputWithoutLocation = raw =>
    raw->OutputConstraint.parse->Option.map(x => Output(x, None))
  let parseOutputWithLocation = %re(
    "/((?:\\n|.)*\\S+)\\s*\\[ at ([^\\]]+) \\]/"
  )->Emacs__Parser.captures(captured =>
    captured[1]
    ->Option.flatMap(x => x)
    ->Option.flatMap(OutputConstraint.parse)
    ->Option.map(oc => {
      let r = captured[2]->Option.flatMap(x => x)->Option.flatMap(Common.Agda.Location.parse)
      Output(oc, r)
    })
  )
  let parse = raw => {
    let locRe = %re("/\\[ at (\\S+\\:(?:\\d+\\,\\d+\\-\\d+\\,\\d+|\\d+\\,\\d+\\-\\d+)) \\]$/")
    let hasLocation = Js.Re.test_(locRe, raw)
    if hasLocation {
      parseOutputWithLocation(raw)
    } else {
      parseOutputWithoutLocation(raw)
    }
  }

  @react.component
  let make = (~value: t) => {
    let Output(oc, location) = value
    <OutputConstraint value=oc location />
  }
}

// <Text> represents a mixed array of Strings & Locations
module Text = {
  module Segment = {
    type t =
      | PlainText(string)
      | Location(Common.Agda.Location.t)

    let toString = x =>
      switch x {
      | PlainText(s) => s
      | Location(r) => Common.Agda.Location.toString(r)
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
        ->Common.Agda.Location.parse
        ->Option.mapWithDefault(Segment.PlainText(token), x => Segment.Location(x))
      | _ => PlainText(token)
      }
    )
    ->(xs => Text(xs))

  @react.component
  let make = (~text: t) => {
    let Text(segments) = text
    <span>
      {segments
      ->Array.mapWithIndex((i, x) =>
        switch x {
        | PlainText(plainText) => string(plainText)
        | Location(location) => <Location key={string_of_int(i)} location />
        }
      )
      ->React.array}
    </span>
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
