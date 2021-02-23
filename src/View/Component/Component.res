open Belt
open React

// <Text> represents a mixed array of Strings & Locations
module Text = {
  module Segment = {
    type t =
      | PlainText(string, option<array<string>>)
      | Icon(string)
      | Link(string, option<array<string>>, bool, bool, View.Link.t)
      | Location(Common.Agda.Location.t, bool)

    let toString = x =>
      switch x {
      | PlainText(s, _) => s
      | Icon(s) => "[Icon " ++ s ++ "]"
      | Link(s, _, _, _, _) => s
      | Location(r, _) => Common.Agda.Location.toString(r)
      }
  }
  type t = Text(array<Segment.t>)
  let toSegments = x =>
    switch x {
    | Text(xs) => xs
    }
  let concatMany = xs => Text(xs->Array.map(toSegments)->Array.concatMany)
  // smart constructors
  let empty = Text([])
  let plainText = (~className=?, s) => Text([Segment.PlainText(s, className)])
  let link = (text, ~jump=true, ~hover=false, ~className=?, loc) => Text([
    Segment.Link(text, className, jump, hover, View.Link.ToLocation(loc)),
  ])
  let hole = (text, ~jump=true, ~hover=false, ~className=?, holeIndex) => Text([
    Segment.Link(text, className, jump, hover, View.Link.ToHole(holeIndex)),
  ])
  let location = (location, abbr) => Text([Segment.Location(location, abbr)])

  // serialize
  let toString = (Text(segments)) => segments->Array.map(Segment.toString)->Js.Array.joinWith("", _)
  // from string
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
        ->Option.mapWithDefault(Segment.PlainText(token, None), x => Segment.Location(x, false))
      | _ => PlainText(token, None)
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
        | PlainText(plainText, None) => string(plainText)
        | PlainText(plainText, Some(className)) =>
          <span key={string_of_int(i)} className={className->Array.joinWith(" ", x => x)}>
            {string(plainText)}
          </span>
        | Icon(kind) => <div className={"codicon codicon-" ++ kind} />
        | Link(text, None, jump, hover, target) =>
          <Component__Link key={string_of_int(i)} jump hover target>
            {string(text)}
          </Component__Link>
        | Link(text, Some(className), jump, hover, target) =>
          let className = className->List.fromArray
          <Component__Link key={string_of_int(i)} jump hover className target>
            {string(text)}
          </Component__Link>
        | Location(location, true) =>
          <Component__Link key={string_of_int(i)} jump=true target=View.Link.ToLocation(location)>
            <div className="codicon codicon-link" />
          </Component__Link>
        | Location(location, false) =>
          <Component__Link key={string_of_int(i)} jump=true target=View.Link.ToLocation(location)>
            <div className="codicon codicon-link" />
            {string(Common.Agda.Location.toString(location))}
          </Component__Link>
        }
      )
      ->React.array}
    </span>
  }
}

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
  let toText = x =>
    switch x {
    | Plain(string) => Text.plainText(~className=["expr"], string)
    | QuestionMark(i) =>
      Text.hole(
        "?" ++ string_of_int(i),
        ~className=["expr", "question-mark"],
        ~jump=true,
        ~hover=true,
        i,
      )
    | Underscore(string) => Text.plainText(~className=["expr underscore"], "_" ++ string)
    }

  // let jump = true
  // let hover = true

  // @react.component
  // let make = (~term: t) =>
  //   switch term {
  //   | Plain(s) => <span className="expr"> {string(s)} </span>
  //   | QuestionMark(i) =>
  //     <Link className=list{"expr", "question-mark"} jump hover target=View.Link.ToHole(i)>
  //       {string("?" ++ string_of_int(i))}
  //     </Link>
  //   | Underscore(s) => <span className="expr underscore"> {string(s)} </span>
  //   }
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

  let toText = xs => xs->Array.map(Term.toText)->Text.concatMany

  // @react.component
  // let make = (~expr: t) => {
  //   <span>
  //     {expr
  //     ->Array.mapWithIndex((i, term) => <Text key={string_of_int(i)} text={Term.toText(term)} />)
  //     ->React.array}
  //   </span>
  // }
}

module OutputConstraint = {
  type t =
    | OfType(Text.t, Text.t)
    | JustType(Text.t)
    | JustSort(Text.t)
    | Others(Text.t)

  let toString = x =>
    switch x {
    | OfType(e, t) => Text.toString(e) ++ (" : " ++ Text.toString(t))
    | JustType(t) => Text.toString(t)
    | JustSort(t) => Text.toString(t)
    | Others(t) => Text.toString(t)
    }

  let parseOfType =
    %re("/^([^\\:]*) \\: ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured
      ->Emacs__Parser.at(2, Expr.parse)
      ->Option.flatMap(type_ =>
        captured
        ->Emacs__Parser.at(1, Expr.parse)
        ->Option.flatMap(term => Some(OfType(Expr.toText(term), Expr.toText(type_))))
      )
    )
  let parseJustType =
    %re("/^Type ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(type_ => JustType(Expr.toText(type_)))
    )
  let parseJustSort =
    %re("/^Sort ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(sort => JustSort(Expr.toText(sort)))
    )
  let parseOthers = raw => raw->Expr.parse->Option.map(raw' => Others(Expr.toText(raw')))

  let parse = Emacs__Parser.choice([parseOfType, parseJustType, parseJustSort, parseOthers])

  @react.component
  let make = (~value: t, ~location: option<Common.Agda.Location.t>) => {
    let location = Option.mapWithDefault(location, null, location =>
      <Component__Link jump=true target=View.Link.ToLocation(location)>
        <div className="codicon codicon-link" />
      </Component__Link>
    )
    switch value {
    | OfType(e, t) =>
      <li className="unlabeled-item">
        <div className="item-content">
          <Text text=e /> {string(" : ")} <Text text=t /> location
        </div>
      </li>
    | JustType(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> {string("Type ")} <Text text=e /> location </div>
      </li>
    | JustSort(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> {string("Sort ")} <Text text=e /> location </div>
      </li>
    | Others(e) =>
      <li className="unlabeled-item">
        <div className="item-content"> <Text text=e /> location </div>
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

module Item = {
  type t =
    | PlainText(Text.t)
    | Error(Text.t)
    | Warning(Text.t)
    | Goal(Text.t)
    | Have(Text.t)
    | Output(Output.t)

  let toString = x =>
    switch x {
    | PlainText(text) => "Item [PlainText] " ++ Text.toString(text)
    | Error(text) => "Item [Error] " ++ Text.toString(text)
    | Warning(text) => "Item [Warning] " ++ Text.toString(text)
    | Goal(text) => "Item [Goal] " ++ Text.toString(text)
    | Have(text) => "Item [Have] " ++ Text.toString(text)
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
    | Goal(text) =>
      <li className="labeled-item special">
        <div className="item-label"> {string("Goal")} </div>
        <div className="item-content"> <Text text /> </div>
      </li>
    | Have(text) =>
      <li className="labeled-item special">
        <div className="item-label"> {string("Have")} </div>
        <div className="item-content"> <Text text /> </div>
      </li>
    | Output(value) => <Output value />
    }
}
