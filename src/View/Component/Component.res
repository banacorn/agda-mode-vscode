open Belt
open React

// <Text> represents a mixed array of Strings & Locations
module Text = {
  module Segment = {
    type t =
      | PlainText(string, option<array<string>>)
      | Icon(string)
      | Link(string, option<array<string>>, bool, bool, Common.Link.t)
      | Location(Common.Agda.Location.t, bool)

    open! Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "PlainText" =>
        Contents(
          pair(string, optional(array(string))) |> map(((s, className)) => PlainText(s, className)),
        )
      | "Icon" => Contents(string |> map(kind => Icon(kind)))
      | "Link" =>
        Contents(
          tuple5(string, optional(array(string)), bool, bool, Common.Link.decode) |> map(((
            s,
            className,
            jump,
            hover,
            link,
          )) => Link(s, className, jump, hover, link)),
        )
      | "Location" =>
        Contents(
          pair(Common.Agda.Location.decode, bool) |> map(((loc, abbr)) => Location(loc, abbr)),
        )
      | tag => raise(DecodeError("[Component.Text] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | PlainText(s, className) =>
        object_(list{
          ("tag", string("PlainText")),
          ("contents", (s, className) |> pair(string, nullable(array(string)))),
        })
      | Icon(kind) => object_(list{("tag", string("Icon")), ("contents", kind |> string)})
      | Link(s, className, jump, hover, link) =>
        object_(list{
          ("tag", string("Link")),
          (
            "contents",
            (s, className, jump, hover, link) |> Util.Encode.tuple5(
              string,
              nullable(array(string)),
              bool,
              bool,
              Common.Link.encode,
            ),
          ),
        })
      | Location(loc, abbr) =>
        object_(list{
          ("tag", string("Location")),
          ("contents", (loc, abbr) |> pair(Common.Agda.Location.encode, bool)),
        })
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
    Segment.Link(text, className, jump, hover, Common.Link.ToLocation(loc)),
  ])
  let hole = (text, ~jump=true, ~hover=false, ~className=?, holeIndex) => Text([
    Segment.Link(text, className, jump, hover, Common.Link.ToHole(holeIndex)),
  ])
  let location = (location, abbr) => Text([Segment.Location(location, abbr)])
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
          <Component__Link key={string_of_int(i)} jump=true target=Common.Link.ToLocation(location)>
            <div className="codicon codicon-link" />
          </Component__Link>
        | Location(location, false) =>
          <Component__Link key={string_of_int(i)} jump=true target=Common.Link.ToLocation(location)>
            <div className="codicon codicon-link" />
            {string(Common.Agda.Location.toString(location))}
          </Component__Link>
        }
      )
      ->React.array}
    </span>
  }

  open! Json.Decode
  let decode: decoder<t> = array(Segment.decode) |> map(segments => Text(segments))
  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Text(segments) => segments |> array(Segment.encode)
    }
}

module Term = {
  type t =
    | Plain(string)
    | QuestionMark(int)
    | Underscore(string)
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
}

module Expr = {
  type t = array<Term.t>
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
}

module OutputConstraint = {
  type t =
    | OfType(Text.t, Text.t)
    | JustType(Text.t)
    | JustSort(Text.t)
    | Others(Text.t)

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

  let toText = (value, location) => {
    let location = location->Option.mapWithDefault(Text.empty, loc => Text.location(loc, true))
    switch value {
    | OfType(e, t) => Text.concatMany([e, Text.plainText(" : "), t, location])
    | JustType(e) => Text.concatMany([Text.plainText("Type "), e, location])
    | JustSort(e) => Text.concatMany([Text.plainText("Sort "), e, location])
    | Others(e) => Text.concatMany([e, location])
    }
  }
}

module Output = {
  type t = Output(OutputConstraint.t, option<Common.Agda.Location.t>)

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

  let toText = value => {
    let Output(oc, location) = value
    OutputConstraint.toText(oc, location)
  }
}

module Item = {
  type t =
    | Labeled(string, string, Text.t)
    | Unlabeled(Text.t)

  let plainText = s => Unlabeled(Text.plainText(s))

  @react.component
  let make = (~item: t) =>
    switch item {
    | Labeled(label, style, text) =>
      <li className={"labeled-item " ++ style}>
        <div className="item-label"> {string(label)} </div>
        <div className="item-content"> <Text text /> </div>
      </li>
    | Unlabeled(text) =>
      <li className="unlabeled-item"> <div className="item-content"> <Text text /> </div> </li>
    }

  open! Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Labeled" =>
      Contents(
        tuple3(string, string, Text.decode) |> map(((label, style, text)) => Labeled(
          label,
          style,
          text,
        )),
      )
    | "Unlabeled" => Contents(Text.decode |> map(text => Unlabeled(text)))
    | tag => raise(DecodeError("[Component.Item] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Labeled(label, style, text) =>
      object_(list{
        ("tag", string("Labeled")),
        ("contents", (label, style, text) |> tuple3(string, string, Text.encode)),
      })
    | Unlabeled(text) =>
      object_(list{("tag", string("Unlabeled")), ("contents", text |> Text.encode)})
    }
}
