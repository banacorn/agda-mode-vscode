// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

open Component
open Belt

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

module NamedMeta = {
  type t = NamedMeta(string, int)

  let toText = value => {
    switch value {
    | NamedMeta("", int)
    | NamedMeta("_", int) =>
      Text.plainText(string_of_int(int))
    | NamedMeta(string, int) => Text.plainText("_" ++ string ++ string_of_int(int))
    }
  }

  let render = value => {
    switch value {
    | NamedMeta("", int)
    | NamedMeta("_", int) =>
      RichText.text(string_of_int(int))
    | NamedMeta(string, int) => RichText.text("_" ++ string ++ string_of_int(int))
    }
  }

  open Json.Decode
  let decode: decoder<t> = pair(string, int) |> map(((name, id)) => NamedMeta(name, id))
}

// InteractionId
module InteractionId = {
  type t = InteractionId(int)

  let toText = value => {
    switch value {
    | InteractionId(index) => Text.hole("?" ++ string_of_int(index), index)
    }
  }

  let render = value => {
    switch value {
    | InteractionId(index) => RichText.hole("?" ++ string_of_int(index), index)
    }
  }

  open Json.Decode
  let decode: decoder<t> = int |> map(id => InteractionId(id))
}

// Polarity
module Polarity = {
  type t =
    | Covariant
    | Contravariant
    | Invariant
    | Nonvariant

  let toText = value => {
    switch value {
    | Covariant => Text.plainText(" + ")
    | Contravariant => Text.plainText(" - ")
    | Invariant => Text.plainText(" * ")
    | Nonvariant => Text.plainText(" _ ")
    }
  }
  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "Covariant" => TagOnly(Covariant)
    | "Contravariant" => TagOnly(Contravariant)
    | "Invariant" => TagOnly(Invariant)
    | "Nonvariant" => TagOnly(Nonvariant)
    | tag => raise(DecodeError("[Agda.Polarity] Unknown constructor: " ++ tag))
    }
  )
}

module Comparison = {
  type t = CmpEq | CmpLeq

  let toText = value => {
    switch value {
    | CmpEq => Text.plainText(" = ")
    | CmpLeq => Text.plainText(" =< ")
    }
  }
  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "CmpEq" => TagOnly(CmpEq)
    | "CmpLeq" => TagOnly(CmpLeq)
    | tag => raise(DecodeError("[Agda.Comparison] Unknown constructor: " ++ tag))
    }
  )
}

module OutputConstraint: {
  type t<'b>
  let parse: string => option<t<InteractionId.t>>
  let toText: ('b => Text.t, 'b => RichText.t, t<'b>, option<Common.AgdaRange.t>) => Text.t
  let decode: Json.Decode.decoder<'b> => Json.Decode.decoder<t<'b>>
} = {
  // CmpEq: true / CmpLeq: false
  type rec t<'b> =
    | OfType('b, RichText.t)
    | JustType('b)
    | JustSort('b)
    | CmpInType(Comparison.t, string, 'b, 'b)
    | CmpElim(array<Polarity.t>, string, array<'b>, array<'b>)
    | CmpTypes(Comparison.t, 'b, 'b)
    | CmpLevels(Comparison.t, 'b, 'b)
    | CmpTeles(Comparison.t, 'b, 'b)
    | CmpSorts(Comparison.t, 'b, 'b)
    | Guard(t<'b>, int)
    | Assign('b, string)
    | TypedAssign('b, string, string)
    | PostponedCheckArgs('b, array<string>, string, string)
    | IsEmptyType(string)
    | SizeLtSat(string)
    | FindInstanceOF('b, string, array<(string, string)>)
    | PTSInstance('b, 'b)
    | PostponedCheckFunDef(string, string)
    // NOTE: legacy constructors
    | OfType'(Text.t, Text.t)
    | JustType'(Text.t)
    | JustSort'(Text.t)
    | Others'(Text.t)

  let parseOfType =
    %re("/^([^\\:]*) \\: ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured
      ->Emacs__Parser.at(2, Expr.parse)
      ->Option.flatMap(type_ =>
        captured
        ->Emacs__Parser.at(1, Expr.parse)
        ->Option.flatMap(term => Some(OfType'(Expr.toText(term), Expr.toText(type_))))
      )
    )
  let parseJustType =
    %re("/^Type ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(type_ => JustType'(Expr.toText(type_)))
    )
  let parseJustSort =
    %re("/^Sort ((?:\\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(sort => JustSort'(Expr.toText(sort)))
    )
  let parseOthers = raw => raw->Expr.parse->Option.map(raw' => Others'(Expr.toText(raw')))

  let parse = Emacs__Parser.choice([parseOfType, parseJustType, parseJustSort, parseOthers])

  let rec toText = (idToText, renderId, value, location) => {
    let location = location->Option.mapWithDefault(Text.empty, loc => Text.location(loc, true))

    let cmpToText = (cmp, a, b) =>
      Text.concatMany([idToText(a), Comparison.toText(cmp), idToText(b)])
    switch value {
    | OfType(name, expr) =>
      RichText.concatMany([renderId(name), RichText.text(" : "), expr])->Text.fromRichText
    | JustType(name) => Text.concatMany([Text.plainText("Type "), idToText(name)])
    | JustSort(name) => Text.concatMany([Text.plainText("Sort "), idToText(name)])
    | CmpInType(cmp, expr, name1, name2) =>
      Text.concatMany([cmpToText(cmp, name1, name2), Text.plainText(" : "), Text.plainText(expr)])
    | CmpElim(polarities, expr, names1, names2) =>
      let polarities = polarities->Array.map(Polarity.toText)->Text.concatMany
      let names1 = names1->Array.map(idToText)->Text.concatMany
      let names2 = names2->Array.map(idToText)->Text.concatMany
      Text.concatMany([names1, polarities, names2, Text.plainText(" : "), Text.plainText(expr)])
    | CmpTypes(cmp, name1, name2) => cmpToText(cmp, name1, name2)
    | CmpLevels(cmp, name1, name2) => cmpToText(cmp, name1, name2)
    | CmpTeles(cmp, name1, name2) => cmpToText(cmp, name1, name2)
    | CmpSorts(cmp, name1, name2) => cmpToText(cmp, name1, name2)
    | Guard(self, pid) =>
      Text.concatMany([
        toText(idToText, renderId, self, None),
        Text.plainText("(blocked by problem "),
        Text.plainText(string_of_int(pid)),
        Text.plainText(")"),
      ])
    | Assign(name, expr) =>
      Text.concatMany([idToText(name), Text.plainText(" := "), Text.plainText(expr)])
    | TypedAssign(name, expr1, expr2) =>
      Text.concatMany([
        idToText(name),
        Text.plainText(" := "),
        Text.plainText(expr1),
        Text.plainText(" :? "),
        Text.plainText(expr2),
      ])
    | PostponedCheckArgs(name, exprs, t0, t1) =>
      let t0 = Text.concatMany([
        Text.plainText("(_"),
        Text.plainText(" : "),
        Text.plainText(t0),
        Text.plainText(")"),
      ])
      let exprs = exprs->Array.map(expr => Text.plainText(" (" ++ expr ++ ")"))->Text.concatMany
      Text.concatMany([
        idToText(name),
        Text.plainText(" := "),
        t0,
        exprs,
        Text.plainText(" : "),
        Text.plainText(t1),
      ])
    | IsEmptyType(expr) => Text.concatMany([Text.plainText("Is empty: "), Text.plainText(expr)])
    | SizeLtSat(expr) =>
      Text.concatMany([Text.plainText("Not empty type of sizes: "), Text.plainText(expr)])
    | FindInstanceOF(name, expr, pairs) =>
      let line1 = Text.concatMany([
        Text.plainText("Resolve instance argument "),
        idToText(name),
        Text.plainText(" : "),
        Text.plainText(expr),
      ])
      let pairs =
        pairs
        ->Array.map(((s, t)) =>
          Text.concatMany([
            Text.plainText(s),
            Text.plainText(" : "),
            Text.plainText(t),
            Text.plainText(" \n"),
          ])
        )
        ->Text.concatMany
      let line2 = Text.concatMany([Text.plainText(" \nCandidate: "), pairs])
      Text.concatMany([line1, line2])
    | PTSInstance(a, b) =>
      Text.concatMany([
        Text.plainText("PTS instance for ("),
        idToText(a),
        Text.plainText(" , "),
        idToText(b),
        Text.plainText(")"),
      ])
    | PostponedCheckFunDef(a, b) => Text.plainText("Check definition of " ++ a ++ " : " ++ b)
    | OfType'(e, t) => Text.concatMany([e, Text.plainText(" : "), t, location])
    | JustType'(e) => Text.concatMany([Text.plainText("Type "), e, location])
    | JustSort'(e) => Text.concatMany([Text.plainText("Sort "), e, location])
    | Others'(e) => Text.concatMany([e, location])
    }
  }

  open Json.Decode
  open Util.Decode
  // extra gymnastics to bypass OCaml's limitations on recursion
  let rec decode': decoder<'b> => decoder<t<'b>> = decodeId => decode(decodeId)
  and decode: decoder<'b> => decoder<t<'b>> = decodeID =>
    sum(x =>
      switch x {
      | "OfType" =>
        Contents(pair(decodeID, RichText.decode) |> map(((name, expr)) => OfType(name, expr)))
      | "JustType" => Contents(decodeID |> map(name => JustType(name)))
      | "JustSort" => Contents(decodeID |> map(name => JustSort(name)))
      | "CmpInType" =>
        Contents(
          tuple4(Comparison.decode, string, decodeID, decodeID) |> map(((
            cmp,
            expr,
            name1,
            name2,
          )) => CmpInType(cmp, expr, name1, name2)),
        )
      | "CmpElim" =>
        Contents(
          tuple4(array(Polarity.decode), string, array(decodeID), array(decodeID)) |> map(((
            polarities,
            expr,
            names1,
            names2,
          )) => CmpElim(polarities, expr, names1, names2)),
        )
      | "CmpTypes" =>
        Contents(
          tuple3(Comparison.decode, decodeID, decodeID) |> map(((cmp, name1, name2)) => CmpTypes(
            cmp,
            name1,
            name2,
          )),
        )
      | "CmpLevels" =>
        Contents(
          tuple3(Comparison.decode, decodeID, decodeID) |> map(((cmp, name1, name2)) => CmpLevels(
            cmp,
            name1,
            name2,
          )),
        )
      | "CmpTeles" =>
        Contents(
          tuple3(Comparison.decode, decodeID, decodeID) |> map(((cmp, name1, name2)) => CmpTeles(
            cmp,
            name1,
            name2,
          )),
        )
      | "CmpSorts" =>
        Contents(
          tuple3(Comparison.decode, decodeID, decodeID) |> map(((cmp, name1, name2)) => CmpSorts(
            cmp,
            name1,
            name2,
          )),
        )
      | "Guard" => Contents(pair(decode'(decodeID), int) |> map(((oc, pid)) => Guard(oc, pid)))
      | "Assign" => Contents(pair(decodeID, string) |> map(((name, expr)) => Assign(name, expr)))
      | "TypedAssign" =>
        Contents(
          tuple3(decodeID, string, string) |> map(((name, expr1, expr2)) => TypedAssign(
            name,
            expr1,
            expr2,
          )),
        )
      | "PostponedCheckArgs" =>
        Contents(
          tuple4(decodeID, array(string), string, string) |> map(((
            name,
            exprs,
            expr1,
            expr2,
          )) => PostponedCheckArgs(name, exprs, expr1, expr2)),
        )
      | "IsEmptyType" => Contents(string |> map(expr => IsEmptyType(expr)))
      | "SizeLtSat" => Contents(string |> map(expr => SizeLtSat(expr)))
      | "FindInstanceOF" =>
        Contents(
          tuple3(decodeID, string, array(pair(string, string))) |> map(((
            name,
            expr,
            pairs,
          )) => FindInstanceOF(name, expr, pairs)),
        )
      | "PTSInstance" =>
        Contents(pair(decodeID, decodeID) |> map(((name1, name2)) => PTSInstance(name1, name2)))
      | "PostponedCheckFunDef" =>
        Contents(pair(string, string) |> map(((a, b)) => PostponedCheckFunDef(a, b)))
      | tag => raise(DecodeError("[Agda.OutputConstraint] Unknown constructor: " ++ tag))
      }
    )
}

module Output = {
  type t<'b> = Output(OutputConstraint.t<'b>, option<Common.AgdaRange.t>)

  // parsing serialized data
  let parseOutputWithoutLocation = raw =>
    raw->OutputConstraint.parse->Option.map(x => Output(x, None))
  let parseOutputWithLocation = %re(
    "/((?:\\n|.)*\\S+)\\s*\\[ at ([^\\]]+) \\]/"
  )->Emacs__Parser.captures(captured =>
    captured[1]
    ->Option.flatMap(x => x)
    ->Option.flatMap(OutputConstraint.parse)
    ->Option.map(oc => {
      let r = captured[2]->Option.flatMap(x => x)->Option.flatMap(Common.AgdaRange.parse)
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

  let toText = (idToText, renderId, value) => {
    let Output(oc, location) = value
    OutputConstraint.toText(idToText, renderId, oc, location)
  }
}

module type Indices = {
  type t
  let make: array<int> => t
  let convert: (t, int) => int
  // expose the intervals for testing
  let expose: t => (array<(int, int)>, int)
}

module Indices: Indices = {
  //    Problem:  Symbols like "𝕁" should be treated like a single character as in UTF-8,
  //              however, it's treated like 2 characters in UTF-16 (which is what VS Code uses)
  type t = {
    intervals: array<(int, int)>,
    lastInterval: int,
    mutable cursor: int,
  }

  // compiles an array of UTF-8 based offset intervals
  // for faster UTF-8 => UTF-16 convertion
  let make = (indicesUTF16: array<int>): t => {
    //  Suppose that, there are surrogate pairs at [6000, 6001] and [6003, 6004]

    //        UTF-8       UTF-16
    //        --------------------
    //        5999        5999
    //        6000        6000           <
    //                    6001
    //        6001        6002
    //        6002        6003           <
    //                    6004
    //        6003        6005

    //  When converting from a UTF-8 based index, say, `6003`,
    //  we need to know how many surrogate pairs are there before `6003`

    //  Here's what we have computed:
    //    * UTF-16 based indices of surrogate pairs: [6000, 6003]

    //  Here's what we are going to compute next:
    //    * UTF-8 based indices of surrogate pairs: [6000, 6002]
    //    * intervals of UTF-8 based indices [(0, 6000), (6001, 6002)]

    //  NOTE: the last interval (6003, ...) will not be included

    //  indicesUTF16 = [6000, 6003]

    // [6000, 6002]
    let indicesUTF8 = indicesUTF16->Array.mapWithIndex((i, x) => x - i)

    // [(0, 6000), (6001, 6002)]
    let intervals = indicesUTF8->Array.mapWithIndex((i, rightEndpoint) => {
      let leftEndpoint = switch indicesUTF8[i - 1] {
      | Some(x) => x + 1
      // first interval
      | None => 0
      }
      (leftEndpoint, rightEndpoint)
    })

    // 6003
    let lastInterval =
      intervals[Array.length(intervals) - 1]->Option.mapWithDefault(0, ((_, x)) => x + 1)

    {
      intervals: intervals,
      lastInterval: lastInterval,
      cursor: 0,
    }
  }

  let rec convert = (self, index) => {
    switch self.intervals[self.cursor] {
    | None =>
      // happens when we enter the last inverval
      if index >= self.lastInterval {
        // return index + how many pairs it have skipped
        index + self.cursor
      } else {
        // reset the cursor to the beginning of the intervals
        self.cursor = 0
        convert(self, index)
      }
    | Some((left, right)) =>
      if index < left {
        // reset the cursor to the beginning of the intervals
        self.cursor = 0
        convert(self, index)
      } else if index > right {
        // move the cursor a tad right
        self.cursor = self.cursor + 1
        convert(self, index)
      } else {
        // index + how many pairs it have skipped
        index + self.cursor
      }
    }
  }

  let expose = self => (self.intervals, self.cursor)
}

module type OffsetConverter = {
  type t
  let make: string => t
  let convert: (t, int) => int

  // for testing
  let characterWidth: string => int
  let computeUTF16SurrogatePairIndices: string => array<int>
  let computeCRLFIndices: string => array<int>
}

module OffsetConverter: OffsetConverter = {
  let characterWidth: string => int = %raw("function (string) {return [...string].length}")

  // returns an array of UTF-16 based indices where surrogate pairs occur,
  // for example, suppose that there are surrogate pairs at [6000, 6001] and [6003, 6004]

  //        UTF-8       UTF-16
  //        --------------------
  //        5999        5999
  //        6000        6000
  //                    6001
  //        6001        6002
  //        6002        6003
  //                    6004
  //        6003        6005

  // then this function should return [6000, 6003]
  let computeUTF16SurrogatePairIndices = (text: string): array<int> => {
    let surrogatePairs = []

    // length in code units (16 bits), not the actual UTF-8 length
    let lengthInCodeUnits = String.length(text)

    // iterate through the text to find surrogate pairs
    let i = ref(0)
    while i.contents < lengthInCodeUnits {
      let charCode = Js.String.charCodeAt(i.contents, text)->int_of_float
      let notFinal = i.contents + 1 < lengthInCodeUnits
      // check if this is a part of a surrogate pair
      if charCode >= 0xD800 && (charCode <= 0xDBFF && notFinal) {
        // found the high surrogate, proceed to check the low surrogate
        let nextCharCode = Js.String.charCodeAt(i.contents + 1, text)->int_of_float
        if nextCharCode >= 0xDC00 && charCode <= 0xDFFF {
          // store the index of this surrogate pair
          Js.Array.push(i.contents, surrogatePairs)->ignore
        }
        // increment by 2 because we have checked the presumably low surrogate char
        i := i.contents + 2
      } else {
        i := i.contents + 1
      }
    }

    surrogatePairs
  }

  // Issue #3: https://github.com/banacorn/agda-mode-vscode/issues/3
  // Agda ignores `CRLF`s (line endings on Windows) and treat them like `LF`s
  //
  // returns an array of indices where CRLF line endings occur
  let computeCRLFIndices = (text: string): array<int> => {
    let regexp = %re("/\\r\\n/g")
    let matchAll = %raw("function (regexp, string) {
          let match;
          let result = [];
          while ((match = regexp.exec(string)) !== null) {
            result.push(match.index);
          }
          return result;
        }
      ")
    matchAll(regexp, text)
  }

  type t = {
    // cached lookup table for fast UTF8 => UTF16 offset conversion
    utf16indices: Indices.t,
    // cached lookup table for fast LF => CRLF => LF offset conversion
    eolIndices: Indices.t,
  }

  let make = text => {
    utf16indices: Indices.make(computeUTF16SurrogatePairIndices(text)),
    eolIndices: Indices.make(computeCRLFIndices(text)),
  }

  let convert = (self, offset) => {
    // UTF8 -> UTF16
    let offset = Indices.convert(self.utf16indices, offset)
    // LF -> CRLF
    Indices.convert(self.eolIndices, offset)
  }
}
