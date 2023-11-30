// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

open Belt

module Term = {
  type t =
    | Plain(string)
    | QuestionMark(int)
    | Underscore(string)

  let render = x =>
    switch x {
    | Plain(string) => RichText.string(string)
    | QuestionMark(i) => RichText.hole(i)
    | Underscore(string) => RichText.string(string)
    }
}

module Expr = {
  type t = array<Term.t>
  let parse = raw =>
    raw
    ->Js.String.trim
    /* 1         2 */
    ->Js.String.splitByRe(%re("/(\?\d+)|(\_\d+[^\}\)\s]*)/"), _)
    ->// RegEx updated to v10.1.4
    Array.mapWithIndex((i, token) =>
      switch mod(i, 3) {
      | 1 =>
        token
        ->Option.map(Js.String.sliceToEnd(~from=1))
        ->Option.flatMap(int_of_string_opt)
        ->Option.map(x => Term.QuestionMark(x))
      | 2 => token->Option.map(x => Term.Underscore(x))
      | _ =>
        token->Option.flatMap(x =>
          if x == "" {
            None
          } else {
            Some(Term.Plain(x))
          }
        )
      }
    )
    ->Array.keepMap(x => x)
    ->(x => Some(x))
  let render = xs => xs->Array.map(Term.render)->RichText.concatMany
}

module OutputConstraint: {
  // RegEx updated to v10.1.4
  type t =
    | OfType(RichText.t, RichText.t)
    | JustType(RichText.t)
    | JustSort(RichText.t)
    | Others(RichText.t)
  let parse: string => option<t>
  let renderItem: (t, option<Common.AgdaRange.t>) => Item.t
} = {
  // RegEx updated to v10.1.4
  type t =
    | OfType(RichText.t, RichText.t)
    | JustType(RichText.t)
    | JustSort(RichText.t)
    | Others(RichText.t)

  let parseOfType =
    %re("/^([^\:]*) \: ((?:\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured
      ->Emacs__Parser.at(2, Expr.parse)
      ->Option.flatMap(type_ =>
        captured
        ->Emacs__Parser.at(1, Expr.parse)
        ->Option.flatMap(term => Some(OfType(Expr.render(term), Expr.render(type_))))
      )
    )
  let parseJustType =
    %re("/^Type ((?:\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(type_ => JustType(Expr.render(type_)))
    )
  let parseJustSort =
    %re("/^Sort ((?:\n|.)+)/")->Emacs__Parser.captures(captured =>
      captured->Emacs__Parser.at(1, Expr.parse)->Option.map(sort => JustSort(Expr.render(sort)))
    )
  let parseOthers = raw => raw->Expr.parse->Option.map(raw' => Others(Expr.render(raw')))

  let parse = Emacs__Parser.choice([parseOfType, parseJustType, parseJustSort, parseOthers])

  let renderItem = (value, location) => {
    open! RichText
    let inlines = switch value {
    | OfType(e, t) => concatMany([e, string(" : "), t])
    | JustType(e) => concatMany([string("Type "), e])
    | JustSort(e) => concatMany([string("Sort "), e])
    | Others(e) => concatMany([e])
    }
    Item.Unlabeled(inlines, None, location)
  }
}

module Output = {
  type t = Output(OutputConstraint.t, option<Common.AgdaRange.t>)

  // parsing serialized data
  let parseOutputWithoutLocation = raw =>
    raw->OutputConstraint.parse->Option.map(x => Output(x, None))
  let parseOutputWithLocation = %re(
    "/((?:\\n|.)*\\S+)\\s*\[ at ([^\\]]+) \]/"
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

  let renderItem = value => {
    let Output(oc, location) = value
    OutputConstraint.renderItem(oc, location)
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
      intervals,
      lastInterval,
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
    let regexp = %re("/\r\n/g") // RegEx updated to v10.1.4
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
