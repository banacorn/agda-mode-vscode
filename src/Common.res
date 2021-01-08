// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

module Offset = {
  type t = int

  let decode = Json.Decode.int
  let encode = Json.Encode.int
}

module Interval = {
  type t = (int, int)

  let contains = (interval, offset) => {
    let (start, end_) = interval
    start <= offset && offset <= end_
  }

  let decode = Json.Decode.pair(Json.Decode.int, Json.Decode.int)
  let encode = Json.Encode.pair(Json.Encode.int, Json.Encode.int)
}

module type Indices = {
  type t
  let make: array<int> => t
  let convert: (t, int) => int
  // expose the intervals for testing
  let expose: t => (array<(int, int)>, int)
}

module Indices: Indices = {
  open Belt
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

module Agda = {
  module Position = {
    type t = {
      pos: option<int>,
      line: int,
      col: int,
    }

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Position" =>
        Contents(
          tuple3(optional(int), int, int) |> map(((pos, line, col)) => {
            pos: pos,
            line: line,
            col: col,
          }),
        )
      | tag => raise(DecodeError("[Agda.Position] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | {pos, line, col} =>
        object_(list{
          ("tag", string("Position")),
          ("contents", (pos, line, col) |> tuple3(nullable(int), int, int)),
        })
      }
  }

  module Range = {
    type t = {
      start: Position.t,
      end_: Position.t,
    }

    let fuse = (a, b) => {
      let start = if a.start.pos > b.start.pos {
        b.start
      } else {
        a.start
      }
      let end_ = if a.end_.pos > b.end_.pos {
        a.end_
      } else {
        b.end_
      }
      {start: start, end_: end_}
    }

    let toString = (self): string =>
      if self.start.line === self.end_.line {
        string_of_int(self.start.line) ++
        ("," ++
        (string_of_int(self.start.col) ++ ("-" ++ string_of_int(self.end_.col))))
      } else {
        string_of_int(self.start.line) ++
        ("," ++
        (string_of_int(self.start.col) ++
        ("-" ++
        (string_of_int(self.end_.line) ++ ("," ++ string_of_int(self.end_.col))))))
      }

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Range" =>
        Contents(
          pair(Position.decode, Position.decode) |> map(((start, end_)) => {
            start: start,
            end_: end_,
          }),
        )
      | tag => raise(DecodeError("[Agda.Range] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | {start, end_} =>
        object_(list{
          ("tag", string("Range")),
          ("contents", (start, end_) |> pair(Position.encode, Position.encode)),
        })
      }
  }

  module Location = {
    type t =
      | NoLocation
      | Location(option<string>, array<Range.t>)

    let parse = %re(
      /* |  different row                    |    same row            | */
      "/^(\\S+)\\:(?:(\\d+)\\,(\\d+)\\-(\\d+)\\,(\\d+)|(\\d+)\\,(\\d+)\\-(\\d+))$/"
    )->Emacs__Parser.captures(captured => {
      open Belt
      open Belt.Option
      let flatten = xs => xs->flatMap(x => x)
      let srcFile = captured[1]->flatten
      let sameRow = captured[6]->flatten->isSome
      if sameRow {
        captured[6]
        ->flatten
        ->flatMap(int_of_string_opt)
        ->flatMap(row =>
          captured[7]
          ->flatten
          ->flatMap(int_of_string_opt)
          ->flatMap(colStart =>
            captured[8]
            ->flatten
            ->flatMap(int_of_string_opt)
            ->flatMap(colEnd => Some(
              Location(
                srcFile,
                [
                  {
                    start: {
                      pos: None,
                      line: row,
                      col: colStart,
                    },
                    end_: {
                      pos: None,
                      line: row,
                      col: colEnd,
                    },
                  },
                ],
              ),
            ))
          )
        )
      } else {
        captured[2]
        ->flatten
        ->flatMap(int_of_string_opt)
        ->flatMap(rowStart =>
          captured[3]
          ->flatten
          ->flatMap(int_of_string_opt)
          ->flatMap(colStart =>
            captured[4]
            ->flatten
            ->flatMap(int_of_string_opt)
            ->flatMap(rowEnd =>
              captured[5]
              ->flatten
              ->flatMap(int_of_string_opt)
              ->flatMap(colEnd => Some(
                Location(
                  srcFile,
                  [
                    {
                      start: {
                        pos: None,
                        line: rowStart,
                        col: colStart,
                      },
                      end_: {
                        pos: None,
                        line: rowEnd,
                        col: colEnd,
                      },
                    },
                  ],
                ),
              ))
            )
          )
        )
      }
    })

    let fuse = (a: t, b: t): t => {
      open Range

      let mergeTouching = (l, e, s, r) =>
        Belt.List.concat(Belt.List.concat(l, list{{start: e.start, end_: s.end_}}), r)

      let rec fuseSome = (s1, r1, s2, r2) => {
        let r1' = Util.List.dropWhile(x => x.end_.pos <= s2.end_.pos, r1)
        helpFuse(r1', list{Range.fuse(s1, s2), ...r2})
      }
      and outputLeftPrefix = (s1, r1, s2, is2) => {
        let (r1', r1'') = Util.List.span(s => s.end_.pos < s2.start.pos, r1)
        Belt.List.concat(Belt.List.concat(list{s1}, r1'), helpFuse(r1'', is2))
      }
      and helpFuse = (a: Belt.List.t<Range.t>, b: Belt.List.t<Range.t>) =>
        switch (a, Belt.List.reverse(a), b, Belt.List.reverse(b)) {
        | (list{}, _, _, _) => a
        | (_, _, list{}, _) => b
        | (list{s1, ...r1}, list{e1, ...l1}, list{s2, ...r2}, list{e2, ...l2}) =>
          if e1.end_.pos < s2.start.pos {
            Belt.List.concat(a, b)
          } else if e2.end_.pos < s1.start.pos {
            Belt.List.concat(b, a)
          } else if e1.end_.pos === s2.start.pos {
            mergeTouching(l1, e1, s2, r2)
          } else if e2.end_.pos === s1.start.pos {
            mergeTouching(l2, e2, s1, r1)
          } else if s1.end_.pos < s2.start.pos {
            outputLeftPrefix(s1, r1, s2, b)
          } else if s2.end_.pos < s1.start.pos {
            outputLeftPrefix(s2, r2, s1, a)
          } else if s1.end_.pos < s2.end_.pos {
            fuseSome(s1, r1, s2, r2)
          } else {
            fuseSome(s2, r2, s1, r1)
          }
        | _ => failwith("something wrong with Location::fuse")
        }
      switch (a, b) {
      | (NoLocation, r2) => r2
      | (r1, NoLocation) => r1
      | (Location(f, r1), Location(_, r2)) =>
        Location(f, helpFuse(Belt.List.fromArray(r1), Belt.List.fromArray(r2))->Belt.List.toArray)
      }
    }

    // hiding Range from Belt
    module Range' = Range
    open! Belt
    module BeltRange = Range
    module Range = Range'

    let toString = (self: t): string =>
      switch self {
      | NoLocation => ""
      | Location(None, xs) =>
        switch (xs[0], xs[Array.length(xs) - 1]) {
        | (Some(first), Some(last)) => Range.toString({start: first.start, end_: last.end_})
        | _ => ""
        }

      | Location(Some(filepath), []) => filepath
      | Location(Some(filepath), xs) =>
        filepath ++
        (":" ++
        switch (xs[0], xs[Array.length(xs) - 1]) {
        | (Some(first), Some(last)) => Range.toString({start: first.start, end_: last.end_})
        | _ => ""
        })
      }

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Location" =>
        Contents(
          pair(optional(string), array(Range.decode)) |> map(((source, intervals)) => Location(
            source,
            intervals,
          )),
        )
      | "NoLocation" => TagOnly(NoLocation)
      | tag => raise(DecodeError("[Agda.Location] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Location(source, intervals) =>
        object_(list{
          ("tag", string("Location")),
          ("contents", (source, intervals) |> pair(nullable(string), array(Range.encode))),
        })
      | NoLocation => object_(list{("tag", string("NoLocation"))})
      }
  }

  module type OffsetConverter = {
    type t
    let make: string => t
    let convert: (t, int) => Offset.t

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
}
