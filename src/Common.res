// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

module Agda = {
  module Position = {
    type t = {
      line: int,
      col: int,
      pos: int,
    }

    open Json.Decode
    let decode: decoder<t> = tuple3(int, int, int) |> map(((line, col, pos)) => {
      line: line,
      col: col,
      pos: pos,
    })

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | {line, col, pos} => (line, col, pos) |> tuple3(int, int, int)
      }
  }

  module Interval = {
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
    let decode: decoder<t> = pair(Position.decode, Position.decode) |> map(((start, end_)) => {
      start: start,
      end_: end_,
    })

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | {start, end_} => (start, end_) |> pair(Position.encode, Position.encode)
      }
  }

  module Range = {
    type t =
      | NoRange
      | Range(option<string>, array<Interval.t>)

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
              Range(
                srcFile,
                [
                  {
                    start: {
                      pos: 0,
                      line: row,
                      col: colStart,
                    },
                    end_: {
                      pos: 0,
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
                Range(
                  srcFile,
                  [
                    {
                      start: {
                        pos: 0,
                        line: rowStart,
                        col: colStart,
                      },
                      end_: {
                        pos: 0,
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
      open Interval

      let mergeTouching = (l, e, s, r) =>
        Belt.List.concat(Belt.List.concat(l, list{{start: e.start, end_: s.end_}}), r)

      let rec fuseSome = (s1, r1, s2, r2) => {
        let r1' = Util.List.dropWhile(x => x.end_.pos <= s2.end_.pos, r1)
        helpFuse(r1', list{Interval.fuse(s1, s2), ...r2})
      }
      and outputLeftPrefix = (s1, r1, s2, is2) => {
        let (r1', r1'') = Util.List.span(s => s.end_.pos < s2.start.pos, r1)
        Belt.List.concat(Belt.List.concat(list{s1}, r1'), helpFuse(r1'', is2))
      }
      and helpFuse = (a: Belt.List.t<Interval.t>, b: Belt.List.t<Interval.t>) =>
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
        | _ => failwith("something wrong with Range::fuse")
        }
      switch (a, b) {
      | (NoRange, r2) => r2
      | (r1, NoRange) => r1
      | (Range(f, r1), Range(_, r2)) =>
        Range(f, helpFuse(Belt.List.fromArray(r1), Belt.List.fromArray(r2))->Belt.List.toArray)
      }
    }

    open Belt
    let toString = (self: t): string =>
      switch self {
      | NoRange => ""
      | Range(None, xs) =>
        switch (xs[0], xs[Array.length(xs) - 1]) {
        | (Some(first), Some(last)) => Interval.toString({start: first.start, end_: last.end_})
        | _ => ""
        }

      | Range(Some(filepath), []) => filepath
      | Range(Some(filepath), xs) =>
        filepath ++
        (":" ++
        switch (xs[0], xs[Array.length(xs) - 1]) {
        | (Some(first), Some(last)) => Interval.toString({start: first.start, end_: last.end_})
        | _ => ""
        })
      }

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Range" =>
        Contents(
          pair(optional(string), array(Interval.decode)) |> map(((source, intervals)) => Range(
            source,
            intervals,
          )),
        )
      | "NoRange" => TagOnly(NoRange)
      | tag => raise(DecodeError("[Agda.Range] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Range(source, intervals) =>
        object_(list{
          ("tag", string("Range")),
          ("contents", (source, intervals) |> pair(nullable(string), array(Interval.encode))),
        })
      | NoRange => object_(list{("tag", string("NoRange"))})
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
}

module Link = {
  type t =
    | ToRange(Agda.Range.t)
    | ToHole(int)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "ToRange" => Contents(Agda.Range.decode |> map(range => ToRange(range)))
    | "ToHole" => Contents(int |> map(index => ToHole(index)))
    | tag => raise(DecodeError("[View.Link] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | ToRange(range) =>
      object_(list{("tag", string("ToRange")), ("contents", range |> Agda.Range.encode)})
    | ToHole(index) => object_(list{("tag", string("ToHole")), ("contents", index |> int)})
    }
}

// NOTE: This is not related to VSCode or Agda
// NOTE: eliminate this
module Interval = {
  type t = (int, int)

  let contains = (interval, offset) => {
    let (start, end_) = interval
    start <= offset && offset <= end_
  }

  let decode = Json.Decode.pair(Json.Decode.int, Json.Decode.int)
  let encode = Json.Encode.pair(Json.Encode.int, Json.Encode.int)
}

module EventFromView = {
  module InputMethod = {
    type t =
      | InsertChar(string)
      | ChooseSymbol(string)

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "InsertChar" => Contents(string |> map(char => InsertChar(char)))
      | "ChooseSymbol" => Contents(string |> map(char => ChooseSymbol(char)))
      | tag => raise(DecodeError("[EventFromView.InputMethod] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | InsertChar(char) =>
        object_(list{("tag", string("InsertChar")), ("contents", char |> string)})
      | ChooseSymbol(symbol) =>
        object_(list{("tag", string("ChooseSymbol")), ("contents", symbol |> string)})
      }
  }

  module PromptIMUpdate = {
    type t =
      | MouseSelect(Interval.t)
      | KeyUpdate(string)
      | BrowseUp
      | BrowseDown
      | BrowseLeft
      | BrowseRight
      | Escape

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "MouseSelect" => Contents(Interval.decode |> map(interval => MouseSelect(interval)))
      | "KeyUpdate" => Contents(string |> map(char => KeyUpdate(char)))
      | "BrowseUp" => TagOnly(BrowseUp)
      | "BrowseDown" => TagOnly(BrowseDown)
      | "BrowseLeft" => TagOnly(BrowseLeft)
      | "BrowseRight" => TagOnly(BrowseRight)
      | "Escape" => TagOnly(Escape)
      | tag => raise(DecodeError("[EventFromView.Prompt] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | MouseSelect(interval) =>
        object_(list{("tag", string("MouseSelect")), ("contents", interval |> Interval.encode)})
      | KeyUpdate(char) => object_(list{("tag", string("KeyUpdate")), ("contents", char |> string)})
      | BrowseUp => object_(list{("tag", string("BrowseUp"))})
      | BrowseDown => object_(list{("tag", string("BrowseDown"))})
      | BrowseLeft => object_(list{("tag", string("BrowseLeft"))})
      | BrowseRight => object_(list{("tag", string("BrowseRight"))})
      | Escape => object_(list{("tag", string("Escape"))})
      }
  }

  type t =
    | Initialized
    | Destroyed
    | InputMethod(InputMethod.t)
    | PromptIMUpdate(PromptIMUpdate.t)
    | JumpToTarget(Link.t)
    | MouseOver(Link.t)
    | MouseOut(Link.t)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Initialized" => TagOnly(Initialized)
    | "Destroyed" => TagOnly(Destroyed)
    | "InputMethod" => Contents(InputMethod.decode |> map(action => InputMethod(action)))
    | "PromptIMUpdate" => Contents(PromptIMUpdate.decode |> map(action => PromptIMUpdate(action)))
    | "JumpToTarget" => Contents(Link.decode |> map(link => JumpToTarget(link)))
    | "MouseOver" => Contents(Link.decode |> map(link => MouseOver(link)))
    | "MouseOut" => Contents(Link.decode |> map(link => MouseOut(link)))
    | tag => raise(DecodeError("[Response.EventFromView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Initialized => object_(list{("tag", string("Initialized"))})
    | Destroyed => object_(list{("tag", string("Destroyed"))})
    | InputMethod(action) =>
      object_(list{("tag", string("InputMethod")), ("contents", action |> InputMethod.encode)})
    | PromptIMUpdate(action) =>
      object_(list{
        ("tag", string("PromptIMUpdate")),
        ("contents", action |> PromptIMUpdate.encode),
      })
    | JumpToTarget(link) =>
      object_(list{("tag", string("JumpToTarget")), ("contents", link |> Link.encode)})
    | MouseOver(link) =>
      object_(list{("tag", string("MouseOver")), ("contents", link |> Link.encode)})
    | MouseOut(link) =>
      object_(list{("tag", string("MouseOut")), ("contents", link |> Link.encode)})
    }
}
