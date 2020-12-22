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
      | "Interval" =>
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
          ("tag", string("Interval")),
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
      | "Range" =>
        Contents(
          pair(optional(string), array(Range.decode)) |> map(((source, intervals)) => Location(
            source,
            intervals,
          )),
        )
      | "NoRange" => TagOnly(NoLocation)
      | tag => raise(DecodeError("[Agda.Location] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Location(source, intervals) =>
        object_(list{
          ("tag", string("Range")),
          ("contents", (source, intervals) |> pair(nullable(string), array(Range.encode))),
        })
      | NoLocation => object_(list{("tag", string("NoRange"))})
      }
  }
}
