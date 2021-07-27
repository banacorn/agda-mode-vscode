// NOTE:
//
//  VSCode imports should not be allowed in this module, otherwise it would contaminate the view
//

module AgdaPosition = {
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

module AgdaInterval = {
  type t = {
    start: AgdaPosition.t,
    end_: AgdaPosition.t,
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
  let decode: decoder<t> = pair(AgdaPosition.decode, AgdaPosition.decode) |> map(((
    start,
    end_,
  )) => {
    start: start,
    end_: end_,
  })

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | {start, end_} => (start, end_) |> pair(AgdaPosition.encode, AgdaPosition.encode)
    }
}

module AgdaRange = {
  type t =
    | NoRange
    | Range(option<string>, array<AgdaInterval.t>)

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
    open AgdaInterval

    let mergeTouching = (l, e, s, r) =>
      Belt.List.concat(Belt.List.concat(l, list{{start: e.start, end_: s.end_}}), r)

    let rec fuseSome = (s1, r1, s2, r2) => {
      let r1' = Util.List.dropWhile(x => x.end_.pos <= s2.end_.pos, r1)
      helpFuse(r1', list{AgdaInterval.fuse(s1, s2), ...r2})
    }
    and outputLeftPrefix = (s1, r1, s2, is2) => {
      let (r1', r1'') = Util.List.span(s => s.end_.pos < s2.start.pos, r1)
      Belt.List.concat(Belt.List.concat(list{s1}, r1'), helpFuse(r1'', is2))
    }
    and helpFuse = (a: Belt.List.t<AgdaInterval.t>, b: Belt.List.t<AgdaInterval.t>) =>
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
    | Range(Some(filepath), []) => filepath
    | Range(None, xs) =>
      switch (xs[0], xs[Array.length(xs) - 1]) {
      | (Some(first), Some(last)) => AgdaInterval.toString({start: first.start, end_: last.end_})
      | _ => ""
      }
    | Range(Some(filepath), xs) =>
      switch (xs[0], xs[Array.length(xs) - 1]) {
      | (Some(first), Some(last)) =>
        filepath ++ ":" ++ AgdaInterval.toString({start: first.start, end_: last.end_})
      | _ => ""
      }
    }

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Range" =>
      Contents(
        pair(optional(string), array(AgdaInterval.decode)) |> map(((source, intervals)) => Range(
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
        ("contents", (source, intervals) |> pair(nullable(string), array(AgdaInterval.encode))),
      })
    | NoRange => object_(list{("tag", string("NoRange"))})
    }
}

module Link = {
  type t =
    | SrcLoc(AgdaRange.t)
    | Hole(int)

  let toString = x =>
    switch x {
    | SrcLoc(range) => AgdaRange.toString(range)
    | Hole(int) => "?" ++ string_of_int(int)
    }

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "LinkRange" => Contents(AgdaRange.decode |> map(range => SrcLoc(range)))
    | "LinkHole" => Contents(int |> map(index => Hole(index)))
    | tag => raise(DecodeError("[View.Link] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | SrcLoc(range) =>
      object_(list{("tag", string("LinkRange")), ("contents", range |> AgdaRange.encode)})
    | Hole(index) => object_(list{("tag", string("LinkHole")), ("contents", index |> int)})
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
