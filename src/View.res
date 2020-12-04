module Header = {
  type t =
    | Plain(string)
    | Success(string)
    | Warning(string)
    | Error(string)

  let toString = x =>
    switch x {
    | Plain(string) => string
    | Success(string) => string
    | Warning(string) => string
    | Error(string) => string
    }

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Plain" => Contents(string |> map(text => Plain(text)))
    | "Success" => Contents(string |> map(text => Success(text)))
    | "Warning" => Contents(string |> map(text => Warning(text)))
    | "Error" => Contents(string |> map(text => Error(text)))
    | tag => raise(DecodeError("[Header] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Plain(text) => object_(list{("tag", string("Plain")), ("contents", text |> string)})
    | Success(text) => object_(list{("tag", string("Success")), ("contents", text |> string)})
    | Warning(text) => object_(list{("tag", string("Warning")), ("contents", text |> string)})
    | Error(text) => object_(list{("tag", string("Error")), ("contents", text |> string)})
    }
}

module Prompt = {
  type t = {
    body: option<string>,
    placeholder: option<string>,
    value: option<string>,
  }

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Prompt" =>
      Contents(
        tuple3(optional(string), optional(string), optional(string)) |> map(((
          body,
          placeholder,
          value,
        )) => {body: body, placeholder: placeholder, value: value}),
      )
    | tag => raise(DecodeError("[Prompt] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | {body, placeholder, value} =>
      object_(list{
        ("tag", string("Prompt")),
        (
          "contents",
          (body, placeholder, value) |> tuple3(
            nullable(string),
            nullable(string),
            nullable(string),
          ),
        ),
      })
    }
}
module Body = {
  module Emacs = {
    type t =
      | Outputs
      | AllGoalsWarnings
      | GoalType
      | SearchAbout
      | Error
      | Text

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Outputs" => TagOnly(Outputs)
      | "AllGoalsWarnings" => TagOnly(AllGoalsWarnings)
      | "GoalType" => TagOnly(GoalType)
      | "SearchAbout" => TagOnly(SearchAbout)
      | "Error" => TagOnly(Error)
      | "Text" => TagOnly(Text)
      | tag => raise(DecodeError("[Body.Emacs] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Outputs => object_(list{("tag", string("Outputs"))})
      | AllGoalsWarnings => object_(list{("tag", string("AllGoalsWarnings"))})
      | GoalType => object_(list{("tag", string("GoalType"))})
      | SearchAbout => object_(list{("tag", string("SearchAbout"))})
      | Error => object_(list{("tag", string("Error"))})
      | Text => object_(list{("tag", string("Text"))})
      }
  }

  type t =
    | Nothing
    | Plain(string)
    | Emacs(Emacs.t, string, string)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Nothing" => TagOnly(Nothing)
    | "Plain" => Contents(string |> map(text => Plain(text)))
    | "Emacs" =>
      Contents(
        tuple3(Emacs.decode, string, string) |> map(((kind, header, body)) => Emacs(
          kind,
          header,
          body,
        )),
      )
    | tag => raise(DecodeError("[Body] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Nothing => object_(list{("tag", string("Nothing"))})
    | Plain(text) => object_(list{("tag", string("Plain")), ("contents", text |> string)})
    | Emacs(kind, header, body) =>
      object_(list{
        ("tag", string("Emacs")),
        ("contents", (kind, header, body) |> tuple3(Emacs.encode, string, string)),
      })
    }
}

open Belt
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
    | "Position" => Contents(tuple3(optional(int), int, int) |> map(((pos, line, col)) => {
          pos: pos,
          line: line,
          col: col,
        }))
    | tag => raise(DecodeError("[View.Position] Unknown constructor: " ++ tag))
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
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Interval" => Contents(pair(Position.decode, Position.decode) |> map(((start, end_)) => {
          start: start,
          end_: end_,
        }))
    | tag => raise(DecodeError("[View.Interval] Unknown constructor: " ++ tag))
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

module Range = {
  type t =
    | NoRange
    | Range(option<string>, array<Interval.t>)

  let parse = %re(
    /* |  different row                    |    same row            | */
    "/^(\\S+)\\:(?:(\\d+)\\,(\\d+)\\-(\\d+)\\,(\\d+)|(\\d+)\\,(\\d+)\\-(\\d+))$/"
  )->Emacs__Parser.captures(captured => {
    open Option
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
          captured[8]->flatten->flatMap(int_of_string_opt)->flatMap(colEnd => Some(
            Range(
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
            captured[5]->flatten->flatMap(int_of_string_opt)->flatMap(colEnd => Some(
              Range(
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
    open Interval

    let mergeTouching = (l, e, s, r) =>
      List.concat(List.concat(l, list{{start: e.start, end_: s.end_}}), r)

    let rec fuseSome = (s1, r1, s2, r2) => {
      let r1' = Util.List.dropWhile(x => x.end_.pos <= s2.end_.pos, r1)
      helpFuse(r1', list{Interval.fuse(s1, s2), ...r2})
    }
    and outputLeftPrefix = (s1, r1, s2, is2) => {
      let (r1', r1'') = Util.List.span(s => s.end_.pos < s2.start.pos, r1)
      List.concat(List.concat(list{s1}, r1'), helpFuse(r1'', is2))
    }
    and helpFuse = (a: List.t<Interval.t>, b: List.t<Interval.t>) =>
      switch (a, List.reverse(a), b, List.reverse(b)) {
      | (list{}, _, _, _) => a
      | (_, _, list{}, _) => b
      | (list{s1, ...r1}, list{e1, ...l1}, list{s2, ...r2}, list{e2, ...l2}) =>
        if e1.end_.pos < s2.start.pos {
          List.concat(a, b)
        } else if e2.end_.pos < s1.start.pos {
          List.concat(b, a)
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
      Range(f, helpFuse(List.fromArray(r1), List.fromArray(r2))->List.toArray)
    }
  }

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
    | tag => raise(DecodeError("[View.Range] Unknown constructor: " ++ tag))
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

module Link = {
  type t =
    | ToRange(Range.t)
    | ToHole(int)

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "ToRange" => Contents(Range.decode |> map(range => ToRange(range)))
    | "ToHole" => Contents(int |> map(index => ToHole(index)))
    | tag => raise(DecodeError("[View.Link] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | ToRange(range) =>
      object_(list{("tag", string("ToRange")), ("contents", range |> Range.encode)})
    | ToHole(index) => object_(list{("tag", string("ToHole")), ("contents", index |> int)})
    }
}

module EventToView = {
  module InputMethod = {
    type t =
      | Activate
      | Deactivate
      | Update(string, Translator.translation, int)
      | BrowseUp
      | BrowseRight
      | BrowseDown
      | BrowseLeft

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Activate" => TagOnly(Activate)
      | "Deactivate" => TagOnly(Deactivate)
      | "Update" =>
        Contents(
          tuple3(string, Translator.decode, int) |> map(((sequence, translation, index)) => Update(
            sequence,
            translation,
            index,
          )),
        )
      | "BrowseUp" => TagOnly(BrowseUp)
      | "BrowseRight" => TagOnly(BrowseRight)
      | "BrowseDown" => TagOnly(BrowseDown)
      | "BrowseLeft" => TagOnly(BrowseLeft)
      | tag => raise(DecodeError("[EventToView.InputMethod] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Activate => object_(list{("tag", string("Activate"))})
      | Deactivate => object_(list{("tag", string("Deactivate"))})
      | Update(sequence, translation, index) =>
        object_(list{
          ("tag", string("Update")),
          ("contents", (sequence, translation, index) |> tuple3(string, Translator.encode, int)),
        })
      | BrowseUp => object_(list{("tag", string("BrowseUp"))})
      | BrowseRight => object_(list{("tag", string("BrowseRight"))})
      | BrowseDown => object_(list{("tag", string("BrowseDown"))})
      | BrowseLeft => object_(list{("tag", string("BrowseLeft"))})
      }
  }

  type t =
    | Display(Header.t, Body.t)
    | PromptInterrupt
    | PromptIMUpdate(string)
    | InputMethod(InputMethod.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Display" =>
      Contents(pair(Header.decode, Body.decode) |> map(((header, body)) => Display(header, body)))
    | "PromptInterrupt" => TagOnly(PromptInterrupt)
    | "PromptIMUpdate" => Contents(string |> map(text => PromptIMUpdate(text)))
    | "InputMethod" => Contents(InputMethod.decode |> map(x => InputMethod(x)))
    | tag => raise(DecodeError("[EventToView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Display(header, body) =>
      object_(list{
        ("tag", string("Display")),
        ("contents", (header, body) |> pair(Header.encode, Body.encode)),
      })
    | PromptInterrupt => object_(list{("tag", string("PromptInterrupt"))})
    | PromptIMUpdate(text) =>
      object_(list{("tag", string("PromptIMUpdate")), ("contents", text |> string)})
    | InputMethod(payload) =>
      object_(list{("tag", string("InputMethod")), ("contents", payload |> InputMethod.encode)})
    }
}

module Request = {
  type t = Prompt(Header.t, Prompt.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Prompt" =>
      Contents(
        pair(Header.decode, Prompt.decode) |> map(((header, prompt)) => Prompt(header, prompt)),
      )
    | tag => raise(DecodeError("[Request] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Prompt(header, prompt) =>
      object_(list{
        ("tag", string("Prompt")),
        ("contents", (header, prompt) |> pair(Header.encode, Prompt.encode)),
      })
    }
}

module RequestOrEventToView = {
  type t =
    | Request(Request.t)
    | Event(EventToView.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Request" => Contents(Request.decode |> map(x => Request(x)))
    | "Event" => Contents(EventToView.decode |> map(x => Event(x)))
    | tag => raise(DecodeError("[RequestOrEventToView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Request(payload) =>
      object_(list{("tag", string("Request")), ("contents", payload |> Request.encode)})
    | Event(payload) =>
      object_(list{("tag", string("Event")), ("contents", payload |> EventToView.encode)})
    }
}

module Response = {
  type t =
    | PromptSuccess(string)
    | PromptInterrupted

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "PromptSuccess" => Contents(string |> map(result => PromptSuccess(result)))
    | "PromptInterrupted" => TagOnly(PromptInterrupted)
    | tag => raise(DecodeError("[Response] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | PromptSuccess(result) =>
      object_(list{("tag", string("PromptSuccess")), ("contents", result |> string)})
    | PromptInterrupted => object_(list{("tag", string("PromptInterrupted"))})
    }
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

  module Prompt = {
    type t =
      | Select(array<int>)
      | Change(string)
      | BrowseUp
      | BrowseDown
      | BrowseLeft
      | BrowseRight

    open Json.Decode
    open Util.Decode

    let decode: decoder<t> = sum(x =>
      switch x {
      | "Select" => Contents(array(int) |> map(offsets => Select(offsets)))
      | "Change" => Contents(string |> map(char => Change(char)))
      | "BrowseUp" => TagOnly(BrowseUp)
      | "BrowseDown" => TagOnly(BrowseDown)
      | "BrowseLeft" => TagOnly(BrowseLeft)
      | "BrowseRight" => TagOnly(BrowseRight)
      | tag => raise(DecodeError("[EventFromView.Prompt] Unknown constructor: " ++ tag))
      }
    )

    open! Json.Encode
    let encode: encoder<t> = x =>
      switch x {
      | Select(offsets) =>
        object_(list{("tag", string("Select")), ("contents", offsets |> array(int))})
      | Change(char) => object_(list{("tag", string("Change")), ("contents", char |> string)})
      | BrowseUp => object_(list{("tag", string("BrowseUp"))})
      | BrowseDown => object_(list{("tag", string("BrowseDown"))})
      | BrowseLeft => object_(list{("tag", string("BrowseLeft"))})
      | BrowseRight => object_(list{("tag", string("BrowseRight"))})
      }
  }

  type t =
    | Initialized
    | Destroyed
    | InputMethod(InputMethod.t)
    | PromptChange(Prompt.t)
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
    | "PromptChange" => Contents(Prompt.decode |> map(action => PromptChange(action)))
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
    | PromptChange(action) =>
      object_(list{("tag", string("PromptChange")), ("contents", action |> Prompt.encode)})
    | JumpToTarget(link) =>
      object_(list{("tag", string("JumpToTarget")), ("contents", link |> Link.encode)})
    | MouseOver(link) =>
      object_(list{("tag", string("MouseOver")), ("contents", link |> Link.encode)})
    | MouseOut(link) =>
      object_(list{("tag", string("MouseOut")), ("contents", link |> Link.encode)})
    }
}

module ResponseOrEventFromView = {
  type t =
    | Response(Response.t)
    | Event(EventFromView.t)

  // JSON encode/decode

  open Json.Decode
  open Util.Decode

  let decode: decoder<t> = sum(x =>
    switch x {
    | "Response" => Contents(Response.decode |> map(x => Response(x)))
    | "Event" => Contents(EventFromView.decode |> map(x => Event(x)))
    | tag => raise(DecodeError("[ResponseOrEventFromView] Unknown constructor: " ++ tag))
    }
  )

  open! Json.Encode
  let encode: encoder<t> = x =>
    switch x {
    | Response(payload) =>
      object_(list{("tag", string("Response")), ("contents", payload |> Response.encode)})
    | Event(payload) =>
      object_(list{("tag", string("Event")), ("contents", payload |> EventFromView.encode)})
    }
}
