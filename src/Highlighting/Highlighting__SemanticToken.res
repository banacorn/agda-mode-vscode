open Belt

open Highlighting__Agda

// Tokens for Semantic Highlighting
module type Module = {
  // a Range that does not span multiple lines
  module SingleLineRange: {
    type t = {
      line: int,
      column: (int, int),
    }
    let toString: t => string
    let toVsCodeRange: t => VSCode.Range.t
    let splitRange: (VSCode.TextDocument.t, VSCode.Range.t) => array<t>
  }

  type t = {
    range: SingleLineRange.t,
    type_: Aspect.TokenType.t,
    modifiers: option<array<Aspect.TokenModifier.t>>,
  }
  let toString: t => string
}

module Module: Module = {
  // a Range that does not span multiple lines
  module SingleLineRange = {
    type t = {
      line: int,
      column: (int, int),
    }

    let toString = ({line, column}) =>
      string_of_int(line) ++ ":" ++ string_of_int(fst(column)) ++ "-" ++ string_of_int(snd(column))

    let toVsCodeRange = ({line, column}) =>
      VSCode.Range.make(
        VSCode.Position.make(line, fst(column)),
        VSCode.Position.make(line, snd(column)),
      )

    // split a single range into multiple ranges that only occupies single lines
    let splitRange = (doc: VSCode.TextDocument.t, range: VSCode.Range.t): array<t> => {
      open VSCode.Range
      open VSCode.Position
      let startingLine = line(start(range))
      let endingLine = line(end_(range))

      let ranges = []
      for i in startingLine to endingLine {
        let startingPoint = if i == startingLine {
          start(range)
        } else {
          VSCode.Position.make(i, 0)
        }
        let endingPoint = if i == endingLine {
          end_(range)
        } else {
          let offset = doc->VSCode.TextDocument.offsetAt(VSCode.Position.make(i + 1, 0)) - 1
          doc->VSCode.TextDocument.positionAt(offset)
        }
        Js.Array.push(
          {
            line: VSCode.Position.line(startingPoint),
            column: (
              VSCode.Position.character(startingPoint),
              VSCode.Position.character(endingPoint),
            ),
          },
          ranges,
        )->ignore
      }
      ranges
    }
  }

  type t = {
    range: SingleLineRange.t,
    type_: Aspect.TokenType.t,
    modifiers: option<array<Aspect.TokenModifier.t>>,
  }

  let toString = token => {
    // let range = token.range
    let tokenType = token.type_->Aspect.TokenType.toString
    let modifiers =
      token.modifiers
      ->Option.mapWithDefault([], xs => xs->Array.map(Aspect.TokenModifier.toString))
      ->Util.Pretty.array

    "(" ++ SingleLineRange.toString(token.range) ++ ") " ++ tokenType ++ " " ++ modifiers
  }
}

include Module
