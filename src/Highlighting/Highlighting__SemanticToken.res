// VS Code Token type
// https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#semantic-token-classification
module TokenType = {
  type t =
    | Namespace
    | Type
    | Class
    | Enum
    | Interface
    | Struct
    | TypeParameter
    | Parameter
    | Variable
    | Property
    | EnumMember
    | Event
    | Function
    | Member
    | Macro
    | Label
    | Comment
    | String
    | Keyword
    | Number
    | Regexp
    | Operator

  let toString = x =>
    switch x {
    | Namespace => "namespace"
    | Type => "type"
    | Class => "class"
    | Enum => "enum"
    | Interface => "interface"
    | Struct => "struct"
    | TypeParameter => "typeParameter"
    | Parameter => "parameter"
    | Variable => "variable"
    | Property => "property"
    | EnumMember => "enumMember"
    | Event => "event"
    | Function => "function"
    | Member => "member"
    | Macro => "macro"
    | Label => "label"
    | Comment => "comment"
    | String => "string"
    | Keyword => "keyword"
    | Number => "number"
    | Regexp => "regexp"
    | Operator => "operator"
    }

  let enumurate = [
    "namespace",
    "type",
    "class",
    "enum",
    "interface",
    "struct",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "enumMember",
    "event",
    "function",
    "member",
    "macro",
    "label",
    "comment",
    "string",
    "keyword",
    "number",
    "regexp",
    "operator",
  ]
}

// VS Code Token modifier
// https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide#semantic-token-classification
module TokenModifier = {
  type t =
    | Declaration
    | Readonly
    | Static
    | Deprecated
    | Abstract
    | Async
    | Modification
    | Documentation
    | DefaultLibrary

  let toString = x =>
    switch x {
    | Declaration => "declaration"
    | Readonly => "readonly"
    | Static => "static"
    | Deprecated => "deprecated"
    | Abstract => "abstract"
    | Async => "async"
    | Modification => "modification"
    | Documentation => "documentation"
    | DefaultLibrary => "defaultLibrary"
    }

  let enumurate = [
    "declaration",
    "readonly",
    "static",
    "deprecated",
    "abstract",
    "async",
    "modification",
    "documentation",
    "defaultLibrary",
  ]
}

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
    type_: TokenType.t,
    modifiers: option<array<TokenModifier.t>>,
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
        ranges->Array.push({
          line: VSCode.Position.line(startingPoint),
          column: (
            VSCode.Position.character(startingPoint),
            VSCode.Position.character(endingPoint),
          ),
        })
      }
      ranges
    }
  }

  type t = {
    range: SingleLineRange.t,
    type_: TokenType.t,
    modifiers: option<array<TokenModifier.t>>,
  }

  let toString = token => {
    // let range = token.range
    let tokenType = token.type_->TokenType.toString
    let modifiers =
      token.modifiers
      ->Option.mapOr([], xs => xs->Array.map(TokenModifier.toString))
      ->Util.Pretty.array

    "(" ++ SingleLineRange.toString(token.range) ++ ") " ++ tokenType ++ " " ++ modifiers
  }
}

include Module
