// Pure token data types and parsing logic
// Extracted from Tokens.res for better modularity

module Aspect = Highlighting__AgdaAspect

// phantom types for differentiating offsets counted by Agda and VSCode
type agdaOffset
type vscodeOffset

type filepath = string

type t<'a> = {
  start: int, // agda offset
  end: int, // agda offset
  aspects: array<Aspect.t>, // a list of names of aspects
  isTokenBased: bool,
  note: option<string>,
  source: option<(Parser.Filepath.t, int)>, // The defining module and the position in that module
}

let toStringWithoutOffsets = self =>
  self.aspects->Util.Pretty.array(Aspect.toString) ++
    switch self.source {
    | None => ""
    | Some((_s, i)) => " [src: " ++ string_of_int(i) ++ "]"
    }

let toString = self =>
  "(" ++
  string_of_int(self.start) ++
  ", " ++
  string_of_int(self.end) ++
  ") " ++
  toStringWithoutOffsets(self)

// Parse token from SExpression
let parse: Parser.SExpression.t => option<t<agdaOffset>> = x => {
  open Parser.SExpression
  switch x {
  | A(_) => None
  | L(xs) =>
    switch xs {
    | [A(start'), A(end'), aspects, _, _, L([A(filepath), _, A(index')])] =>
      int_of_string_opt(start')->Option.flatMap(start =>
        int_of_string_opt(end')->Option.flatMap(end =>
          int_of_string_opt(index')->Option.map(
            index => {
              start: start - 1,
              end: end - 1,
              aspects: flatten(aspects)->Array.map(Aspect.parse),
              isTokenBased: false, // NOTE: fix this
              note: None, // NOTE: fix this
              source: Some((Parser.Filepath.make(filepath), index)),
            },
          )
        )
      )

    | [A(start'), A(end'), aspects] =>
      int_of_string_opt(start')->Option.flatMap(start =>
        int_of_string_opt(end')->Option.map(end => {
          start: start - 1,
          end: end - 1,
          aspects: flatten(aspects)->Array.map(Aspect.parse),
          isTokenBased: false, // NOTE: fix this
          note: None, // NOTE: fix this
          source: None,
        })
      )
    | [A(start'), A(end'), aspects, _] =>
      int_of_string_opt(start')->Option.flatMap(start =>
        int_of_string_opt(end')->Option.map(end => {
          start: start - 1,
          end: end - 1,
          aspects: flatten(aspects)->Array.map(Aspect.parse),
          isTokenBased: false, // NOTE: fix this
          note: None, // NOTE: fix this
          source: None,
        })
      )
    | _ => None
    }
  }
}

// Parse direct highlighting tokens from SExpression array
let parseDirectHighlightings: array<Parser.SExpression.t> => array<t<agdaOffset>> = tokens =>
  tokens->Array.sliceToEnd(~start=2)->Array.map(parse)->Array.filterMap(x => x)

// JSON decoder for token
let decodeToken: JsonCombinators.Json.Decode.t<t<agdaOffset>> = {
  open JsonCombinators.Json.Decode
  Util.Decode.tuple6(
    int,
    int,
    array(string),
    bool,
    option(string),
    option(pair(string, int)),
  )->map(((start, end, aspects, isTokenBased, note, source)) => {
    start: start - 1,
    end: end - 1,
    aspects: aspects->Array.map(Aspect.parse),
    isTokenBased,
    note,
    source: source->Option.map(((filepath, offset)) => (Parser.Filepath.make(filepath), offset)),
  })
}

// JSON decoder for highlighting info response
let decodeResponseHighlightingInfoDirect = {
  open JsonCombinators.Json.Decode
  pair(bool, array(decodeToken))->map(((keepHighlighting, xs)) => (keepHighlighting, xs))
}