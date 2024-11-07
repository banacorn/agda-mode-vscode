// All RegExps in this file has been updated to work with ReScript v10.1.4

open Belt
open Common

module FileType = {
  type t =
    | Agda
    | LiterateTeX
    | LiterateRST
    | LiterateMarkdown
    | LiterateOrg
  let parse = filepath =>
    if Js.Re.test_(%re("/\.lagda\.rst$/i"), Parser.filepath(filepath)) {
      LiterateRST
    } else if Js.Re.test_(%re("/\.lagda\.md$/i"), Parser.filepath(filepath)) {
      LiterateMarkdown
    } else if Js.Re.test_(%re("/\.lagda\.tex$|\.lagda$/i"), Parser.filepath(filepath)) {
      LiterateTeX
    } else if Js.Re.test_(%re("/\.lagda\.org$/i"), Parser.filepath(filepath)) {
      LiterateOrg
    } else {
      Agda
    }
}

module Token = {
  type kind =
    | AgdaRaw
    | Literate
    | Comment
    | GoalBracket
    | /* QM: Question marks */
    GoalQMRaw
    | GoalQM
  type t = {
    content: string,
    range: (int, int),
    kind: kind,
  }

  let isHole = token =>
    switch token.kind {
    | GoalBracket
    | GoalQM => true
    | _ => false
    }
}

module Lexer = {
  type t = array<Token.t>
  // return
  let make = (raw: string): t => [{content: raw, range: (0, String.length(raw)), kind: AgdaRaw}]
  // bind >==
  /* break tokens down into smaller pieces

     regex     : regex to perform split on a token
     sourceType: the type of token to look for and perform splitting
     targetType: the type of token given to the splitted tokens when identified */
  let lex = (regex: Js.Re.t, source: Token.kind, target: Token.kind, tokens): t => {
    let f = (token: Token.t) =>
      if token.kind === source {
        let cursor = ref(fst(token.range))
        token.content
        ->Js.String.splitByRe(regex, _)
        ->Array.keepMap(x => x)
        ->Array.map(content => {
          let kind = Js.Re.test_(regex, content) ? target : source
          let cursorOld = cursor.contents
          cursor := cursor.contents + String.length(content)
          open Token
          {content, range: (cursorOld, cursor.contents), kind}
        })
      } else {
        [token]
      }
    tokens->Array.map(f)->Array.concatMany
  }

  // transforms a list of tokens while preserving the ranges
  let map = (f: Token.t => Token.t, self): t => {
    let delta = ref(0)
    self->Array.map(token => {
      let {Token.content: content, kind} = f(token)
      let (start, end_) = token.range
      let lengthDiff = String.length(content) - String.length(token.content)
      let result = {
        open Token
        {
          content,
          range: (start + delta.contents, end_ + delta.contents + lengthDiff),
          kind,
        }
      }
      delta := delta.contents + lengthDiff
      result
    })
  }

  // only apply map(f) on a specific tokenType
  let mapOnly = (kind: Token.kind, f: Token.t => Token.t, self): t =>
    self |> map(token => token.kind === kind ? f(token) : token)
}

module Regex = {
  // RegEx updated to v10.1.4
  let texBegin = %re("/\\begin\{code\}.*/")
  let texEnd = %re("/\\end\{code\}.*/")
  let markdown = %re("/\`\`\`(agda)?/")
  let rstBegin = %re("/\:\:/")
  let rstEnd = %re("/^[^\s]/")
  let orgBegin = %re("/\#\+begin\_src agda2/i")
  let orgEnd = %re("/\#\+end\_src/i")

  let comment = %re(
    "/((?<=^|[\s\"\_\;\.\(\)\{\}\@])--[^\r\n]*(?:\r|\n|$))|(\{-(?:[^-]|[\r\n]|(?:-+(?:[^-\}]|[\r\n])))*-+\})/"
  )
  // // https://agda.readthedocs.io/en/v2.6.1/language/lexical-structure.html#keywords-and-special-symbols
  // let specialSymbol = [%re "/[\.\;\{\}\(\)\@\"]/"];

  // for regular holes: {! content !}
  let goalBracket = %re("/(\{\!(?:(?!\!\})(?:.|\s))*\!\})/")
  // for question marks (with specialSymbols around), e.g.:
  //    (?)
  //    .?@
  let goalQuestionMarkRaw = %re("/([\s\(\{\_\;\.\\\"@]|^)(\?)([\s\)\}\_\;\.\\\"@]|$)/gm")
  // for question marks: ?
  let goalQuestionMark = %re("/(\?)/")
  // for content inside {! !}
  let goalBracketContent = %re("/\{\!((?:(?!\!\})(?:.|\s))*)\!\}/")
}

module Literate = {
  // split a single string into tokens (Literate)
  let toTokens = (raw: string): Lexer.t => {
    let cursor = ref(0)
    Js.String.match_(
      %re("/(.*(?:\r\n|[\n\v\f\r\x85\u2028\u2029])?)/g"),
      raw,
    )->Option.mapWithDefault([], lines =>
      lines
      ->Array.map(x =>
        switch x {
        | None => ""
        | Some(s) => s
        }
      )
      ->Array.keep(s => s != "")
      ->Array.map(line => {
        // [\s\.\;\{\}\(\)\@]
        let cursorOld = cursor.contents
        cursor := cursor.contents + String.length(line)
        open Token
        {
          content: Js.String.substring(~from=cursorOld, ~to_=cursor.contents, raw),
          range: (cursorOld, cursor.contents),
          kind: Literate,
        }
      })
    )
  }

  // find and mark some tokens as AgdaRaw/Literate
  let markWithRules = (begin_, end_, raw) => {
    let previous = ref(false)
    let current = ref(false)
    raw
    ->toTokens
    ->Array.map(token => {
      open Token
      let {content, range} = token

      // update the previous line
      previous := current.contents

      if Js.Re.test_(begin_, content) && !current.contents {
        // entering Agda code
        current := true
      } else if Js.Re.test_(end_, content) && current.contents {
        // leaving Agda code
        current := false
      }

      // to prevent the beginning line (e.g. "\begin{code}") get treated as "insideAgda"
      let insideAgda = previous.contents && current.contents

      let kind = insideAgda ? AgdaRaw : Literate

      {content, kind, range}
    })
  }

  let markMarkdown = markWithRules(Regex.markdown, Regex.markdown, ...)
  let markTex = markWithRules(Regex.texBegin, Regex.texEnd, ...)
  let markRST = markWithRules(Regex.rstBegin, Regex.rstEnd, ...)
  let markOrg = markWithRules(Regex.orgBegin, Regex.orgEnd, ...)
}

module Diff = {
  type t = {
    index: int,
    modifiedInterval: Interval.t,
    originalInterval: Interval.t,
    content: string,
    changed: bool,
  }

  let toString = ({index, modifiedInterval, originalInterval, content}) =>
    "Hole [" ++
    (string_of_int(index) ++
    ("] (" ++
    (string_of_int(fst(originalInterval)) ++
    (", " ++
    (string_of_int(snd(originalInterval)) ++
    (") => (" ++
    (string_of_int(fst(modifiedInterval)) ++
    (", " ++ (string_of_int(snd(modifiedInterval)) ++ (") \"" ++ (content ++ "\"")))))))))))
}

let parse = (indices: array<int>, filepath: string, raw: string): array<Diff.t> => {
  open Token
  // counter for indices
  let i = ref(0)
  // processed literate Agda
  let fileType = FileType.parse(filepath)
  let preprocessed = switch fileType {
  | LiterateTeX => Literate.markTex(raw)
  | LiterateMarkdown => Literate.markMarkdown(raw)
  | LiterateRST => Literate.markRST(raw)
  | LiterateOrg => Literate.markOrg(raw)
  | Agda => Lexer.make(raw)
  }
  /* just lexing, doesn't mess around with raw text, preserves positions */
  let original =
    preprocessed
    |> Lexer.lex(Regex.comment, AgdaRaw, Comment)
    |> Lexer.lex(Regex.goalBracket, AgdaRaw, GoalBracket)
    |> Lexer.lex(Regex.goalQuestionMarkRaw, AgdaRaw, GoalQMRaw)
    |> Lexer.lex(Regex.goalQuestionMark, GoalQMRaw, GoalQM)
  let questionMark2GoalBracket = token => {
    /* ? => {!  !} */

    content: "{!   !}",
    range: token.range,
    kind: GoalBracket,
  }
  let adjustGoalBracket = (token: Token.t) => {
    /* {!!} => {!   !} */

    /* in case that the goal index wasn't given, make it '*' */
    /* this happens when splitting case, agda2-goals-action is one index short */
    let goalIndex = switch indices[i.contents] {
    | Some(idx) => string_of_int(idx)
    | None => "*"
    }

    /* {! zero 42!}
         <------>    hole content
               <>    index
              <->    space for index
 */

    /* calculate how much space the index would take */
    let requiredSpaces = String.length(goalIndex)

    /* calculate how much space we have */
    let content: string =
      Js.Re.exec_(Regex.goalBracketContent, token.content)
      ->Option.flatMap(result =>
        Js.Re.captures(result)[1]->Option.map(Js.Nullable.toOption)->Option.flatMap(x => x)
      )
      ->Option.getWithDefault("")
    let actualSpaces =
      content
      ->Js.String.match_(%re("/\s*$/"), _)
      ->Option.flatMap(matches =>
        switch matches[0] {
        | None => None
        | Some(None) => None
        | Some(Some(s)) => Some(Js.String.length(s))
        }
      )
      ->Option.getWithDefault(0)

    /* make room for the index, if there's not enough space */
    let newContent = if actualSpaces < requiredSpaces {
      let padding = Js.String.repeat(requiredSpaces - actualSpaces, "")

      Js.String.replaceByRe(%re("/\{!.*!\}/"), "{!" ++ content ++ padding ++ "!}", token.content)
    } else {
      token.content
    }

    /* update the index */
    i := i.contents + 1
    {content: newContent, kind: GoalBracket, range: (1, 2)}
  }

  let modified =
    original
    |> Lexer.mapOnly(GoalQM, questionMark2GoalBracket)
    |> Lexer.mapOnly(GoalBracket, adjustGoalBracket)
  let originalHoles = original->Array.keep(isHole)
  let modifiedHoles = modified->Array.keep(isHole)

  originalHoles
  ->Array.mapWithIndex((idx, token: Token.t) =>
    switch (modifiedHoles[idx], indices[idx]) {
    | (Some(modifiedHole), Some(index)) =>
      let (start, _) = modifiedHole.range
      Some({
        Diff.index,
        originalInterval: (start, start + String.length(token.content)),
        modifiedInterval: modifiedHole.range,
        content: modifiedHole.content,
        changed: token.content != modifiedHole.content,
      })
    | _ => None
    }
  )
  ->Array.keepMap(x => x)
}
