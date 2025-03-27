// All RegExps in this file has been updated to work with ReScript v10.1.4

open Common

module FileType = {
  type t =
    | Agda
    | LiterateTeX
    | LiterateRST
    | LiterateMarkdown
    | LiterateTypst
    | LiterateOrg
    | LiterateForester
  let parse = filepath =>
    if RegExp.test(%re("/\.lagda\.rst$/i"), Parser.filepath(filepath)) {
      LiterateRST
    } else if RegExp.test(%re("/\.lagda\.md$/i"), Parser.filepath(filepath)) {
      LiterateMarkdown
    } else if RegExp.test(%re("/\.lagda\.typ$/i"), Parser.filepath(filepath)) {
      LiterateTypst
    } else if RegExp.test(%re("/\.lagda\.tex$|\.lagda$/i"), Parser.filepath(filepath)) {
      LiterateTeX
    } else if RegExp.test(%re("/\.lagda\.org$/i"), Parser.filepath(filepath)) {
      LiterateOrg
    } else if RegExp.test(%re("/\.lagda\.tree$/i"), Parser.filepath(filepath)) {
      LiterateForester
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

  let toString = token => {
    let {content, range, kind} = token
    let (start, end_) = range
    let kindStr = switch kind {
    | AgdaRaw => "AgdaRaw"
    | Literate => "Literate"
    | Comment => "Comment"
    | GoalBracket => "GoalBracket"
    | GoalQMRaw => "GoalQMRaw"
    | GoalQM => "GoalQM"
    }
    kindStr ++
    " [" ++
    (string_of_int(start) ++
    (", " ++ (string_of_int(end_) ++ ("] \"" ++ content ++ "\""))))
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
  let lex = (tokens, regex: RegExp.t, source: Token.kind, target: Token.kind): t => {
    let f = (token: Token.t) =>
      if token.kind === source {
        let cursor = ref(fst(token.range))
        token.content
        ->String.splitByRegExp(regex)
        ->Array.filterMap(x => x)
        ->Array.map(content => {
          let kind = RegExp.test(regex, content) ? target : source
          let cursorOld = cursor.contents
          cursor := cursor.contents + String.length(content)
          open Token
          {content, range: (cursorOld, cursor.contents), kind}
        })
      } else {
        [token]
      }
    tokens->Array.map(f)->Array.flat
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
    map(token => token.kind === kind ? f(token) : token, self)
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
  let foresterBegin = %re("/\\agda\{/")
  let foresterEnd = %re("/^\s*\}/")

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
    raw
    ->String.match(%re("/(.*(?:\r\n|[\n\v\f\r\x85\u2028\u2029])?)/g"))
    ->Option.mapOr([], lines =>
      lines
      ->Array.map(x =>
        switch x {
        | None => ""
        | Some(s) => s
        }
      )
      ->Array.filter(s => s != "")
      ->Array.map(line => {
        // [\s\.\;\{\}\(\)\@]
        let cursorOld = cursor.contents
        cursor := cursor.contents + String.length(line)
        open Token
        {
          content: String.substring(~start=cursorOld, ~end=cursor.contents, raw),
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

      if RegExp.test(begin_, content) && !current.contents {
        // entering Agda code
        current := true
      } else if RegExp.test(end_, content) && current.contents {
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
  let markTypst = markWithRules(Regex.markdown, Regex.markdown, ...)
  let markTex = markWithRules(Regex.texBegin, Regex.texEnd, ...)
  let markRST = markWithRules(Regex.rstBegin, Regex.rstEnd, ...)
  let markOrg = markWithRules(Regex.orgBegin, Regex.orgEnd, ...)
  let markForester = markWithRules(Regex.foresterBegin, Regex.foresterEnd, ...)
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
  // processed literate Agda
  let fileType = FileType.parse(filepath)
  let preprocessed = switch fileType {
  | LiterateTeX => Literate.markTex(raw)
  | LiterateMarkdown => Literate.markMarkdown(raw)
  | LiterateTypst => Literate.markTypst(raw)
  | LiterateRST => Literate.markRST(raw)
  | LiterateOrg => Literate.markOrg(raw)
  | LiterateForester => Literate.markForester(raw)
  | Agda => Lexer.make(raw)
  }
  // just lexing, doesn't mess around with raw text, preserves positions
  let original =
    preprocessed
    ->Lexer.lex(Regex.comment, AgdaRaw, Comment)
    ->Lexer.lex(Regex.goalBracket, AgdaRaw, GoalBracket)
    ->Lexer.lex(Regex.goalQuestionMarkRaw, AgdaRaw, GoalQMRaw)
    ->Lexer.lex(Regex.goalQuestionMark, GoalQMRaw, GoalQM)

  let questionMark2GoalBracket = token => {
    /* ? => {!  !} */

    content: "{!   !}",
    range: token.range,
    kind: GoalBracket,
  }

  let modified = Lexer.mapOnly(GoalQM, questionMark2GoalBracket, original)
  let originalHoles = original->Array.filter(isHole)
  let modifiedHoles = modified->Array.filter(isHole)

  originalHoles
  ->Array.mapWithIndex((token: Token.t, idx: int) =>
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
  ->Array.filterMap(x => x)
}
