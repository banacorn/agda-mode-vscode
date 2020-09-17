open Belt;

module FileType = {
  type t =
    | Agda
    | LiterateTeX
    | LiterateRST
    | LiterateMarkdown
    | LiterateOrg;
  let parse = filepath =>
    if (Js.Re.test_([%re "/\\.lagda\\.rst$/i"], Parser.filepath(filepath))) {
      LiterateRST;
    } else if (Js.Re.test_(
                 [%re "/\\.lagda\\.md$/i"],
                 Parser.filepath(filepath),
               )) {
      LiterateMarkdown;
    } else if (Js.Re.test_(
                 [%re "/\\.lagda\\.tex$|\\.lagda$/i"],
                 Parser.filepath(filepath),
               )) {
      LiterateTeX;
    } else if (Js.Re.test_(
                 [%re "/\\.lagda\\.org$/i"],
                 Parser.filepath(filepath),
               )) {
      LiterateOrg;
    } else {
      Agda;
    };
};

module Token = {
  type kind =
    | AgdaRaw
    | Literate
    | Comment
    | GoalBracket
    /* QM: Question marks  */
    | GoalQMRaw
    | GoalQM;
  type t = {
    content: string,
    range: (int, int),
    kind,
  };

  let isHole = token =>
    switch (token.kind) {
    | GoalBracket
    | GoalQM => true
    | _ => false
    };
};

module Lexer = {
  type t = array(Token.t);
  // return
  let make = (raw: string): t => {
    [|{content: raw, range: (0, String.length(raw)), kind: AgdaRaw}|];
  };
  // bind >==
  /* break tokens down into smaller pieces

     regex     : regex to perform split on a token
     sourceType: the type of token to look for and perform splitting
     targetType: the type of token given to the splitted tokens when identified */
  let lex =
      (regex: Js.Re.t, source: Token.kind, target: Token.kind, tokens): t => {
    let f = (token: Token.t) =>
      if (token.kind === source) {
        let cursor = ref(fst(token.range));
        token.content
        ->Js.String.splitByRe(regex, _)
        ->Array.keepMap(x => x)
        ->Array.map(content => {
            let kind = Js.Re.test_(regex, content) ? target : source;
            let cursorOld = cursor^;
            cursor := cursor^ + String.length(content);
            Token.{content, range: (cursorOld, cursor^), kind};
          });
      } else {
        [|token|];
      };
    tokens->Array.map(f)->Array.concatMany;
  };

  // transforms a list of tokens while preserving the ranges
  let map = (f: Token.t => Token.t, self): t => {
    let delta = ref(0);
    self->Array.map(token => {
      let Token.{content, kind} = f(token);
      let (start, end_) = token.range;
      let lengthDiff = String.length(content) - String.length(token.content);
      let result =
        Token.{
          content,
          range: (start + delta^, end_ + delta^ + lengthDiff),
          kind,
        };
      delta := delta^ + lengthDiff;
      result;
    });
  };

  // only apply map(f) on a specific tokenType
  let mapOnly = (kind: Token.kind, f: Token.t => Token.t, self): t => {
    self |> map(token => token.kind === kind ? f(token) : token);
  };
};

module Regex = {
  let texBegin = [%re "/\\\\begin\\{code\\}.*/"];
  let texEnd = [%re "/\\\\end\\{code\\}.*/"];
  let markdown = [%re "/\\`\\`\\`(agda)?/"];
  let rstBegin = [%re "/\\:\\:/"];
  let rstEnd = [%re "/^[^\\s]/"];
  let orgBegin = [%re "/\\#\\+begin\\_src agda2/i"];
  let orgEnd = [%re "/\\#\\+end\\_src/i"];

  let comment = [%re
    "/(--[^\\r\\n]*[\\r\\n])|(\\{-(?:[^-]|[\\r\\n]|(?:-+(?:[^-\\}]|[\\r\\n])))*-+\\})/"
  ];

  // // https://agda.readthedocs.io/en/v2.6.1/language/lexical-structure.html#keywords-and-special-symbols
  // let specialSymbol = [%re "/[\.\;\{\}\(\)\@\"]/"];

  let goalBracket = [%re "/(\\{\\!(?:(?!\\!\\})(?:.|\\s))*\\!\\})/"];
  let goalQuestionMarkRaw = [%re
    "/([\\s\\(\\{\\_\\;\\.\\\"@]|^)(\\?)([\\s\\)\\}\\_\\;\\.\\\"@]|$)/gm"
  ];
  let goalQuestionMark = [%re "/(\\?)/"];
  let goalBracketContent = [%re "/\\{\\!((?:(?!\\!\\})(?:.|\\s))*)\\!\\}/"];
};

module Literate = {
  // split a single string into tokens (Literate)
  let toTokens = (raw: string): Lexer.t => {
    let cursor = ref(0);
    Js.String.match(
      [%re "/(.*(?:\\r\\n|[\\n\\v\\f\\r\\x85\\u2028\\u2029])?)/g"],
      raw,
    )
    // [\s\.\;\{\}\(\)\@]
    ->Option.mapWithDefault([||], lines =>
        lines
        ->Array.keep(x => x != "")
        ->Array.map(line => {
            let cursorOld = cursor^;
            cursor := cursor^ + String.length(line);
            Token.{
              content:
                Js.String.substring(~from=cursorOld, ~to_=cursor^, raw),
              range: (cursorOld, cursor^),
              kind: Literate,
            };
          })
      );
  };

  // find and mark some tokens as AgdaRaw/Literate
  let markWithRules = (begin_, end_, raw) => {
    let previous = ref(false);
    let current = ref(false);
    raw
    ->toTokens
    ->Array.map(token => {
        open Token;
        let {content, range} = token;

        // update the previous line
        previous := current^;

        if (Js.Re.test_(begin_, content) && ! current^) {
          // entering Agda code
          current := true;
        } else if (Js.Re.test_(end_, content) && current^) {
          // leaving Agda code
          current := false;
        };

        // to prevent the beginning line (e.g. "\begin{code}") get treated as "insideAgda"
        let insideAgda = previous^ && current^;

        let kind = insideAgda ? AgdaRaw : Literate;

        {content, kind, range};
      });
  };

  let markMarkdown = markWithRules(Regex.markdown, Regex.markdown);
  let markTex = markWithRules(Regex.texBegin, Regex.texEnd);
  let markRST = markWithRules(Regex.rstBegin, Regex.rstEnd);
  let markOrg = markWithRules(Regex.orgBegin, Regex.orgEnd);
};

module Diff = {
  type t = {
    index: int,
    modifiedRange: (int, int),
    originalRange: (int, int),
    content: string,
    changed: bool,
  };

  let toString = ({index, modifiedRange, originalRange, content}) => {
    "Hole ["
    ++ string_of_int(index)
    ++ "] ("
    ++ string_of_int(fst(originalRange))
    ++ ", "
    ++ string_of_int(snd(originalRange))
    ++ ") => ("
    ++ string_of_int(fst(modifiedRange))
    ++ ", "
    ++ string_of_int(snd(modifiedRange))
    ++ ") \""
    ++ content
    ++ "\"";
  };
};

let parse =
    (indices: array(int), filepath: string, raw: string): array(Diff.t) => {
  open Token;
  // counter for indices
  let i = ref(0);
  // processed literate Agda
  let fileType = FileType.parse(filepath);
  let preprocessed =
    switch (fileType) {
    | LiterateTeX => Literate.markTex(raw)
    | LiterateMarkdown => Literate.markMarkdown(raw)
    | LiterateRST => Literate.markRST(raw)
    | LiterateOrg => Literate.markOrg(raw)
    | Agda => Lexer.make(raw)
    };
  /* just lexing, doesn't mess around with raw text, preserves positions */
  let original =
    preprocessed
    |> Lexer.lex(Regex.comment, AgdaRaw, Comment)
    |> Lexer.lex(Regex.goalBracket, AgdaRaw, GoalBracket)
    |> Lexer.lex(Regex.goalQuestionMarkRaw, AgdaRaw, GoalQMRaw)
    |> Lexer.lex(Regex.goalQuestionMark, GoalQMRaw, GoalQM);
  let questionMark2GoalBracket = token => {
    /* ? => {!  !} */

    content: "{!   !}",
    range: token.range,
    kind: GoalBracket,
  };
  let adjustGoalBracket = (token: Token.t) => {
    /* {!!} => {!   !} */

    /* in case that the goal index wasn't given, make it '*' */
    /* this happens when splitting case, agda2-goals-action is one index short */
    let goalIndex =
      switch (indices[i^]) {
      | Some(idx) => string_of_int(idx)
      | None => "*"
      };

    /* {! zero 42!}
         <------>    hole content
               <>    index
              <->    space for index
       */

    /* calculate how much space the index would take */
    let requiredSpaces = String.length(goalIndex);

    /* calculate how much space we have */
    let content: string =
      Js.Re.exec_(Regex.goalBracketContent, token.content)
      ->Option.flatMap(result =>
          Js.Re.captures(result)[1]
          ->Option.map(Js.Nullable.toOption)
          ->Option.flatMap(x => x)
        )
      ->Option.getWithDefault("");
    let actualSpaces =
      content
      ->Js.String.match([%re "/\\s*$/"], _)
      ->Option.flatMap(matches => matches[0]->Option.map(Js.String.length))
      ->Option.getWithDefault(0);

    /* make room for the index, if there's not enough space */
    let newContent =
      if (actualSpaces < requiredSpaces) {
        let padding = Js.String.repeat(requiredSpaces - actualSpaces, "");

        Js.String.replaceByRe(
          [%re "/\\{!.*!\\}/"],
          "{!" ++ content ++ padding ++ "!}",
          token.content,
        );
      } else {
        token.content;
      };

    /* update the index */
    i := i^ + 1;
    {content: newContent, kind: GoalBracket, range: (1, 2)};
  };

  let modified =
    original
    |> Lexer.mapOnly(GoalQM, questionMark2GoalBracket)
    |> Lexer.mapOnly(GoalBracket, adjustGoalBracket);
  let originalHoles = original->Array.keep(isHole);
  let modifiedHoles = modified->Array.keep(isHole);

  originalHoles
  ->Array.mapWithIndex((idx, token: Token.t) =>
      switch (modifiedHoles[idx], indices[idx]) {
      | (Some(modifiedHole), Some(index)) =>
        let (start, _) = modifiedHole.range;
        Some({
          Diff.index,
          originalRange: (start, start + String.length(token.content)),
          modifiedRange: modifiedHole.range,
          content: modifiedHole.content,
          changed: token.content != modifiedHole.content,
        });
      | _ => None
      }
    )
  ->Array.keepMap(x => x);
};
