// External representation of a goal in the editor
module type Module = {
  type t = {
    index: int,
    indexString: string,
    start: int,
    end: int,
  }
  let toString: t => string

  let getContent: (t, VSCode.TextDocument.t) => string

  let indentationWidth: (t, VSCode.TextDocument.t) => (int, string, VSCode.Range.t)
  // helper function for building Haskell Agda ranges
  let makeHaskellRange: (t, VSCode.TextDocument.t, string, string) => string

  // for case splitting
  let replaceWithLines: (
    t,
    VSCode.TextEditor.t,
    array<string>,
  ) => promise<option<(VSCode.Range.t, string)>>
  let replaceWithLambda: (
    t,
    VSCode.TextEditor.t,
    array<string>,
  ) => promise<option<(VSCode.Range.t, string)>>
  let placeCursorAtFirstNewGoal: (VSCode.TextEditor.t, VSCode.Range.t, string) => unit

  // for property-based testing
  let arbitraryBatch: (~size: int=?, ~after: int=?) => FastCheck.Arbitrary.arbitrary<array<t>>
}

module Module: Module = {
  type t = {
    index: int,
    indexString: string, // for serialization
    start: int,
    end: int,
  }

  let toString = goal =>
    switch VSCode.Window.activeTextEditor {
    | Some(editor) =>
      let document = VSCode.TextEditor.document(editor)
      let startPoint = VSCode.TextDocument.positionAt(document, goal.start)
      let endPoint = VSCode.TextDocument.positionAt(document, goal.end)
      let startLine = VSCode.Position.line(startPoint) + 1
      let startColumn = VSCode.Position.character(startPoint) + 1
      let endLine = VSCode.Position.line(endPoint) + 1
      let endColumn = VSCode.Position.character(endPoint) + 1
      if startLine == endLine {
        `#${string_of_int(goal.index)} [${string_of_int(startLine)}:${string_of_int(
            startColumn,
          )}-${string_of_int(endColumn)})`
      } else {
        `#${string_of_int(goal.index)} [${string_of_int(startLine)}:${string_of_int(
            startColumn,
          )}-${string_of_int(endLine)}:${string_of_int(endColumn)})`
      }
    | None =>
      `#${string_of_int(goal.index)} offset [${string_of_int(goal.start)}-${string_of_int(
          goal.end,
        )})`
    }

  let makeHaskellRange = (goalInfo, document, version, filepath) => {
    let startPoint = VSCode.TextDocument.positionAt(document, goalInfo.start)
    let endPoint = VSCode.TextDocument.positionAt(document, goalInfo.end)

    let startIndex = string_of_int(goalInfo.start + 3)
    let startRow = string_of_int(VSCode.Position.line(startPoint) + 1)
    let startColumn = string_of_int(VSCode.Position.character(startPoint) + 3)
    let startPart = `${startIndex} ${startRow} ${startColumn}`
    let endIndex' = string_of_int(goalInfo.end - 3)
    let endRow = string_of_int(VSCode.Position.line(endPoint) + 1)
    let endColumn = string_of_int(VSCode.Position.character(endPoint) - 1)
    let endPart = `${endIndex'} ${endRow} ${endColumn}`

    if Util.Version.gte(version, "2.8.0") {
      `(intervalsToRange (Just (mkAbsolute "${filepath}")) [Interval () (Pn () ${startPart}) (Pn () ${endPart})])` // after 2.8.0
    } else if Util.Version.gte(version, "2.5.1") {
      `(intervalsToRange (Just (mkAbsolute "${filepath}")) [Interval (Pn () ${startPart}) (Pn () ${endPart})])` // after 2.5.1, before (not including) 2.8.0
    } else {
      `(Range [Interval (Pn (Just (mkAbsolute "${filepath}")) ${startPart}) (Pn (Just (mkAbsolute "${filepath}")) ${endPart})])` // before (not including) 2.5.1
    }
  }

  let getContent = (goal, document) => {
    let innerRange = VSCode.Range.make(
      VSCode.TextDocument.positionAt(document, goal.start + 2),
      VSCode.TextDocument.positionAt(document, goal.end - 2),
    )
    Editor.Text.get(document, innerRange)->String.trim
  }

  // returns the width of indentation of the first line of a goal
  // along with the text and the range before the goal
  let indentationWidth = (goal, document) => {
    let goalStart = document->VSCode.TextDocument.positionAt(goal.start)
    let lineNo = VSCode.Position.line(goalStart)
    let range = VSCode.Range.make(VSCode.Position.make(lineNo, 0), goalStart)
    let textBeforeGoal = Editor.Text.get(document, range)
    // tally the number of blank characters
    // ' ', '\012', '\n', '\r', and '\t'
    let indentedBy = s => {
      let n = ref(0)
      for i in 0 to String.length(s) - 1 {
        switch String.charAt(s, i) {
        | " "
        | ""
        | "
"
        | "	" =>
          if i == n.contents {
            n := n.contents + 1
          }
        | _ => ()
        }
      }
      n.contents
    }
    (indentedBy(textBeforeGoal), textBeforeGoal, range)
  }

  // There are two kinds of lambda abstraction
  //  1.  λ { x → ? }
  //  2.  λ where x → ?
  // We employ the strategy implemented by the Emacs mode
  // https://github.com/agda/agda/blob/f46ecaf729c00217efad7a77e5d9932bfdd030e5/src/data/emacs-mode/agda2-mode.el#L950
  // which searches backward (starting from the goal) and see if there's a open curly bracket "{"

  //      λ { x → ? }
  //        ^------------------- open curly bracket

  //  or if there's a "where" token

  //      λ where x → ?
  //        ^------------------- the "where" token

  let caseSplitAux = (goal, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let textBeforeGoal = {
      let range = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, 0),
        VSCode.TextDocument.positionAt(document, goal.start),
      )
      Editor.Text.get(document, range)
    }

    // returns the offset of the first non-blank character it encountered from somewhere
    let nextWordBoundary = (start, string) => {
      let break = ref(false)
      let n = ref(0)

      let i = ref(start)
      while i.contents < String.length(string) && !break.contents {
        let char = string->String.charAt(i.contents)
        switch char {
        // skip blank characters
        | " "
        | ""
        | "	" =>
          n := n.contents + 1
        // stop when we hit something
        | _ => break := true
        }
        i := i.contents + 1
      }
      start + n.contents
    }

    let (inWhereClause, searchStart, lastLineBreakOffset) = {
      let lastOpenCurlyBracketOffset =
        {
          let bracketCount = ref(0)
          let i = ref(goal.start - 1)
          while i.contents >= 0 && bracketCount.contents >= 0 {
            switch i.contents {
            // no preceding character
            | 0 => ()
            // has preceding character
            | i' =>
              switch textBeforeGoal->String.charAt(i' - 1) {
              | "}" => bracketCount := bracketCount.contents + 1
              | "{" => bracketCount := bracketCount.contents - 1
              | _ => ()
              }
            }
            // scanning backwards
            i := i.contents - 1
          }
          i.contents
        } + 1

      let lastSemicolonOffset =
        switch textBeforeGoal->String.lastIndexOf(";") {
        | -1 => 0
        | n => n
        } + 1

      let lastWhereTokenOffset =
        switch textBeforeGoal->String.lastIndexOf("where") {
        | -1 => 0
        | n => n
        } + 5

      let lastLineBreakOffset =
        max(
          0,
          max(textBeforeGoal->String.lastIndexOf("\r"), textBeforeGoal->String.lastIndexOf("\n")),
        ) + 1

      let inWhereClause = lastWhereTokenOffset > lastOpenCurlyBracketOffset
      let offset = max(
        max(lastLineBreakOffset, lastSemicolonOffset),
        max(lastWhereTokenOffset, lastOpenCurlyBracketOffset),
      )
      (inWhereClause, offset, lastLineBreakOffset)
    }

    // returns the range of the case clause (that is going to be replaced)
    //    "x -> {!   !}"
    let caseStart = nextWordBoundary(searchStart, textBeforeGoal)
    let caseEnd = goal.end
    let range = {
      VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, caseStart),
        VSCode.TextDocument.positionAt(document, caseEnd),
      )
    }
    (inWhereClause, caseStart - lastLineBreakOffset, range)
  }

  // replace and insert one or more lines of content at the goal
  // usage: case split
  let replaceWithLines = async (goal, editor, lines) => {
    // get the width of indentation from the first line of the goal
    let document = VSCode.TextEditor.document(editor)
    let (indentWidth, _, _) = indentationWidth(goal, document)
    let indentation = String.repeat(" ", indentWidth)
    let indentedLines = indentation ++ lines->Array.join("\n" ++ indentation)
    // the rows spanned by the goal (including the text outside the goal)
    // will be replaced by the `indentedLines`
    let start = VSCode.TextDocument.positionAt(document, goal.start)
    let startLineNo = VSCode.Position.line(start)
    let startLineRange = document->VSCode.TextDocument.lineAt(startLineNo)->VSCode.TextLine.range
    let start = VSCode.Range.start(startLineRange)

    let end_ = VSCode.TextDocument.positionAt(document, goal.end)
    let rangeToBeReplaced = VSCode.Range.make(start, end_)
    if await Editor.Text.replace(editor, rangeToBeReplaced, indentedLines) {
      Some(rangeToBeReplaced, indentedLines)
    } else {
      None
    }
  }

  // Replace definition of extended lambda with new clauses

  // We are asked to replace a clause like "x → ?" with multiple clauese
  // There are two kinds of syntax for extended lambda
  //  1.  λ { x → ?
  //        ; y → ?
  //        }
  //  2.  λ where
  //          x → ?
  //          y → ?
  let replaceWithLambda = async (goal, editor, lines) => {
    let (inWhereClause, indentWidth, rewriteRange) = caseSplitAux(goal, editor)
    let rewriteText = if inWhereClause {
      lines->Array.join("\n" ++ String.repeat(" ", indentWidth))
    } else {
      lines->Array.join("\n" ++ (String.repeat(" ", indentWidth - 2) ++ "; "))
    }
    if await Editor.Text.replace(editor, rewriteRange, rewriteText) {
      Some(rewriteRange, rewriteText)
    } else {
      None
    }
  }

  let placeCursorAtFirstNewGoal = (editor, rewriteRange, rewriteText) => {
    // locate the first new goal and place the cursor there
    let splittedLines = Parser.splitToLines(rewriteText)
    splittedLines[0]->Option.forEach(line => {
      let col = String.length(line) - 1
      let lastChar = String.charAt(line, col)
      if lastChar == "?" {
        let position = VSCode.Position.translate(VSCode.Range.start(rewriteRange), 0, col)
        Editor.Cursor.set(editor, position)
      }
    })
  }

  open FastCheck.Arbitrary

  // Given an offset, generates a Goal's range
  let arbitraryInterval = (after): arbitrary<(int, int)> => {
    integerRange(after, after + 10)->Derive.chain(start => {
      integerRange(start + 4, start + 10)->Derive.map(end => (start, end))
    })
  }

  let arbitraryBatch = (~size=10, ~after=0): arbitrary<array<t>> => {
    let rec aux = ((acc, after), index) =>
      if index >= size {
        Combinators.constant((acc, after))
      } else {
        arbitraryInterval(after)->Derive.chain(((start, end)) => {
          let goal = {
            index,
            indexString: string_of_int(index),
            start,
            end,
          }
          aux(([...acc, goal], end), index + 1)
        })
      }
    aux(([], after), 0)->Derive.map(fst)
  }
}
include Module
