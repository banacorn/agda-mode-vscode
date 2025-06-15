open Common
module type Module = {
  // let instantiate: (State.t, array<int>) => promise<unit>
  // let instantiateGoals: (
  //   State.t,
  //   VSCode.TextEditor.t,
  //   Map.t<int, int>,
  //   array<int>,
  // ) => promise<array<Goal.t>>
  // let modify: (State.t, Goal.t, string => string) => promise<unit>
  // let removeBoundaryAndDestroy: (State.t, Goal.t) => promise<unit>
  let pointed: State.t => option<(Goal.t, string)>
  // let replaceWithLines: (State.t, Goal.t, array<string>) => promise<unit>
  // let replaceWithLambda: (State.t, Goal.t, array<string>) => promise<unit>
  // let indentationWidth: (VSCode.TextDocument.t, Goal.t) => (int, string, VSCode.Range.t)
  // let caseSplitAux: (VSCode.TextDocument.t, AgdaModeVscode.Goal.t) => (bool, int, Interval.t)

  let redecorate: State.t => unit
  // let next: State.t => unit
  // let previous: State.t => unit

  // helper functions
  // let parseHolesFromRefineResult: string => array<int>

  // let instantiateGoalsFromQuestionMarks: (
  //   State.t,
  //   array<int>,
  // ) => promise<Map.t<int, int>>
}

module Module: Module = {
  // return an array of Offsets of Goals
  // let getOffsets = (state: State.t): array<int> =>
  //   state.goals->Array.map(goal => {
  //     let (from, to) = goal.interval
  //     from + min(3, (to - from) / 2)
  //   })

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

  // let caseSplitAux = (document, goal: Goal.t) => {
  //   let textBeforeGoal = {
  //     let interval = (0, fst(goal.interval))
  //     let range = Interval.toVSCodeRange(document, interval)
  //     Editor.Text.get(document, range)
  //   }

  //   // returns the offset of the first non-blank character it encountered from somewhere
  //   let nextWordBoundary = (start, string) => {
  //     let break = ref(false)
  //     let n = ref(0)

  //     let i = ref(start)
  //     while i.contents < String.length(string) && !break.contents {
  //       let char = string->String.charAt(i.contents)
  //       switch char {
  //       // skip blank characters
  //       | " "
  //       | ""
  //       | "	" =>
  //         n := n.contents + 1
  //       // stop when we hit something
  //       | _ => break := true
  //       }
  //       i := i.contents + 1
  //     }
  //     start + n.contents
  //   }

  //   let (inWhereClause, searchStart, lastLineBreakOffset) = {
  //     let lastOpenCurlyBracketOffset =
  //       {
  //         let bracketCount = ref(0)
  //         let i = ref(fst(goal.interval) - 1)
  //         while i.contents >= 0 && bracketCount.contents >= 0 {
  //           switch i.contents {
  //           // no preceding character
  //           | 0 => ()
  //           // has preceding character
  //           | i' =>
  //             switch textBeforeGoal->String.charAt(i' - 1) {
  //             | "}" => bracketCount := bracketCount.contents + 1
  //             | "{" => bracketCount := bracketCount.contents - 1
  //             | _ => ()
  //             }
  //           }
  //           // scanning backwards
  //           i := i.contents - 1
  //         }
  //         i.contents
  //       } + 1

  //     let lastSemicolonOffset =
  //       switch textBeforeGoal->String.lastIndexOf(";") {
  //       | -1 => 0
  //       | n => n
  //       } + 1

  //     let lastWhereTokenOffset =
  //       switch textBeforeGoal->String.lastIndexOf("where") {
  //       | -1 => 0
  //       | n => n
  //       } + 5

  //     let lastLineBreakOffset =
  //       max(
  //         0,
  //         max(textBeforeGoal->String.lastIndexOf("\r"), textBeforeGoal->String.lastIndexOf("\n")),
  //       ) + 1

  //     let inWhereClause = lastWhereTokenOffset > lastOpenCurlyBracketOffset
  //     let offset = max(
  //       max(lastLineBreakOffset, lastSemicolonOffset),
  //       max(lastWhereTokenOffset, lastOpenCurlyBracketOffset),
  //     )
  //     (inWhereClause, offset, lastLineBreakOffset)
  //   }

  //   // returns the range of the case clause (that is going to be replaced)
  //   //    "x -> {!   !}"
  //   let range = {
  //     let caseStart = nextWordBoundary(searchStart, textBeforeGoal)
  //     let caseEnd = snd(goal.interval)
  //     (caseStart, caseEnd)
  //   }
  //   (inWhereClause, fst(range) - lastLineBreakOffset, range)
  // }

//   // returns the width of indentation of the first line of a goal
//   // along with the text and the range before the goal
//   let indentationWidth = (document, goal: Goal.t): (int, string, VSCode.Range.t) => {
//     let goalStart = document->VSCode.TextDocument.positionAt(fst(goal.interval))
//     let lineNo = VSCode.Position.line(goalStart)
//     let range = VSCode.Range.make(VSCode.Position.make(lineNo, 0), goalStart)
//     let textBeforeGoal = Editor.Text.get(document, range)
//     // tally the number of blank characters
//     // ' ', '\012', '\n', '\r', and '\t'
//     let indentedBy = s => {
//       let n = ref(0)
//       for i in 0 to String.length(s) - 1 {
//         switch String.charAt(s, i) {
//         | " "
//         | ""
//         | "
// "
//         | "	" =>
//           if i == n.contents {
//             n := n.contents + 1
//           }
//         | _ => ()
//         }
//       }
//       n.contents
//     }
//     (indentedBy(textBeforeGoal), textBeforeGoal, range)
//   }

  // let jumpToOffset = (state: State.t, offset) => {
  //   let point = state.document->VSCode.TextDocument.positionAt(offset)
  //   let range = VSCode.Range.make(point, point)
  //   Editor.reveal(state.editor, range)
  // }

  // parse the whole source file and update the intervals of an array of Goal.t
  let updateIntervals = (state: State.t) => {
    let indices = state.goals->Array.map(goal => goal.index)
    let diffs = Goal.generateDiffs(state.document, indices)
    diffs->Array.forEachWithIndex((diff, i) =>
      switch state.goals[i] {
      | None => () // do nothing :|
      | Some(goal) => goal.interval = diff.modifiedInterval
      }
    )
  }

  // let modify = async (state: State.t, goal, f) => {
  //   updateIntervals(state)
  //   let content = Goal.getContent(goal, state.document)
  //   if await Goal.setContent(goal, state.document, f(content)) {
  //     updateIntervals(state)
  //   } else {
  //     await State__View.Panel.display(
  //       state,
  //       Error("Goal-related Error"),
  //       [Item.plainText("Failed to modify the content of goal #" ++ string_of_int(goal.index))],
  //     )
  //   }
  // }


  // Given offsets of holes, instantiate Goals and decorate them
  // rewrite question marks "?" to holes "{!   !}" when necessary
  // let instantiateHoles = async (state: State.t, editor: VSCode.TextEditor.t): Map.t<int, int> => {
  //   // acquire holes offsets from the tokens
  //   let holeOffsets = await state.tokens->Tokens.getHolesOffsets->Resource.get
  //   let holeOffsets =
  //     holeOffsets
  //     ->Map.entries
  //     ->Iterator.toArray

  //   // see if we need to rewrite question marks "?" to holes "{!   !}"
  //   let document = VSCode.TextEditor.document(editor)
  //   let rewrites = holeOffsets->Array.filterMap(((start, end)) => {
  //     let range = VSCode.Range.make(
  //       VSCode.TextDocument.positionAt(document, start),
  //       VSCode.TextDocument.positionAt(document, end),
  //     )
  //     let content = VSCode.TextDocument.getText(document, Some(range))
  //     if content == "?" {
  //       Some((range, "{!   !}"))
  //     } else {
  //       None
  //     }
  //   })
  //   let _ = await Editor.Text.batchReplace(document, rewrites)

  //   // return the new offsets after the rewrite
  //   // holeOffsets
  //   // ->Array.reduce(([], 0), ((acc, delta), (start, end)) => {
  //   //   ([...acc, (start + delta, end + delta + 6)], delta + 6)
  //   // })
  //   // ->fst
  //   // ->Map.fromArray
  // }

  // NOTE: holes should only be decorated when all question marks have been replaced as "{!   !}"
  // let decorateHoles = async (
  //   editor: VSCode.TextEditor.t,
  //   holeOffsets: Map.t<int, int>,
  //   indices: array<int>,
  // ): array<Goal.t> => {
  //   holeOffsets
  //   ->Map.entries
  //   ->Iterator.toArray
  //   ->Belt.Array.zip(indices)
  //   ->Array.map(((interval, index)) => {
  //     let (decorationBackground, decorationIndex) = Highlighting.decorateHole(
  //       editor,
  //       interval,
  //       index,
  //     )
  //     {
  //       Goal.index,
  //       interval,
  //       decorationBackground,
  //       decorationIndex,
  //     }
  //   })
  // }

  // let instantiateGoals = async (
  //   state: State.t,
  //   editor: VSCode.TextEditor.t,
  //   holeOffsets: Map.t<int, int>,
  //   indices: array<int>,
  // ): array<Goal.t> => {
  //   // let holeOffsets =
  //   //   holeOffsets
  //   //   ->Map.entries
  //   //   ->Iterator.toArray
  //   // Js.log2("Instantiating goals with offsets: ", holeOffsets)

  //   await decorateHoles(editor, holeOffsets, indices)
  // }

  // New holes may be introduced by a refine command, however, we don't have highlighting information
  // for the result of the refine command.
  // So we need to parse the holes from the refine result and decorate them ourself.
  // This function calculates the offsets of the question marks
  // let parseHolesFromRefineResult = raw => {
  //   let goalQuestionMark = %re("/([\s\(\{\_\;\.\\\"@]|^)(\?)([\s\)\}\_\;\.\\\"@]|$)/gm")
  //   // the chunks may contain:
  //   //   1. the question mark itself              (\?)
  //   //   2. the part before the question mark     ([\s\(\{\_\;\.\\\"@]|^)
  //   //   3. the part after the question mark      ([\s\)\}\_\;\.\\\"@]|$)
  //   //   4. other strings not matching the regex
  //   let chunks = raw->String.splitByRegExp(goalQuestionMark)

  //   chunks
  //   ->Array.reduce(([], 0), ((offsets, i), chunk) =>
  //     switch chunk {
  //     | None => (offsets, i)
  //     | Some(chunk) =>
  //       if chunk == "?" {
  //         let offset = i
  //         ([...offsets, offset], offset + 1)
  //       } else {
  //         // not a question mark, just append it to the string
  //         (offsets, i + String.length(chunk))
  //       }
  //     }
  //   )
  //   ->fst
  // }

  // let instantiate = async (state: State.t, indices) => {
  //   // // destroy all existing goals
  //   // state.goals->Array.forEach(Goal.destroyDecoration)

  //   // // get the cursor position before editing the text buffer
  //   // // so that we can place the cursor inside a goal later
  //   // let selection = VSCode.TextEditor.selection(state.editor)
  //   // let cursorStart = VSCode.TextDocument.offsetAt(
  //   //   state.document,
  //   //   VSCode.Selection.start(selection),
  //   // )
  //   // let cursorEnd = VSCode.TextDocument.offsetAt(state.document, VSCode.Selection.end_(selection))

  //   // // instantiate new ones
  //   // // let holeOffsets = await rewriteQuestionMarks(state, state.editor)
  //   // // let goals = await decorateHoles(state.editor, holeOffsets, indices)

  //   // let holeOffsets = await state.tokens->Tokens.getHolesOffsets->Resource.get
  //   // let goals = await instantiateGoals(state, state.editor, holeOffsets, indices)
  //   // goals->Array.forEach(goal => {
  //   //   // if there's a cursor that touches the hole's "boundary"
  //   //   //
  //   //   //                 {! some hole !}
  //   //   //                 ^^           ^^
  //   //   //
  //   //   //  move the cursor inside the hole
  //   //   //
  //   //   //                 {! some hole !}
  //   //   //                    ^
  //   //   //
  //   //   let (left, right) = goal.interval
  //   //   let touched =
  //   //     (left <= cursorStart && cursorStart <= left + 2) ||
  //   //     right - 2 <= cursorStart && cursorStart <= right ||
  //   //     left <= cursorEnd && cursorEnd <= left + 2 ||
  //   //     (right - 2 <= cursorEnd && cursorEnd <= right)

  //   //   if touched {
  //   //     Goal.setCursor(goal, state.editor)
  //   //   }
  //   // })

  //   // state.goals = goals
  //   ()
  // }

  let pointed = (state: State.t): option<(Goal.t, string)> => {
    updateIntervals(state)
    let cursor = Editor.Cursor.get(state.editor)
    let cursorOffset = state.document->VSCode.TextDocument.offsetAt(cursor)
    let pointedGoals =
      state.goals->Array.filter(goal => Interval.contains(goal.interval, cursorOffset))
    // return the first pointed goal
    pointedGoals[0]->Option.map(goal => (goal, Goal.getContent(goal, state.document)))
  }

  // let placeCursorAtFirstNewGoal = (state: State.t, rewriteText, rewriteRange) => {
  //   // locate the first new goal and place the cursor there
  //   let splittedLines = Parser.splitToLines(rewriteText)
  //   splittedLines[0]->Option.forEach(line => {
  //     let col = String.length(line) - 1
  //     let lastChar = String.charAt(line, col)
  //     if lastChar == "?" {
  //       let position = VSCode.Position.translate(VSCode.Range.start(rewriteRange), 0, col)
  //       Editor.Cursor.set(state.editor, position)
  //     }
  //   })
  // }

  // Replace definition of extended lambda with new clauses

  // We are asked to replace a clause like "x → ?" with multiple clauese
  // There are two kinds of syntax for extended lambda
  //  1.  λ { x → ?
  //        ; y → ?
  //        }
  //  2.  λ where
  //          x → ?
  //          y → ?
  // let replaceWithLambda = async (state: State.t, goal, lines) => {
  //   let (inWhereClause, indentWidth, rewriteInterval) = caseSplitAux(state.document, goal)
  //   let rewriteText = if inWhereClause {
  //     lines->Array.join("\n" ++ String.repeat(" ", indentWidth))
  //   } else {
  //     lines->Array.join("\n" ++ (String.repeat(" ", indentWidth - 2) ++ "; "))
  //   }
  //   let rewriteRange = Interval.toVSCodeRange(state.document, rewriteInterval)
  //   if await Editor.Text.replace(state.document, rewriteRange, rewriteText) {
  //     // destroy the old goal
  //     Goal.destroyDecoration(goal)
  //     // locate the first new goal and place the cursor there
  //     placeCursorAtFirstNewGoal(state, rewriteText, rewriteRange)
  //   } else {
  //     await State__View.Panel.display(
  //       state,
  //       Error("Goal-related Error"),
  //       [Item.plainText("Unable to replace the lines of goal #" ++ string_of_int(goal.index))],
  //     )
  //   }
  // }

  // // replace and insert one or more lines of content at the goal
  // // usage: case split
  // let replaceWithLines = async (state: State.t, goal, lines) => {
  //   // get the width of indentation from the first line of the goal
  //   let (indentWidth, _, _) = indentationWidth(state.document, goal)
  //   let indentation = String.repeat(" ", indentWidth)
  //   let indentedLines = indentation ++ lines->Array.join("\n" ++ indentation)
  //   // the rows spanned by the goal (including the text outside the goal)
  //   // will be replaced by the `indentedLines`
  //   let start = VSCode.TextDocument.positionAt(state.document, fst(goal.interval))
  //   let startLineNo = VSCode.Position.line(start)
  //   let startLineRange =
  //     state.document->VSCode.TextDocument.lineAt(startLineNo)->VSCode.TextLine.range
  //   let start = VSCode.Range.start(startLineRange)

  //   let end_ = VSCode.TextDocument.positionAt(state.document, snd(goal.interval))
  //   let rangeToBeReplaced = VSCode.Range.make(start, end_)
  //   if await Editor.Text.replace(state.document, rangeToBeReplaced, indentedLines) {
  //     // destroy the old goal
  //     Goal.destroyDecoration(goal)
  //     // locate the first new goal and place the cursor there
  //     placeCursorAtFirstNewGoal(state, indentedLines, rangeToBeReplaced)
  //   } else {
  //     await State__View.Panel.display(
  //       state,
  //       Error("Goal-related Error"),
  //       [Item.plainText("Unable to replace the lines of goal #" ++ string_of_int(goal.index))],
  //     )
  //   }
  // }

  // let removeBoundaryAndDestroy = async (state: State.t, goal) => {
  //   updateIntervals(state)

  //   let innerRange = Goal.getInnerRange(goal, state.document)
  //   let outerRange = Interval.toVSCodeRange(state.document, goal.interval)
  //   let content = Editor.Text.get(state.document, innerRange)->String.trim
  //   if await Editor.Text.replace(state.document, outerRange, content) {
  //     Goal.destroyDecoration(goal)
  //   } else {
  //     await State__View.Panel.display(
  //       state,
  //       Error("Goal-related Error"),
  //       [Item.plainText("Unable to remove the boundary of goal #" ++ string_of_int(goal.index))],
  //     )
  //   }
  // }

  let redecorate = state => {
    updateIntervals(state)
    // goal decorations
    state.goals->Array.forEach(goal => goal->Goal.refreshDecoration(state.editor))
  }
}
include Module
