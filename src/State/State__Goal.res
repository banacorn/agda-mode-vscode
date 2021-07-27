open Belt
open Common
module type Module = {
  let instantiate: (State.t, array<int>) => Promise.t<unit>
  let modify: (State.t, Goal.t, string => string) => Promise.t<unit>
  let removeBoundaryAndDestroy: (State.t, Goal.t) => Promise.t<unit>
  let pointed: State.t => option<(Goal.t, string)>
  let replaceWithLines: (State.t, Goal.t, array<string>) => Promise.t<unit>
  let replaceWithLambda: (State.t, Goal.t, array<string>) => Promise.t<unit>
  let caseSplitAux: (VSCode.TextDocument.t, AgdaModeVscode.Goal.t) => (bool, int, Interval.t)

  let redecorate: State.t => unit
  let next: State.t => Promise.t<unit>
  let previous: State.t => Promise.t<unit>
}

module Module: Module = {
  // return an array of Offsets of Goals
  let getOffsets = (state: State.t): array<int> =>
    state.goals->Array.map(goal => fst(goal.interval) + 3)

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

  let caseSplitAux = (document, goal: Goal.t) => {
    let textBeforeGoal = {
      let interval = (0, fst(goal.interval))
      let range = Editor.Range.fromInterval(document, interval)
      Editor.Text.get(document, range)
    }

    // returns the offset of the first non-blank character it encountered from somewhere
    let nextWordBoundary = (start, string) => {
      let break = ref(false)
      let n = ref(0)

      let i = ref(start)
      while i.contents < Js.String.length(string) && !break.contents {
        let char = Js.String.charAt(i.contents, string)
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
          let i = ref(fst(goal.interval) - 1)
          while i.contents >= 0 && bracketCount.contents >= 0 {
            switch i.contents {
            // no preceding character
            | 0 => ()
            // has preceding character
            | i' =>
              switch Js.String.charAt(i' - 1, textBeforeGoal) {
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
        switch Js.String.lastIndexOf(";", textBeforeGoal) {
        | -1 => 0
        | n => n
        } + 1

      let lastWhereTokenOffset =
        switch Js.String.lastIndexOf("where", textBeforeGoal) {
        | -1 => 0
        | n => n
        } + 5

      let lastLineBreakOffset =
        max(
          0,
          max(
            Js.String.lastIndexOf("\r", textBeforeGoal),
            Js.String.lastIndexOf("\n", textBeforeGoal),
          ),
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
    let range = {
      let caseStart = nextWordBoundary(searchStart, textBeforeGoal)
      let caseEnd = snd(goal.interval)
      (caseStart, caseEnd)
    }
    (inWhereClause, fst(range) - lastLineBreakOffset, range)
  }

  // returns the width of indentation of the first line of a goal
  // along with the text and the range before the goal
  let indentationWidth = (document, goal: Goal.t): (int, string, VSCode.Range.t) => {
    let goalStart = document->VSCode.TextDocument.positionAt(fst(goal.interval))
    let lineNo = VSCode.Position.line(goalStart)
    let range = VSCode.Range.make(VSCode.Position.make(lineNo, 0), goalStart)
    let textBeforeGoal = Editor.Text.get(document, range)
    // tally the number of blank characters
    // ' ', '\012', '\n', '\r', and '\t'
    let indentedBy = s => {
      let n = ref(0)
      for i in 0 to Js.String.length(s) - 1 {
        switch Js.String.charAt(i, s) {
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

  let jumpToOffset = (state: State.t, offset) => {
    let point = state.document->VSCode.TextDocument.positionAt(offset)
    let range = VSCode.Range.make(point, point)
    Editor.reveal(state.editor, range)
  }

  // parse the whole source file and update the intervals of an array of Goal.t
  let updateIntervals = (state: State.t) => {
    let indices = state.goals->Array.map(goal => goal.index)
    let diffs = Goal.generateDiffs(state.document, indices)
    diffs->Array.forEachWithIndex((i, diff) =>
      switch state.goals[i] {
      | None => () // do nothing :|
      | Some(goal) => goal.interval = diff.modifiedInterval
      }
    )
  }

  let modify = (state: State.t, goal, f) => {
    updateIntervals(state)
    let content = Goal.getContent(goal, state.document)
    Goal.setContent(goal, state.document, f(content))->Promise.flatMap(x => {
      switch x {
      | true =>
        updateIntervals(state)
        Promise.resolved()
      | false =>
        State.View.display(
          state,
          Error("Goal-related Error"),
          [
            Item.plainText(
              "Failed to modify the content of goal #" ++ string_of_int(goal.index),
            ),
          ],
        )
      }
    })
  }

  let next = (state: State.t): Promise.t<unit> => {
    updateIntervals(state)

    let nextGoal = ref(None)
    let cursorOffset = VSCode.TextDocument.offsetAt(state.document, Editor.Cursor.get(state.editor))
    let offsets = getOffsets(state)

    // find the first Goal after the cursor
    offsets->Array.forEach(offset =>
      if cursorOffset < offset && nextGoal.contents === None {
        nextGoal := Some(offset)
      }
    )

    // if there's no Goal after the cursor, then loop back and return the first Goal
    if nextGoal.contents === None {
      nextGoal := offsets[0]
    }

    switch nextGoal.contents {
    | None => Promise.resolved()
    | Some(offset) =>
      let point = state.document->VSCode.TextDocument.positionAt(offset)
      Editor.Cursor.set(state.editor, point)
      state->jumpToOffset(offset)
      Promise.resolved()
    }
  }

  let previous = (state: State.t): Promise.t<unit> => {
    updateIntervals(state)

    let previousGoal = ref(None)
    let cursorOffset = VSCode.TextDocument.offsetAt(state.document, Editor.Cursor.get(state.editor))
    let offsets = getOffsets(state)

    // find the last Goal before the cursor
    offsets->Array.forEach(offset =>
      if cursorOffset > offset {
        previousGoal := Some(offset)
      }
    )

    // loop back if this is already the first Goal
    if previousGoal.contents === None {
      previousGoal := offsets[Array.length(offsets) - 1]
    }

    switch previousGoal.contents {
    | None => Promise.resolved()
    | Some(offset) =>
      let point = state.document->VSCode.TextDocument.positionAt(offset)
      Editor.Cursor.set(state.editor, point)
      state->jumpToOffset(offset)
      Promise.resolved()
    }
  }

  let instantiate = (state: State.t, indices) => {
    // destroy all existing goals
    state.goals->Array.forEach(Goal.destroy)

    // get ther cursor position before editing the text buffer
    // so that we can place the cursor inside a goal later
    let selection = VSCode.TextEditor.selection(state.editor)
    let cursorStart = VSCode.TextDocument.offsetAt(state.document, VSCode.Selection.start(selection))
    let cursorEnd = VSCode.TextDocument.offsetAt(state.document, VSCode.Selection.end_(selection))

    // instantiate new ones
    Goal.makeMany(state.editor, indices)->Promise.map(goals => {

      goals->Array.forEach(goal => {
        // if there's a cursor that touches the hole's "boundary"
        // 
        //                 {! some hole !}
        //                 ^^           ^^
        // 
        //  move the cursor inside the hole
        // 
        //                 {! some hole !}
        //                    ^
        // 
        let (left, right) = goal.interval 
        let touched = 
            (left <= cursorStart && cursorStart <= left + 2) || 
            (right - 2 <= cursorStart && cursorStart <= right) || 
            (left <= cursorEnd && cursorEnd <= left + 2) || 
            (right - 2 <= cursorEnd && cursorEnd <= right)

        if touched {
          Goal.setCursor(goal, state.editor)
        }
      })

      state.goals = goals
    })
  }

  let pointed = (state: State.t): option<(Goal.t, string)> => {
    updateIntervals(state)

    // let cursor = switch cursor {
    // | None => Editor.Cursor.get(state.editor)
    // | Some(x) => x
    // }
    let cursor = Editor.Cursor.get(state.editor)
    let cursorOffset = state.document->VSCode.TextDocument.offsetAt(cursor)
    let pointedGoals =
      state.goals->Array.keep(goal => Interval.contains(goal.interval, cursorOffset))
    // return the first pointed goal
    pointedGoals[0]->Option.map(goal => (goal, Goal.getContent(goal, state.document)))
  }

  let placeCursorAtFirstNewGoal = (state: State.t, rewriteText, rewriteRange) => {
    // locate the first new goal and place the cursor there
    let splittedLines = Js.String.split("\n", rewriteText)
    splittedLines[0]->Option.forEach(line => {
      let col = Js.String.length(line) - 1
      let lastChar = Js.String.charAt(col, line)
      if lastChar == "?" {
        let position = VSCode.Position.translate(VSCode.Range.start(rewriteRange), 0, col)
        Editor.Cursor.set(state.editor, position)
      }
    })
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
  let replaceWithLambda = (state: State.t, goal, lines) => {
    let (inWhereClause, indentWidth, rewriteInterval) = caseSplitAux(state.document, goal)
    let rewriteText = if inWhereClause {
      Js.Array.joinWith("\n" ++ Js.String.repeat(indentWidth, " "), lines)
    } else {
      Js.Array.joinWith("\n" ++ (Js.String.repeat(indentWidth - 2, " ") ++ "; "), lines)
    }
    let rewriteRange = Editor.Range.fromInterval(state.document, rewriteInterval)
    Editor.Text.replace(state.document, rewriteRange, rewriteText)->Promise.flatMap(x =>
      switch x {
      | true =>
        // destroy the old goal 
        Goal.destroy(goal)
        // locate the first new goal and place the cursor there
        placeCursorAtFirstNewGoal(state, rewriteText, rewriteRange)
        Promise.resolved()
      | false =>
        State.View.display(
          state,
          Error("Goal-related Error"),
          [
            Item.plainText(
              "Unable to replace the lines of goal #" ++ string_of_int(goal.index),
            ),
          ],
        )
      }
    )
  }

  // replace and insert one or more lines of content at the goal
  // usage: case split
  let replaceWithLines = (state: State.t, goal, lines) => {
    // get the width of indentation from the first line of the goal
    let (indentWidth, _, _) = indentationWidth(state.document, goal)
    let indentation = Js.String.repeat(indentWidth, " ")
    let indentedLines = indentation ++ Js.Array.joinWith("\n" ++ indentation, lines)
    // the rows spanned by the goal (including the text outside the goal)
    // will be replaced by the `indentedLines`
    let start = Editor.Position.fromOffset(state.document, fst(goal.interval))
    let startLineNo = VSCode.Position.line(start)
    let startLineRange =
      state.document->VSCode.TextDocument.lineAt(startLineNo)->VSCode.TextLine.range
    let start = VSCode.Range.start(startLineRange)

    let end_ = Editor.Position.fromOffset(state.document, snd(goal.interval))
    let rangeToBeReplaced = VSCode.Range.make(start, end_)
    Editor.Text.replace(state.document, rangeToBeReplaced, indentedLines)->Promise.flatMap(x =>
      switch x {
      | true =>
        // destroy the old goal 
        Goal.destroy(goal)
        // locate the first new goal and place the cursor there
        placeCursorAtFirstNewGoal(state, indentedLines, rangeToBeReplaced)
        Promise.resolved()
      | false =>
        State.View.display(
          state,
          Error("Goal-related Error"),
          [
            Item.plainText(
              "Unable to replace the lines of goal #" ++ string_of_int(goal.index),
            ),
          ],
        )
      }
    )
  }

  let removeBoundaryAndDestroy = (state: State.t, goal) => {
    updateIntervals(state)

    let innerRange = Goal.getInnerRange(goal, state.document)
    let outerRange = Editor.Range.fromInterval(state.document, goal.interval)
    let content = Editor.Text.get(state.document, innerRange)->String.trim
    Editor.Text.replace(state.document, outerRange, content)->Promise.flatMap(x =>
      switch x {
      | true =>
        Goal.destroy(goal)
        Promise.resolved()
      | false =>
        State.View.display(
          state,
          Error("Goal-related Error"),
          [
            Item.plainText(
              "Unable to remove the boundary of goal #" ++ string_of_int(goal.index),
            ),
          ],
        )
      }
    )
  }

  let redecorate = state => {
    updateIntervals(state)
    // goal decorations
    state.goals->Array.forEach(goal => goal->Goal.refreshDecoration(state.editor))
  }
}
include Module
