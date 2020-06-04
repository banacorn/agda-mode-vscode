open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);

  open! Task;

  // return an array of Offsets of Goals
  let getOffsets = (state: State.t): array(int) => {
    state.goals->Array.map(goal => fst(goal.range) + 3);
  };

  let pointingAt = (~cursor=?, state: State.t): option(Goal.t) => {
    let cursorOffset =
      switch (cursor) {
      | None =>
        Editor.offsetAtPoint(
          state.editor,
          Editor.getCursorPosition(state.editor),
        )
      | Some(x) => x
      };
    let pointedGoals =
      state.goals
      ->Array.keep(goal =>
          fst(goal.range) <= cursorOffset && cursorOffset <= snd(goal.range)
        );
    // return the first pointed goal
    pointedGoals[0];
  };

  // get the width of indentation from the first line of a goal
  let indentationWidth = (goal: Goal.t, editor) => {
    let start = Editor.pointAtOffset(editor, fst(goal.range));
    let startLineNo = Editor.Point.line(start);
    let startLineRange = Editor.rangeForLine(editor, startLineNo);
    let startLineText = Editor.getTextInRange(editor, startLineRange);
    // tally the number of blank characters
    // ' ', '\012', '\n', '\r', and '\t'
    let indentedBy = s => {
      let n = ref(0);
      for (i in 0 to Js.String.length(s) - 1) {
        switch (Js.String.charAt(i, s)) {
        | " "
        | "\012"
        | "\n"
        | "\r"
        | "\t" =>
          if (i == n^) {
            n := n^ + 1;
          }
        | _ => ()
        };
      };
      n^;
    };
    indentedBy(startLineText);
  };

  // from Goal-related action to Tasks
  let handle =
    fun
    | Instantiate(indices) => [
        Task.WithState(
          state => {
            // destroy all existing goals
            state.goals->Array.forEach(Goal.destroy);
            // instantiate new ones
            Goal.makeMany(state.editor, indices)
            ->Promise.map(goals => {
                state.goals = goals;
                [];
              });
          },
        ),
      ]
    | UpdateRange => [
        WithState(
          state => {
            Goal.updateRanges(state.goals, state.editor);
            Promise.resolved([]);
          },
        ),
      ]
    | Next => [
        Goal(UpdateRange),
        WithState(
          state => {
            Js.log(Editor.getText(state.editor));
            let nextGoal = ref(None);
            let cursorOffset =
              Editor.offsetAtPoint(
                state.editor,
                Editor.getCursorPosition(state.editor),
              );
            let offsets = getOffsets(state);

            // find the first Goal after the cursor
            offsets->Array.forEach(offset =>
              if (cursorOffset < offset && nextGoal^ === None) {
                nextGoal := Some(offset);
              }
            );

            // if there's no Goal after the cursor, then loop back and return the first Goal
            if (nextGoal^ === None) {
              nextGoal := offsets[0];
            };

            switch (nextGoal^) {
            | None => ()
            | Some(offset) =>
              Editor.setCursorPosition(
                state.editor,
                Editor.pointAtOffset(state.editor, offset),
              )
            };
            Promise.resolved([]);
          },
        ),
      ]
    | Previous => [
        Goal(UpdateRange),
        WithState(
          state => {
            let previousGoal = ref(None);
            let cursorOffset =
              Editor.offsetAtPoint(
                state.editor,
                Editor.getCursorPosition(state.editor),
              );
            let offsets = getOffsets(state);

            // find the last Goal before the cursor
            offsets->Array.forEach(offset =>
              if (cursorOffset > offset) {
                previousGoal := Some(offset);
              }
            );

            // loop back if this is already the first Goal
            if (previousGoal^ === None) {
              previousGoal := offsets[Array.length(offsets) - 1];
            };

            switch (previousGoal^) {
            | None => ()
            | Some(offset) =>
              Editor.setCursorPosition(
                state.editor,
                Editor.pointAtOffset(state.editor, offset),
              )
            };
            Promise.resolved([]);
          },
        ),
      ]
    | Modify(goal, f) => [
        Goal(UpdateRange),
        WithState(
          state => {
            let content = Goal.getContent(goal, state.editor);
            Js.log(
              "[ goal ][ modify ] \""
              ++ content
              ++ "\" => \""
              ++ f(content)
              ++ "\"",
            );
            Goal.setContent(goal, state.editor, f(content))
            ->Promise.map(
                fun
                | true => []
                | false => [
                    displayError(
                      "Goal-related Error",
                      Some(
                        "Failed to modify the content of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]

    | SaveCursor => [
        WithState(
          state => {
            let position = Editor.getCursorPosition(state.editor);
            let offset = Editor.offsetAtPoint(state.editor, position);
            state.cursor = Some(offset);
            Promise.resolved([]);
          },
        ),
      ]
    //  if the cursor is pointing at some empty hole
    //    then move the cursor inside the empty hole
    //    else restore the cursor to its original position (if there's any)
    | RestoreCursor => [
        WithState(
          state => {
            switch (state.cursor) {
            | None => ()
            | Some(offset) =>
              let position = Editor.pointAtOffset(state.editor, offset);

              let pointedGoal = pointingAt(~cursor=offset, state);
              switch (pointedGoal) {
              | Some(goal) =>
                if (Goal.getContent(goal, state.editor) == "") {
                  Goal.setCursor(goal, state.editor);
                } else {
                  Editor.setCursorPosition(state.editor, position);
                }
              | None => Editor.setCursorPosition(state.editor, position)
              };
            };
            Promise.resolved([]);
          },
        ),
      ]
    | RemoveBoundaryAndDestroy(goal) => [
        Goal(UpdateRange),
        WithState(
          state => {
            let innerRange = Goal.getInnerRange(goal, state.editor);
            let outerRange =
              Editor.Range.make(
                Editor.pointAtOffset(state.editor, fst(goal.range)),
                Editor.pointAtOffset(state.editor, snd(goal.range)),
              );
            let content =
              Editor.getTextInRange(state.editor, innerRange)->String.trim;
            Editor.setText(state.editor, outerRange, content)
            ->Promise.map(
                fun
                | true => {
                    Goal.destroy(goal);
                    [];
                  }
                | false => [
                    displayError(
                      "Goal-related Error",
                      Some(
                        "Unable to remove the boundary of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]
    // replace and insert one or more lines of content at the goal
    // usage: case split
    | ReplaceWithLines(goal, lines) => [
        WithState(
          state => {
            // get the width of indentation from the first line of the goal
            let indentWidth = indentationWidth(goal, state.editor);
            let indentation = Js.String.repeat(indentWidth, " ");
            let indentedLines =
              indentation ++ Js.Array.joinWith("\n" ++ indentation, lines);
            // the rows spanned by the goal (including the text outside the goal)
            // will be replaced by the `indentedLines`
            let start = Editor.pointAtOffset(state.editor, fst(goal.range));
            let startLineNo = Editor.Point.line(start);
            let startLineRange =
              Editor.rangeForLine(state.editor, startLineNo);
            let start = Editor.Range.start(startLineRange);

            let end_ = Editor.pointAtOffset(state.editor, snd(goal.range));
            let endLineNo = Editor.Point.line(end_);
            let endLineRange = Editor.rangeForLine(state.editor, endLineNo);
            let end_ = Editor.Range.end_(endLineRange);
            let rangeToBeReplaced = Editor.Range.make(start, end_);

            Editor.setText(state.editor, rangeToBeReplaced, indentedLines)
            ->Promise.map(
                fun
                | true => {
                    Goal.destroy(goal);
                    [];
                  }
                | false => [
                    displayError(
                      "Goal-related Error",
                      Some(
                        "Unable to replace the lines of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]

    // Replace definition of extended lambda with new clauses
    // aside from the goal itself, its clauses also need to be rewritten
    // https://github.com/agda/agda/blob/f46ecaf729c00217efad7a77e5d9932bfdd030e5/src/data/emacs-mode/agda2-mode.el#L950
    | ReplaceWithLambda(goal, lines) => [
        WithState(
          state => {
            Js.log(lines);
            // range to scan
            let goalStart =
              Editor.pointAtOffset(state.editor, fst(goal.range));
            let goalEnd =
              Editor.pointAtOffset(state.editor, snd(goal.range));
            let lineBeforeGoal =
              Editor.getTextInRange(
                state.editor,
                Editor.Range.make(
                  Editor.Point.make(Editor.Point.line(goalStart), 0),
                  goalStart,
                ),
              );
            let indentWidth = indentationWidth(goal, state.editor);
            // start at the last ";", or the first non-blank character
            let scanRow = Editor.Point.line(goalStart);
            let scanColStart =
              switch (Js.String.lastIndexOf(";", lineBeforeGoal)) {
              | (-1) => indentWidth
              | n => n + 1
              };
            let scanColEnd = Editor.Point.column(goalStart);

            // determine the position of "{"
            // iterate from the end to the start
            let bracketCount = ref(0);
            let i = ref(scanColEnd - 1);
            while (i^ >= scanColStart && bracketCount^ >= 0) {
              switch (i^) {
              // no preceding character
              | 0 => ()
              // has preceding character
              | i' =>
                switch (Js.String.charAt(i' - 1, lineBeforeGoal)) {
                | "}" => bracketCount := bracketCount^ + 1
                | "{" => bracketCount := bracketCount^ - 1
                | _ => ()
                }
              };
              i := i^ - 1;
            };
            let rewriteRange =
              Editor.Range.make(Editor.Point.make(scanRow, i^ + 1), goalEnd);
            let isLambdaWhere = i^ + 1 == indentWidth;
            (
              if (isLambdaWhere) {
                Editor.setText(
                  state.editor,
                  rewriteRange,
                  Js.Array.joinWith(
                    "\n" ++ Js.String.repeat(indentWidth, " "),
                    lines,
                  ),
                );
              } else {
                Editor.setText(
                  state.editor,
                  rewriteRange,
                  " " ++ Js.Array.joinWith(" ; ", lines),
                );
              }
            )
            ->Promise.map(
                fun
                | true => {
                    Goal.destroy(goal);
                    [];
                  }
                | false => [
                    displayError(
                      "Goal-related Error",
                      Some(
                        "Unable to replace the lines of goal #"
                        ++ string_of_int(goal.index),
                      ),
                    ),
                  ],
              );
          },
        ),
      ]
    | GetPointedOr(callback, alternative) => [
        Goal(UpdateRange),
        WithState(
          state => {
            switch (pointingAt(state)) {
            | None => Promise.resolved(alternative)
            | Some(goal) =>
              let content = Goal.getContent(goal, state.editor);
              Promise.resolved(
                callback(goal, content == "" ? None : Some(content)),
              );
            }
          },
        ),
      ]
    | GetIndexedOr(index, callback, alternative) => [
        Goal(UpdateRange),
        WithState(
          state => {
            let found = state.goals->Array.keep(goal => goal.index == index);
            switch (found[0]) {
            | None => Promise.resolved(alternative)
            | Some(goal) =>
              let content = Goal.getContent(goal, state.editor);
              Promise.resolved(
                callback(goal, content == "" ? None : Some(content)),
              );
            };
          },
        ),
      ];
};