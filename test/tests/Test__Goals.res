open Mocha
open Test__Util

describe("Goals", () => {
  let filename = "Goals.agda"
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))
  Async.after(async () => await File.write(Path.asset(filename), fileContent.contents))

  describe("Handle `onDidChangeTextDocument`", () => {
    Async.it(
      "should instantiate all 5 goals with question marks expanded to holes",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)

        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        // compare file content before and after
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(Path.asset("Goals.agda.out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should translate goals on an insertion immediately before a goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(8, 18), " ")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:20-27)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should translate goals on an insertion immediately after a goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(8, 25), " ")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should destroy a goal after it has been completely deleted",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
        )
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should destroy a goal after it has been completely replaced 1",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let _ = await Editor.Text.replace(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
          "       ",
        )
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should destroy a goal after it has been completely replaced 2",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let _ = await Editor.Text.replace(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(10, 17), VSCode.Position.make(10, 26)),
          "::DD",
        )
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#4 [11:22-26)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should only resize a goal after its content has been edited",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let _ = await Editor.Text.replace(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(9, 22), VSCode.Position.make(9, 23)),
          ":D",
        )

        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-28)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    describe_skip(
      "Restore hole damaged boundaries",
      () => {
        Async.it(
          "should protect against a backspace on the right boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad(filename)
            let _ = await Editor.Text.delete(
              ctx.state.document,
              VSCode.Range.make(VSCode.Position.make(9, 25), VSCode.Position.make(9, 26)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serializeGoals(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )

        Async.it(
          "should protect against a deletion on the right boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad(filename)
            let _ = await Editor.Text.delete(
              ctx.state.document,
              VSCode.Range.make(VSCode.Position.make(9, 24), VSCode.Position.make(9, 25)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serializeGoals(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )

        Async.it(
          "should protect against a backspace on the left boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad(filename)
            let _ = await Editor.Text.delete(
              ctx.state.document,
              VSCode.Range.make(VSCode.Position.make(9, 20), VSCode.Position.make(9, 21)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serializeGoals(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )

        Async.it(
          "should protect against a deletion on the left boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad(filename)
            let _ = await Editor.Text.delete(
              ctx.state.document,
              VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 20)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serializeGoals(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )
      },
    )
  })

  describe("`parseGoalPositionsFromRefine`", () => {
    Async.it(
      "should find single question mark",
      async () => {
        let input = "?"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(0, 1)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should find question mark surrounded by spaces",
      async () => {
        let input = " ? "
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(1, 2)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should find multiple question marks",
      async () => {
        let input = "(fst ? ?)"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(5, 6), (7, 8)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should find question marks with various delimiters",
      async () => {
        let input = "(a ? b) {c ? d} e ? f"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(3, 4), (11, 12), (18, 19)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should find question mark at start of string",
      async () => {
        let input = "? rest"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(0, 1)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should find question mark at end of string",
      async () => {
        let input = "start ?"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(6, 7)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should ignore question marks in identifiers",
      async () => {
        let input = "foo?bar"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = []
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle mixed standalone and non-standalone question marks",
      async () => {
        let input = "foo?bar ? baz"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(8, 9)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle empty string",
      async () => {
        let input = ""
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = []
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle string with no question marks",
      async () => {
        let input = "no question marks here"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = []
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle complex expression from real use case",
      async () => {
        let input = "record\n{ very-long-field-name-1 = ?\n; very-long-field-name-2 = ?\n; very-long-field-name-3 = ?\n}"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        // Now that the function works, expect all 3 question marks
        let expected = if OS.onUnix {
          [(34, 35), (63, 64), (92, 93)]
        } else {
          [(35, 36), (65, 66), (95, 96)]
        }
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle underscore delimiter",
      async () => {
        let input = "foo_?_bar"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        let expected = [(4, 5)]
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle all supported delimiters",
      async () => {
        let input = " ?(){}_.\\\"@?"
        let actual = Goals.parseGoalPositionsFromRefine(input)
        // Calculate positions: first ? at position 1, last ? at the end
        let firstPos = String.indexOf(input, "?")
        let lastPos = String.lastIndexOf(input, "?")
        let expected = [(firstPos, firstPos + 1), (lastPos, lastPos + 1)]
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("`getGoalAtCursor`", () => {
    Async.it(
      "should return `None` when the cursor is not in a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(0, 0))

        let actual = ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)
        await ctx->AgdaMode.quit
        Assert.deepStrictEqual(actual, None)
      },
    )

    Async.it(
      "should return the goal when the cursor is inside a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(7, 14))

        let actual =
          ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)->Option.map(Goal.toString)
        await ctx->AgdaMode.quit
        Assert.deepStrictEqual(actual, Some("#0 [8:12-19)"))
      },
    )

    Async.it(
      "should return the goal when the cursor is immediately before a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(7, 11))

        let actual =
          ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)->Option.map(Goal.toString)
        await ctx->AgdaMode.quit
        Assert.deepStrictEqual(actual, Some("#0 [8:12-19)"))
      },
    )

    Async.it(
      "should return the goal when the cursor is immediately after a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(7, 18))

        let actual =
          ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)->Option.map(Goal.toString)
        await ctx->AgdaMode.quit
        Assert.deepStrictEqual(actual, Some("#0 [8:12-19)"))
      },
    )
  })

  describe("Jumping between goals", () => {
    Async.it(
      "should jump to the next goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)

        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(0, 0))

        // should land on the first goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(7, 14))

        // should land on the second goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(8, 21))

        // should land on the third goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(9, 22))

        // should land on the fourth goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 21))

        // should land on the fifth goal
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 28))

        // should land on the first goal again
        await ctx->AgdaMode.nextGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(7, 14))

        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should jump to the previous goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(0, 0))

        // should land on the last goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 28))

        // should land on the fourth goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 21))

        // should land on the third goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(9, 22))

        // should land on the second goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(8, 21))

        // should land on the first goal
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(7, 14))

        // should land on the last goal again
        await ctx->AgdaMode.previousGoal
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 28))

        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Cursor placement", () => {
    let filename = filename
    Async.it(
      "should place the cursor inside a nearby hole after expanding it",
      async () => {
        let filepath = Path.asset(filename)
        let _ = await VSCode.Workspace.openTextDocumentWithFileName(filepath)
        switch VSCode.Window.activeTextEditor {
        | None => Assert.fail("Cannot open editor for " ++ filename)
        | Some(editor) => editor->Editor.Cursor.set(VSCode.Position.make(8, 18))
        }

        let ctx = await AgdaMode.makeAndLoad(filename)
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(8, 21))
      },
    )
  })

  describe("Issue #157", () => {
    Async.it(
      "should handle nested holes correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue157.agda")

        // check the goal positions
        Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [4:5-26)"])
        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Issue #159", () => {
    Async.it(
      "should create holes in literate agda files",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue159.lagda.tex")

        // check the goal positions
        Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [5:5-7:3)"])
        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Issue #211", () => {
    Async.it(
      "should create a hole on the second line instead of the first line",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue211.agda")

        // check the goal positions
        Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [5:5-12)"])
        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Issue #214", () => {
    Async.it(
      "should not create a hole in non-Agda code blocks",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue214.lagda.md")

        // check the goal positions
        Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [9:11-17)"])
        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Issue #229", () => {
    Async.it(
      "should not create a hole in an indentifier with a question mark",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue229.agda")

        await AgdaMode.execute(ctx, Refine, ~cursor=VSCode.Position.make(12, 11))

        // check the goal positions
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#1 [19:9-16)", "#2 [13:13-20)", "#3 [13:21-28)"],
        )
        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Issue #239", () => {
    Async.it(
      "should commented out holes should be ignored",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)

        // check the goal positions and make sure they are correct
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        // comment out Line 8 which contains the first goal
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(7, 0), "-- ")

        // reload
        await AgdaMode.execute(ctx, Load)

        // check the goal positions again
        Assert.deepStrictEqual(
          Goals.serializeGoals(ctx.state.goals),
          ["#0 [9:19-26)", "#1 [10:20-27)", "#2 [11:19-26)", "#3 [11:27-31)"],
        )

        await ctx->AgdaMode.quit
      },
    )
  })

  describe("Issue #250 - Unicode surrogate pairs + Windows CRLF", () => {
    let filename = "UnicodeGoalPlacement.agda"
    let fileContent = ref("")

    Async.beforeEach(
      async () => {
        let content = await File.read(Path.asset(filename))
        fileContent := content
      },
    )
    Async.afterEach(
      async () => {
        await File.write(Path.asset(filename), fileContent.contents)
      },
    )

    Async.it(
      "should correctly place holes when Unicode characters are properly handled",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        let goalPositions = Goals.serializeGoals(ctx.state.goals)

        // Platform-agnostic expected positions
        let positions = [
          "#0 [12:6-13)", // Line 12: 𝐱 = {!   !}
          "#1 [16:10-17)", // Line 16: 𝐱𝐲𝐳 = {!   !}
          "#2 [20:24-31)", // Line 20: 𝐍ormal-text-then-𝐱 = {!   !}
          "#3 [24:12-19)", // Line 24: 𝐚 𝐱 𝐲 = {!   !} {!   !}
          "#4 [24:20-27)", // Line 24: second goal
          "#5 [28:6-13)", // Line 28: 𝐛 = ? -> {!   !}
          "#6 [32:11-18)", // Line 32: 𝐀𝔅ℂ𝐝 = {!   !}
        ]

        Assert.deepStrictEqual(goalPositions, positions)

        await ctx->AgdaMode.quit
      },
    )
  })
})
