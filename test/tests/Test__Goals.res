open Mocha
open Test__Util

describe("Goals", () => {
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))
  Async.after(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  describe("Handle `onDidChangeTextDocument`", () => {
    Async.it(
      "should instantiate all 5 goals with question marks expanded to holes",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")

        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        // compare file content before and after
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset("Goals.agda"))
        let expected = await File.read(Path.asset("Goals.agda.out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should translate goals on an insertion immediately before a goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.insert(ctx.state.editor, VSCode.Position.make(8, 18), " ")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:20-27)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should translate goals on an insertion immediately after a goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.insert(ctx.state.editor, VSCode.Position.make(8, 25), " ")
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )

        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should destroy a goal after it has been completely deleted",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.delete(
          ctx.state.editor,
          VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
        )
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should destroy a goal after it has been completely replaced 1",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.replace(
          ctx.state.editor,
          VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
          "       ",
        )
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#3 [11:19-26)", "#4 [11:27-31)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should destroy a goal after it has been completely replaced 2",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.replace(
          ctx.state.editor,
          VSCode.Range.make(VSCode.Position.make(10, 17), VSCode.Position.make(10, 26)),
          "::DD",
        )
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [8:12-19)", "#1 [9:19-26)", "#2 [10:20-27)", "#4 [11:22-26)"],
        )
        await ctx->AgdaMode.quit
      },
    )

    Async.it(
      "should only resize a goal after its content has been edited",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        let _ = await Editor.Text.replace(
          ctx.state.editor,
          VSCode.Range.make(VSCode.Position.make(9, 22), VSCode.Position.make(9, 23)),
          ":D",
        )

        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
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
            let ctx = await AgdaMode.makeAndLoad("Goals.agda")
            let _ = await Editor.Text.delete(
              ctx.state.editor,
              VSCode.Range.make(VSCode.Position.make(9, 25), VSCode.Position.make(9, 26)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serialize(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )

        Async.it(
          "should protect against a deletion on the right boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad("Goals.agda")
            let _ = await Editor.Text.delete(
              ctx.state.editor,
              VSCode.Range.make(VSCode.Position.make(9, 24), VSCode.Position.make(9, 25)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serialize(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )

        Async.it(
          "should protect against a backspace on the left boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad("Goals.agda")
            let _ = await Editor.Text.delete(
              ctx.state.editor,
              VSCode.Range.make(VSCode.Position.make(9, 20), VSCode.Position.make(9, 21)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serialize(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )

        Async.it(
          "should protect against a deletion on the left boundary",
          async () => {
            let ctx = await AgdaMode.makeAndLoad("Goals.agda")
            let _ = await Editor.Text.delete(
              ctx.state.editor,
              VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 20)),
            )
            await ctx->AgdaMode.quit

            // check the file content
            let range = VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26))
            let actual = Editor.Text.get(ctx.state.document, range)
            Assert.deepStrictEqual(actual, "{!   !}")
            // check the positions of the goals
            Assert.deepStrictEqual(
              Goals.serialize(ctx.state.goals),
              ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
            )
          },
        )
      },
    )
  })

  describe("`getGoalAtCursor`", () => {
    Async.it(
      "should return `None` when the cursor is not in a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(0, 0))

        let actual = ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)
        Assert.deepStrictEqual(actual, None)
      },
    )

    Async.it(
      "should return the goal when the cursor is inside a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(7, 14))

        let actual =
          ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)->Option.map(Goal.toString)
        Assert.deepStrictEqual(actual, Some("#0 [8:12-19)"))
      },
    )

    Async.it(
      "should return the goal when the cursor is immediately before a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(7, 11))

        let actual =
          ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)->Option.map(Goal.toString)
        Assert.deepStrictEqual(actual, Some("#0 [8:12-19)"))
      },
    )

    Async.it(
      "should return the goal when the cursor is immediately after a hole",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        ctx.state.editor->Editor.Cursor.set(VSCode.Position.make(7, 18))

        let actual =
          ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)->Option.map(Goal.toString)
        Assert.deepStrictEqual(actual, Some("#0 [8:12-19)"))
      },
    )
  })

  describe("Jumping between goals", () => {
    Async.it(
      "should jump to the next goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
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
      },
    )

    Async.it(
      "should jump to the previous goal",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
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
      },
    )
  })

  describe("Cursor placement", () => {
    let filename = "Goals.agda"
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
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [4:5-26)"])
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
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [5:5-7:3)"])
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
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [5:5-12)"])
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
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [9:11-17)"])
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
          Goals.serialize(ctx.state.goals),
          ["#1 [19:9-16)", "#2 [13:13-20)", "#3 [13:21-28)"],
        )
        await ctx->AgdaMode.quit
      },
    )
  })
})
