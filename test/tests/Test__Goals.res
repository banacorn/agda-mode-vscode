open Mocha
open Test__Util

describe("Goals", () => {
  let fileContent = ref("")

  Async.before(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))
  Async.after(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  describe("On document change", () => {
    Async.it(
      "should instantiate all 4 goals with question marks expanded to holes",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
        await ctx->AgdaMode.quit
        // compare file content before and after
        let actual = await File.read(Path.asset("Goals.agda"))
        let expected = await File.read(Path.asset("Goals.agda.out"))
        Assert.deepStrictEqual(actual, expected)
        // check the positions of the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )
  })

  Async.it("should remove a goal after it has been completely destroyed", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.delete(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 19), VSCode.Position.make(9, 26)),
    )
    await ctx->AgdaMode.quit
    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#3 [164-168)"],
    )
  })

  Async.it("should only resize a goal after its content has been edited", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let _ = await Editor.Text.replace(
      ctx.state.document,
      VSCode.Range.make(VSCode.Position.make(9, 22), VSCode.Position.make(9, 23)),
      ":D",
    )
    await ctx->AgdaMode.quit

    // check the positions of the goals
    Assert.deepStrictEqual(
      Goals.serialize(ctx.state.goals2),
      ["#0 [92-99)", "#1 [118-125)", "#2 [145-153)", "#3 [172-176)"],
    )
  })

  describe("Restore hole damanged boundaries", () => {
    Async.it(
      "should protect against a backspace on the right boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
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
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    Async.it(
      "should protect against a deletion on the right boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
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
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    Async.it(
      "should protect against a backspace on the left boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
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
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
      },
    )

    Async.it(
      "should protect against a deletion on the left boundary",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Goals.agda")
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
          Goals.serialize(ctx.state.goals2),
          ["#0 [92-99)", "#1 [118-125)", "#2 [145-152)", "#3 [171-175)"],
        )
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
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 20))

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
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 20))

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
        Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(10, 20))
      },
    )
  })
})
