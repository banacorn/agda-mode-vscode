open Mocha
open Test__Util

describe("agda-mode.refine", () => {
  This.timeout(4000)
  describe("On GiveString 1 (Issue #158)", () => {
    let fileContent = ref("")

    let filename = "Refine.agda"

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should result in the correct refinement",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(13, 9))

        // examine the goals after the refinement
        let actual = Goals.serialize(ctx.state.goals)
        Assert.deepStrictEqual(
          actual,
          ["#1 [26:11-18)", "#2 [26:19-26)", "#3 [15:30-37)", "#4 [16:30-37)", "#5 [17:30-37)"],
        )

        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString1")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("On GiveString 2", () => {
    let fileContent = ref("")

    let filename = "Refine.agda"

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should result in the correct refinement",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="fst", ~cursor=VSCode.Position.make(21, 13))

        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("On GiveParen", () => {
    let fileContent = ref("")

    let filename = "Refine.agda"

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should result in the correct refinement with simple expression",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="fst ? ?",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle payload with leading spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="  fst ? ?",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle payload with trailing spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="fst ? ?  ",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle payload with leading and trailing spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="  fst ? ?  ",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle payload with leading newline and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="\nfst ? ?",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle payload with trailing newline and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="fst ? ?\n",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle multiline payload with spaces and track goal positions correctly",
      async () => {
        let payload = if OS.onUnix {
          "  fst\n    ?\n    ?  "
        } else {
          "  fst\r\n    ?\r\n    ?  "
        }
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload, ~cursor=VSCode.Position.make(21, 13))

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [24:14-21)", "#3 [23:5-12)", "#4 [24:5-12)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(23, 12)), // Start of refined content (opening paren) // End of refined content spanning multiple lines
        )
        let expected = if OS.onUnix {
          "(fst\n    {!   !}\n    {!   !})"
        } else {
          "(fst\r\n    {!   !}\r\n    {!   !})"
        }
        Assert.strictEqual(actual, expected)
      },
    )

    Async.it(
      "should handle payload with mixed whitespace and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="\n  fst ? ?  \n",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(
          Goals.serialize(ctx.state.goals),
          ["#0 [14:7-14)", "#2 [22:33-40)", "#3 [22:16-23)", "#4 [22:24-31)"],
        )

        // verify the refined content
        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(
          Path.asset(ExpectedFiles.getExpectedFilename(filename ++ ".GiveString2")),
        )
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("On GiveNoParen (Issue #236)", () => {
    let fileContent = ref("")

    let filename = "Refine.agda"

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should result in the correct refinement with simple expression",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="true", ~cursor=VSCode.Position.make(21, 13))

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )

    Async.it(
      "should handle payload with leading spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="  true", ~cursor=VSCode.Position.make(21, 13))

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )

    Async.it(
      "should handle payload with trailing spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="true  ", ~cursor=VSCode.Position.make(21, 13))

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )

    Async.it(
      "should handle payload with leading and trailing spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="  true  ",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )

    Async.it(
      "should handle payload with leading newline and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="\ntrue", ~cursor=VSCode.Position.make(21, 13))

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )

    Async.it(
      "should handle payload with trailing newline and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="true\n", ~cursor=VSCode.Position.make(21, 13))

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )

    Async.it(
      "should handle multiline payload with spaces and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="  true\n    \n      ",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let refinedContent = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)), // Start of refined content (opening paren) // End of refined content spanning multiple lines
        )
        Assert.strictEqual(refinedContent, "true")
      },
    )

    Async.it(
      "should handle payload with mixed whitespace and track goal positions correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(
          Refine,
          ~payload="\n  true  \n",
          ~cursor=VSCode.Position.make(21, 13),
        )

        // verify the goals
        Assert.deepStrictEqual(Goals.serialize(ctx.state.goals), ["#0 [14:7-14)", "#2 [22:16-23)"])

        // verify the refined content
        await ctx->AgdaMode.quit
        let document = VSCode.TextEditor.document(ctx.state.editor)
        let actual = Editor.Text.get(
          document,
          VSCode.Range.make(VSCode.Position.make(21, 10), VSCode.Position.make(21, 14)),
        )
        Assert.strictEqual(actual, "true")
      },
    )
  })
})
