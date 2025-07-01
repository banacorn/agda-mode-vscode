open Mocha
open Test__Util

describe("agda-mode.refine", () => {
  This.timeout(4000)
  describe("On GiveString 1, Issue #158", () => {
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
        let expected = await File.read(Path.asset(filename ++ ".GiveString1.out"))
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
        let expected = await File.read(Path.asset(filename ++ ".GiveString2.out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("On GiveNoParen, Issue #236", () => {
    let fileContent = ref("")

    let filename = "RefineGiveNoParen.agda"

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should result in the correct refinement",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~payload="?", ~cursor=VSCode.Position.make(6, 9))

        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(Path.asset(filename ++ ".out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("On GiveNoParen with multiline content", () => {
    let fileContent = ref("")

    let filename = "RefineGiveNoParenMultiline.agda"

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should handle multiline goal content correctly",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)
        await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(6, 9))

        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(Path.asset(filename ++ ".out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })
})
