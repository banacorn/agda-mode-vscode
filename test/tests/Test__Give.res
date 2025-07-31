open Mocha
open Test__Util

describe("agda-mode.give", () => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Give.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Give.agda"), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")

    let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(7, 14), "y")
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.Give({
        index: 0,
        indexString: "0",
        start: 91,
        end: 98,
      }),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(
      filteredResponses,
      [
        GiveAction(0, GiveNoParen),
        DisplayInfo(AllGoalsWarnings("*All Goals*", "?1 : ℕ\n")),
        InteractionPoints([1]),
      ],
    )
  })

  Async.it("should remove the given goal", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")
    await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 14), ~payload="y")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)

    await ctx->AgdaMode.quit
    let actual = await File.read(Path.asset("Give.agda"))
    let expected = await File.read(Path.asset("Give.agda.out"))
    Assert.deepStrictEqual(actual, expected)
  })

  // Test case for GitHub issue #249: give command bug when used twice without reloading
  describe_only("Issue #249: consecutive give commands", () => {
    let filename = "Issue249.agda"
    let fileContent = ref("")
    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should handle consecutive give commands without infinite loops or disabled goals",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)

        // First give command: give "? , ?"
        await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 9), ~payload="? , ?") // position of the goal ?

        // Verify that two new goals were created
        Assert.ok(ctx.state.goals->Goals.size >= 2)

        // Second give command: give "tt" to the first goal without reloading
        await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 9), ~payload="tt") // position of first goal

        // compare file content before and after
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(Path.asset(filename ++ ".out"))
        Assert.deepStrictEqual(actual, expected)

        await ctx->AgdaMode.quit
      },
    )
  })
})
