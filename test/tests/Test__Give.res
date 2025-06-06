open Mocha
open Test__Util

describe_only("agda-mode.give", () => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Give.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Give.agda"), fileContent.contents))

  Async.it("should be responded with the correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")

    let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(7, 14), "y")
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.Give2({
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

  Async.it("should remove the give goal", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")
    await AgdaMode.give(ctx, ~cursor=VSCode.Position.make(7, 14), ~payload="y")
    Assert.deepStrictEqual(ctx.state.goals2->Goals.size, 1)

    let actual = await File.read(Path.asset("Give.agda"))
    let expected = await File.read(Path.asset("Give.agda.out"))
    Assert.deepStrictEqual(actual, expected)
  })
})
