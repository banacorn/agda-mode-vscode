open Mocha
open Test__Util

let run = normalization => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Issue204.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Issue204.agda"), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue204.agda")

    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.SolveConstraintsGlobal(normalization),
      )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(
      filteredResponses,
      [InteractionPoints([0, 1]), SolveAll([(0, "4"), (1, "4")])],
    )
    await ctx->AgdaMode.quit
  })

  Async.it("should solve all goals", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue204.agda")
    await AgdaMode.execute(ctx, SolveConstraints(normalization))
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 0)

    let actual = await File.read(Path.asset("Issue204.agda"))
    let expected = await File.read(Path.asset("Issue204.agda.out"))
    Assert.deepStrictEqual(actual, expected)

    await ctx->AgdaMode.quit
  })
}

describe("agda-mode.solve-constraints", () => {
  describe("Simplified", () => {
    run(Simplified)
  })

  describe("Normalised", () => {
    run(Normalised)
  })

  describe("Instantiated", () => {
    run(Instantiated)
  })
})
