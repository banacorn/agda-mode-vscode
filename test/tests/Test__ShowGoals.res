open Mocha
open Test__Util

let run = normalization => {
  let filename = "Goals.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.ShowGoals(normalization),
      )

    let filteredResponses = responses->Array.filter(filteredResponse)

    let filepath = Path.asset(filename)
    let expectedAllGoalsWarningsBody = `?0 : ℕ\n?1 : ℕ\n?2 : ℕ\n?3 : _11\nSort _10  [ at ${filepath}:11,19-31 ]\n_11 : _10  [ at ${filepath}:11,19-31 ]\n_14 : ℕ  [ at ${filepath}:11,19-31 ]\n\n———— Error —————————————————————————————————————————————————\nUnsolved constraints`

    Assert.deepStrictEqual(
      filteredResponses,
      [DisplayInfo(AllGoalsWarnings("*All Goals, Errors*", expectedAllGoalsWarningsBody))],
    )
  })

  Async.it("should work", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.execute(ctx, ShowGoals(normalization))
    await ctx->AgdaMode.quit
  })
}

describe("agda-mode.show-goals", () => {
  describe("AsIs", () => {
    run(AsIs)
  })

  describe("Simplified", () => {
    run(Simplified)
  })

  describe("Normalised", () => {
    run(Normalised)
  })

  describe("HeadNormal", () => {
    run(HeadNormal)
  })
})
