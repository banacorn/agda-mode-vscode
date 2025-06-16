open Mocha
open Test__Util

let run = normalization => {
  let filename = "GoalTypeAndContext.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with the correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.GoalTypeAndContext(
        normalization,
        {
          index: 0,
          indexString: "0",
          start: 281,
          end: 288,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [DisplayInfo(GoalType("Goal: ℕ\n————————————————————————————————————————————————————————————\nb : Bool\ny : ℕ\nx : ℕ"))])
  })

  Async.it("should work", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.execute(ctx, GoalTypeAndContext(normalization), ~cursor=VSCode.Position.make(15, 26))
    await ctx->AgdaMode.quit
  })
}

describe("agda-mode.goal-type-and-context", () => {
  describe("Simplified", () => {
    run(Simplified)
  })

  describe("Instantiated", () => {
    run(Instantiated)
  })

  describe("Normalised", () => {
    run(Normalised)
  })
})
