open Mocha
open Test__Util

let run = normalization => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Issue204.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Issue204.agda"), fileContent.contents))

  Async.it("should be responded with the correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue204.agda")

    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.SolveConstraintsGlobal(normalization),
      )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Js.log(filteredResponses->Array.map(Response.toString)->Array.join("\n"))
    Assert.deepEqual(filteredResponses, [InteractionPoints([0]), SolveAll([(0, "4")])])
  })

  Async.it("should remove all goals", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue204.agda")
    await AgdaMode.solveConstraints(ctx, normalization)
    Assert.deepEqual(ctx.state.goals->Array.length, 0)
  })
}

describe("agda-mode.solve-constraints", () => {
  This.timeout(4000) // it takes more than 2000ms sometimes

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
