open Mocha
open Test__Util

let run = normalization => {
  let filename = "SearchAbout.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := await File.read(Path.asset(filename)))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.SearchAbout(normalization, "ℕ"),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [Response.DisplayInfo(SearchAbout("Definitions about ℕ\n  _+_     : ℕ → ℕ → ℕ\n  isZero  : ℕ → Bool\n  suc     : ℕ → ℕ\n  testNat : ℕ\n  zero    : ℕ"))])
  })


  Async.it("should handle empty search terms gracefully", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.SearchAbout(normalization, ""),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [])
    await ctx->AgdaMode.quit
  })

  Async.it("should handle non-existent identifiers", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.SearchAbout(normalization, "NonExistentIdentifier"),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [Response.DisplayInfo(SearchAbout("Definitions about NonExistentIdentifier"))])
    await ctx->AgdaMode.quit
  })
}

describe("agda-mode.search-about", () => {
  describe("Simplified", () => {
    run(Simplified)
  })

  describe("Instantiated", () => {
    run(Instantiated)
  })

  describe("Normalised", () => {
    run(Normalised)
  })

  describe("HeadNormal", () => {
    run(HeadNormal)
  })
})