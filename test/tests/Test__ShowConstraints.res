open Mocha
open Test__Util

// Agda 2.9 made `Cmd_constraints` take a normalization argument, and the version check
// in `Request.encode` follows it. Encoding tests cannot tell if Agda accepts the request,
// so this test sends it to a real Agda in every normalization mode.
let run = normalization => {
  let filename = "Goals.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with the unsolved constraints", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.ShowConstraints(normalization),
      )
    await ctx->AgdaMode.quit

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(
      filteredResponses,
      [DisplayInfo(Constraints(Some("_21 := (_ : _11) ? : ℕ (blocked on _11)")))],
    )
  })
}

describe("agda-mode.show-constraints", () => {
  describe("AsIs", () => {
    run(AsIs)
  })

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
