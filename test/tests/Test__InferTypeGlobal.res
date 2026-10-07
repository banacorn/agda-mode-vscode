open Mocha
open Test__Util

// `InferType` works inside a goal. `InferTypeGlobal` works anywhere else and sends
// `Cmd_infer_toplevel`, so this test sends it to a real Agda in every normalization mode.
let run = normalization => {
  let filename = "InferType.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with the type of the expression", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.InferTypeGlobal(normalization, "Z"),
      )
    await ctx->AgdaMode.quit

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [DisplayInfo(InferredType("ℕ"))])
  })
}

describe("agda-mode.infer-type-global", () => {
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
