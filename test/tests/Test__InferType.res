open Mocha
open Test__Util

let run = normalization => {
  let filename = "InferType.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.InferType(
        normalization,
        "x",
        {
          index: 0,
          indexString: "0",
          start: 88,
          end: 95,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [DisplayInfo(InferredType("ℕ"))])
  })

  Async.it("should work", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.execute(
      ctx,
      InferType(normalization),
      ~cursor=VSCode.Position.make(7, 11),
      ~payload="x",
    )
    await ctx->AgdaMode.quit
  })
}

describe("agda-mode.infer-type", () => {
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
