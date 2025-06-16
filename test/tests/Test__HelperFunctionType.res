open Mocha
open Test__Util

let run = normalization => {
  let filename = "HelperFunctionType.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with the correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(7, 14), "y")
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.HelperFunctionType(
        normalization,
        "helper t'",
        {
          index: 0,
          indexString: "0",
          start: 316,
          end: 329,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(
      filteredResponses,
      [
        DisplayInfo(
          HelperFunction("helper : ∀ {m} {t : T m} → T (test m t .fst) → Σ ℕ T\n"),
        ),
      ],
    )
  })

  Async.it("should copy type to the pasteboard", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.execute(ctx, HelperFunctionType(normalization), ~cursor=VSCode.Position.make(15, 3))
    // await ctx->AgdaMode.quit
    let text = await VSCode.Env.clipboard->VSCode.Clipboard.readText()
    Assert.deepStrictEqual(
      text,
      "helper : ∀ {m} {t : T m} → T (test m t .fst) → Σ ℕ T\n",
    )
  })
}

describe("agda-mode.helper-function-type", () => {
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
