open Mocha
open Test__Util

let run = normalization => {
  let filename = "ModuleContents.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses (goal)", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.ModuleContents(
        normalization,
        "Lib",
        {
          index: 0,
          indexString: "0",
          start: 48,
          end: 55,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(
      filteredResponses,
      [
        DisplayInfo(
          ModuleContents(
            "Modules\n  Bool\n  ℕ\nNames\n  Bool  : Set\n  false : Lib.Bool\n  if_then_else_\n        : {A : Set} → Lib.Bool → A → A → A\n  suc   : Lib.ℕ → Lib.ℕ\n  true  : Lib.Bool\n  zero  : Lib.ℕ\n  ℕ     : Set",
          ),
        ),
      ],
    )
  })

  Async.it("should be responded with correct responses (global)", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.ModuleContentsGlobal(normalization, "Lib"),
      )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(
      filteredResponses,
      [
        DisplayInfo(
          ModuleContents(
            "Modules\n  Bool\n  ℕ\nNames\n  Bool  : Set\n  false : Lib.Bool\n  if_then_else_\n        : {A : Set} → Lib.Bool → A → A → A\n  suc   : Lib.ℕ → Lib.ℕ\n  true  : Lib.Bool\n  zero  : Lib.ℕ\n  ℕ     : Set",
          ),
        ),
      ],
    )
  })
}

describe("agda-mode.module-contents", () => {
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
