open Mocha
open Test__Util

let run = normalization => {
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Give.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Give.agda"), fileContent.contents))

  Async.it("should remove the given goal", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")
    await AgdaMode.elaborateAndGive(
      ctx,
      normalization,
      ~cursor=VSCode.Position.make(7, 14),
      ~payload="y",
    )
    Assert.deepStrictEqual(ctx.state.goals2->Goals.size, 1)

    await ctx->AgdaMode.quit
    let actual = await File.read(Path.asset("Give.agda"))
    let expected = await File.read(Path.asset("Give.agda.out"))
    Assert.deepStrictEqual(actual, expected)
  })
}

describe("agda-mode.elaborate-and-give", () => {
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
