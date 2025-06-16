open Mocha
open Test__Util

describe("agda-mode.refine", () => {
  describe("Issue #158", () => {
    let fileContent = ref("")

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Refine.agda"))))
    Async.afterEach(async () => await File.write(Path.asset("Refine.agda"), fileContent.contents))

    Async.it(
      "should result in the correct refinement",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Refine.agda")
        await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(13, 9))

        await ctx->AgdaMode.quit
        let actual = await File.read(Path.asset("Refine.agda"))
        let expected = await File.read(Path.asset("Refine.agda.out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })
})
