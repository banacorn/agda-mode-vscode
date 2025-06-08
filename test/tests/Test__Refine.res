open Mocha
open Test__Util

describe_only("agda-mode.refine", () => {
  describe("Issue #158", () => {
    let fileContent = ref("")

    Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Refine.agda"))))
    Async.afterEach(async () => await File.write(Path.asset("Refine.agda"), fileContent.contents))

    Async.it(
      "should result in the correct refinement",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Refine.agda")
        await ctx->AgdaMode.refine(~cursor=VSCode.Position.make(13, 9))
        let actual = await File.read(Path.asset("Refine.agda"))
        let expected = await File.read(Path.asset("Refine.agda.out"))
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })
})

describe("State__Goal", () => {
  describe("parseHolesFromRefineResult", () => {
    Async.it(
      "should parse holes correctly",
      async () => {
        let raw = "record
{ very-long-field-name-1 = ?
; very-long-field-name-2 = ?
; very-long-field-name-3 = ?
} "
        let actual = State__Goal.parseHolesFromRefineResult(raw)
        let expected = [34, 63, 92]
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })
})

// record
// { very-long-field-name-1 = ?
// ; very-long-field-name-2 = ?
// ; very-long-field-name-3 = ?
// } 
