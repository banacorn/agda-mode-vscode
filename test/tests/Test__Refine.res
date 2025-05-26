open Mocha
open Test__Util

describe("agda-mode.refine", () => {
  describe("Issue #158", () => {
    let fileContent = ref("")

    Async.before(async () => fileContent := (await File.read(Path.asset("Issue158.agda"))))
    Async.after(async () => await File.write(Path.asset("Issue158.agda"), fileContent.contents))

    Async.it(
      "should result in the correct refinement",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Issue158.agda")
        await ctx->AgdaMode.refine(~cursor=VSCode.Position.make(12, 9))
        let actual = await File.read(Path.asset("Issue158.agda"))
        let expected = await File.read(Path.asset("Issue158.agda.out"))
        Assert.equal(actual, expected)
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
