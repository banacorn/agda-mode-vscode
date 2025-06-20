open Mocha
open Test__Util

describe("agda-mode.why-in-scope", () => {
  let filename = "ModuleContents.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses (goal)", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.WhyInScope(
        "Lib.true",
        {
          index: 0,
          indexString: "0",
          start: 48,
          end: 55,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    let dependencyFilepath = Path.asset("Lib.agda")
    Assert.deepStrictEqual(
      filteredResponses,
      [
        DisplayInfo(
          WhyInScope(
            "Lib.true is in scope as\n  * a constructor Lib.Bool.true brought into scope by\n    - its definition at " ++
            dependencyFilepath ++ ":4,3-7",
          ),
        ),
      ],
    )
  })

  Async.it("should be responded with correct responses (global)", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.WhyInScopeGlobal("Lib.true"),
      )

    let filteredResponses = responses->Array.filter(filteredResponse)
    let dependencyFilepath = Path.asset("Lib.agda")
    Assert.deepStrictEqual(
      filteredResponses,
      [
        DisplayInfo(
          WhyInScope(
            "Lib.true is in scope as\n  * a constructor Lib.Bool.true brought into scope by\n    - its definition at " ++
            dependencyFilepath ++ ":4,3-7",
          ),
        ),
      ],
    )
  })
})
