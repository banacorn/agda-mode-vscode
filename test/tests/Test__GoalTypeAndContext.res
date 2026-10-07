open Mocha
open Test__Util

let run = normalization => {
  let filename = "GoalTypeAndContext.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.GoalTypeAndContext(
        normalization,
        {
          index: 0,
          indexString: "0",
          start: 281,
          end: 288,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    let delimiter = await AgdaMode.contextDelimiter()
    Assert.deepStrictEqual(
      filteredResponses,
      [DisplayInfo(GoalType("Goal: ℕ\n" ++ delimiter ++ "\nb : Bool\ny : ℕ\nx : ℕ"))],
    )
  })

  // Agda 2.9 labels the line between the goal and the context with "Context" (issue #371).
  // The goal and the context must render the same as with the plain line of older Agda.
  Async.it("should render the goal and the context the same on every Agda version", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.GoalTypeAndContext(
        normalization,
        {
          index: 0,
          indexString: "0",
          start: 281,
          end: 288,
        },
      ),
    )
    await ctx->AgdaMode.quit

    let rendered = responses->Array.filterMap(response =>
      switch response {
      | DisplayInfo(GoalType(body)) =>
        Some(body->Emacs__Parser2.parseGoalType->Emacs__Parser2.render)
      | _ => None
      }
    )
    let plain = "Goal: ℕ\n————————————————————————————————————————————————————————————\nb : Bool\ny : ℕ\nx : ℕ"
    Assert.deepStrictEqual(
      rendered,
      [plain->Emacs__Parser2.parseGoalType->Emacs__Parser2.render],
    )
  })

  Async.it("should work", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.execute(ctx, GoalTypeAndContext(normalization), ~cursor=VSCode.Position.make(15, 26))
    await ctx->AgdaMode.quit
  })
}

describe("agda-mode.goal-type-and-context", () => {
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
