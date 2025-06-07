open Mocha
open Test__Util

let run = normalization => {
  describe("request to Agda", () => {
    This.timeout(4000)
    describe("global", () => {
      Async.it(
        "should be responded with the correct answer 1",
        async () => {
          let ctx = await AgdaMode.makeAndLoad("Auto.agda")

          let responses = switch Goals.getGoalByIndex(ctx.state.goals2, 0) {
          | Some(goal) =>
            await ctx.state->State__Connection.sendRequestAndCollectResponses(
              Request.Auto(normalization, goal),
            )
          | None => []
          }

          let filteredResponses = responses->Array.filter(filteredResponse)

          switch ctx.state.agdaVersion {
          | Some(version) =>
            if Util.Version.gte(version, "2.7.0") {
              Assert.deepStrictEqual(
                filteredResponses,
                [GiveAction(0, GiveString("n")), InteractionPoints([1])],
              )
            } else {
              Assert.deepStrictEqual(
                filteredResponses,
                [
                  GiveAction(0, GiveString("n")),
                  DisplayInfo(AllGoalsWarnings("*All Goals*", "?1 : ℕ\n")),
                  InteractionPoints([1]),
                ],
              )
            }
          | None => Assert.fail("No Agda version found")
          }
        },
      )

      Async.it(
        "should be responded with the correct answer 2",
        async () => {
          let ctx = await AgdaMode.makeAndLoad("Auto.agda")

          let responses = switch Goals.getGoalByIndex(ctx.state.goals2, 1) {
          | Some(goal) =>
            await ctx.state->State__Connection.sendRequestAndCollectResponses(
              Request.Auto(normalization, goal),
            )
          | None => []
          }

          let filteredResponses = responses->Array.filter(filteredResponse)

          switch ctx.state.agdaVersion {
          | Some(version) =>
            if Util.Version.gte(version, "2.7.0") {
              Assert.deepStrictEqual(
                filteredResponses,
                [GiveAction(1, GiveString("n")), InteractionPoints([0])],
              )
            } else {
              Assert.deepStrictEqual(
                filteredResponses,
                [
                  GiveAction(1, GiveString("m")),
                  DisplayInfo(AllGoalsWarnings("*All Goals*", "?0 : ℕ\n")),
                  InteractionPoints([0]),
                ],
              )
            }
          | None => Assert.fail("No Agda version found")
          }
        },
      )
    })
  })
}

describe_only("agda-mode.auto", () => {
  describe("AsIs", () => {
    run(AsIs)
  })

  describe("Simplified", () => {
    run(Simplified)
  })

  describe("Normalised", () => {
    run(Normalised)
  })

  describe("HeadNormal", () => {
    run(HeadNormal)
  })
})
