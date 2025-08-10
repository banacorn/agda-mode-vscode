open Mocha
open Test__Util

let run = normalization => {
  Async.it("should be responded with correct responses 1", async () => {
    let ctx = await AgdaMode.makeAndLoad("Auto.agda")

    let responses = switch Goals.getGoalByIndex(ctx.state.goals, 0) {
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
  })

  Async.it("should be responded with correct responses 2", async () => {
    let ctx = await AgdaMode.makeAndLoad("Auto.agda")

    let responses = switch Goals.getGoalByIndex(ctx.state.goals, 1) {
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
  })
}

describe("agda-mode.auto", () => {
  This.timeout(4000)

  // testing for Auto somehow hangs on GitHub Actions Windows runners (but works fine on my desktop Windows :))
  // cannot find out why, so skip the tests on Windows for now
  if OS.onUnix {
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
  }
})
