open Mocha
open Test__Util

let run = normalization => {
  describe("request to Agda", () => {
    describe("global", () => {
      Async.it(
        "should be responded with the correct answer 1",
        async () => {
          let ctx = await AgdaMode.make("Auto.agda")
          let state = await ctx->AgdaMode.load

          let responses = ref([])
          let responseHandler = async response => responses.contents->Array.push(response)

          switch state.goals[0] {
          | Some(goal) =>
            await state->State.sendRequest(responseHandler, Request.Auto(normalization, goal))
          | None => Assert.fail("No goals found")
          }

          switch state.agdaVersion {
          | Some(version) =>
            if Util.Version.gte(version, "2.7.0") {
              Assert.deepEqual(
                responses.contents,
                [
                  GiveAction(0, GiveString("n")),
                  CompleteHighlightingAndMakePromptReappear,
                  InteractionPoints([1]),
                ],
              )
            } else {
              Assert.deepEqual(
                responses.contents,
                [
                  GiveAction(0, GiveString("n")),
                  Status(false, false),
                  DisplayInfo(AllGoalsWarnings("*All Goals*", "?1 : ℕ\n")),
                  CompleteHighlightingAndMakePromptReappear,
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
          let ctx = await AgdaMode.make("Auto.agda")
          let state = await ctx->AgdaMode.load

          let responses = ref([])
          let responseHandler = async response => responses.contents->Array.push(response)

          switch state.goals[1] {
          | Some(goal) =>
            await state->State.sendRequest(responseHandler, Request.Auto(normalization, goal))
          | None => Assert.fail("No goals found")
          }

          switch state.agdaVersion {
          | Some(version) =>
            if Util.Version.gte(version, "2.7.0") {
              Assert.deepEqual(
                responses.contents,
                [
                  GiveAction(1, GiveString("n")),
                  CompleteHighlightingAndMakePromptReappear,
                  InteractionPoints([0]),
                ],
              )
            } else {
              Assert.deepEqual(
                responses.contents,
                [
                  GiveAction(1, GiveString("m")),
                  Status(false, false),
                  DisplayInfo(AllGoalsWarnings("*All Goals*", "?0 : ℕ\n")),
                  CompleteHighlightingAndMakePromptReappear,
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

describe("agda-mode.auto[AsIs]", () => {
  run(AsIs)
})

describe("agda-mode.auto[Simplified]", () => {
  run(Simplified)
})

describe("agda-mode.auto[Normalised]", () => {
  run(Normalised)
})

describe("agda-mode.auto[HeadNormal]", () => {
  run(HeadNormal)
})
