open Mocha
open Test__Util

describe("agda-mode.auto[AsIs]", () => {
  describe("request to Agda", () => {
    describe(
      "global",
      () => {
        Async.it(
          "should be responded with the correct answer 1",
          async () => {
            let ctx = await AgdaMode.make("Auto.agda")
            let state = await ctx->AgdaMode.load

            let responses = ref([])
            let responseHandler = async response => responses.contents->Array.push(response)

            switch state.goals[0] {
            | Some(goal) =>
              await state->State.sendRequest(responseHandler, Request.Auto(AsIs, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(0, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([1]),
              ],
            )
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
              await state->State.sendRequest(responseHandler, Request.Auto(AsIs, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(1, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([0]),
              ],
            )
          },
        )
      },
    )
  })
})

describe("agda-mode.auto[Simplified]", () => {
  describe("request to Agda", () => {
    describe(
      "global",
      () => {
        Async.it(
          "should be responded with the correct answer 1",
          async () => {
            let ctx = await AgdaMode.make("Auto.agda")
            let state = await ctx->AgdaMode.load

            let responses = ref([])
            let responseHandler = async response => responses.contents->Array.push(response)

            switch state.goals[0] {
            | Some(goal) =>
              await state->State.sendRequest(responseHandler, Request.Auto(Simplified, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(0, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([1]),
              ],
            )
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
              await state->State.sendRequest(responseHandler, Request.Auto(Simplified, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(1, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([0]),
              ],
            )
          },
        )
      },
    )
  })
})

describe("agda-mode.auto[Normalised]", () => {
  describe("request to Agda", () => {
    describe(
      "global",
      () => {
        Async.it(
          "should be responded with the correct answer 1",
          async () => {
            let ctx = await AgdaMode.make("Auto.agda")
            let state = await ctx->AgdaMode.load

            let responses = ref([])
            let responseHandler = async response => responses.contents->Array.push(response)

            switch state.goals[0] {
            | Some(goal) =>
              await state->State.sendRequest(responseHandler, Request.Auto(Normalised, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(0, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([1]),
              ],
            )
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
              await state->State.sendRequest(responseHandler, Request.Auto(Normalised, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(1, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([0]),
              ],
            )
          },
        )
      },
    )
  })
})

describe("agda-mode.auto[HeadNormal]", () => {
  describe("request to Agda", () => {
    describe(
      "global",
      () => {
        Async.it(
          "should be responded with the correct answer 1",
          async () => {
            let ctx = await AgdaMode.make("Auto.agda")
            let state = await ctx->AgdaMode.load

            let responses = ref([])
            let responseHandler = async response => responses.contents->Array.push(response)

            switch state.goals[0] {
            | Some(goal) =>
              await state->State.sendRequest(responseHandler, Request.Auto(HeadNormal, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(0, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([1]),
              ],
            )
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
              await state->State.sendRequest(responseHandler, Request.Auto(HeadNormal, goal))
            | None => Assert.fail("No goals found")
            }

            Assert.deepEqual(
              responses.contents,
              [
                GiveAction(1, GiveString("n")),
                CompleteHighlightingAndMakePromptReappear,
                InteractionPoints([0]),
              ],
            )
          },
        )
      },
    )
  })
})