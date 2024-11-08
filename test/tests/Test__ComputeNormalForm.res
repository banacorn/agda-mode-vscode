open Mocha
open Test__Util

let run = normalization =>
    describe("request to Agda", () => {
      describe(
        "global",
        () => {
          Async.it(
            "should be responded with the correct answer",
            async () => {
              let ctx = await AgdaMode.make("ComputeNormalForm.agda")
              let state = await ctx->AgdaMode.load

              let responses = ref([])
              let responseHandler = async response => responses.contents->Array.push(response)
              await state->State.sendRequest(
                responseHandler,
                Request.ComputeNormalFormGlobal(normalization, "Z + S Z"),
              )

              Assert.deepEqual(
                responses.contents,
                [
                  Status(false, false),
                  DisplayInfo(NormalForm("S Z")),
                  CompleteHighlightingAndMakePromptReappear,
                ],
              )
            },
          )

          Async.it(
            "should be responded with the correct answer",
            async () => {
              let ctx = await AgdaMode.make("ComputeNormalForm.agda")
              let state = await ctx->AgdaMode.load

              let responses = ref([])
              let responseHandler = async response => responses.contents->Array.push(response)
              await state->State.sendRequest(
                responseHandler,
                Request.ComputeNormalFormGlobal(normalization, "S Z + S Z"),
              )

              Assert.deepEqual(
                responses.contents,
                [
                  Status(false, false),
                  DisplayInfo(NormalForm("S (S Z)")),
                  CompleteHighlightingAndMakePromptReappear,
                ],
              )
            },
          )
        },
      )
    })


describe("agda-mode.compute-normal-form[DefaultCompute]", () => {
  run(DefaultCompute)
})

describe("agda-mode.compute-normal-form[IgnoreAbstract]", () => {
  run(IgnoreAbstract)
})