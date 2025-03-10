open Mocha
open Test__Util

let run = normalization =>
  describe("request to Agda", () => {
    describe("global", () => {
      Async.it(
        "should be responded with the correct answer",
        async () => {
          let ctx = await AgdaMode.makeAndLoad("ComputeNormalForm.agda")

          let responses =
            await ctx.state->State__Request.sendRequestAndCollectResponses(
              Request.ComputeNormalFormGlobal(normalization, "Z + S Z"),
            )
          let filteredResponses = responses->Array.filter(filteredResponse)

          Assert.deepEqual(filteredResponses, [DisplayInfo(NormalForm("S Z"))])
        },
      )

      Async.it(
        "should be responded with the correct answer",
        async () => {
          let ctx = await AgdaMode.makeAndLoad("ComputeNormalForm.agda")

          let responses =
            await ctx.state->State__Request.sendRequestAndCollectResponses(
              Request.ComputeNormalFormGlobal(normalization, "S Z + S Z"),
            )
          let filteredResponses = responses->Array.filter(filteredResponse)

          Assert.deepEqual(filteredResponses, [DisplayInfo(NormalForm("S (S Z)"))])
        },
      )
    })
  })

describe("agda-mode.compute-normal-form[DefaultCompute]", () => {
  run(DefaultCompute)
})

describe("agda-mode.compute-normal-form[IgnoreAbstract]", () => {
  run(IgnoreAbstract)
})
