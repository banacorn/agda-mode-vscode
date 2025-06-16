open Mocha
open Test__Util

let run = normalization => {
  let filename = "ComputeNormalForm.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

  Async.it("should be responded with correct responses (goal)", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.ComputeNormalForm(
        normalization,
        "Z + S Z",
        {
          index: 0,
          indexString: "0",
          start: 137,
          end: 144,
        },
      ),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [DisplayInfo(NormalForm("S Z"))])
  })

  Async.it("should be responded with correct responses (global)", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let responses =
      await ctx.state->State__Connection.sendRequestAndCollectResponses(
        Request.ComputeNormalFormGlobal(normalization, "Z + S Z"),
      )

    let filteredResponses = responses->Array.filter(filteredResponse)
    Assert.deepStrictEqual(filteredResponses, [DisplayInfo(NormalForm("S Z"))])
  })
}

describe_only("agda-mode.compute-normal-form", () => {
  describe("DefaultCompute", () => {
    run(DefaultCompute)
  })

  describe("IgnoreAbstract", () => {
    run(IgnoreAbstract)
  })

  // describe("UseShowInstance", () => {
  //   run(UseShowInstance)
  // })
})
