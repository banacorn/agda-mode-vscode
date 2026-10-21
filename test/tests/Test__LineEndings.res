open Mocha
open Test__Util

// New goals from Give and Refine are located by offsets computed from the
// inserted text. Those offsets must count line breaks as the document stores
// them, not as the host OS writes them.
//
// Each fixture is written to disk with explicit line endings before VS Code
// opens it, so neither the host OS nor the checkout decides them.
//
//   AGDA_TEST_GLOB="Test__LineEndings.bs.js" npm test
describe("Line endings of the document", () => {
  let lines = [
    "module ModuleName where",
    "",
    "record Long : Set₁ where",
    "  field",
    "    very-long-field-name-1 : Set",
    "    very-long-field-name-2 : Set",
    "    very-long-field-name-3 : Set",
    "",
    "long : Long",
    "long = {!   !}",
    "",
  ]

  let cases = [("LF", "\n", "RefineLF"), ("CRLF", "\r\n", "RefineCRLF")]

  cases->Array.forEach(((name, eol, moduleName)) => {
    describe(name, () => {
      let filename = moduleName ++ ".agda"

      Async.beforeEach(async () => {
        let content =
          lines
          ->Array.map(line => line->String.replace("ModuleName", moduleName))
          ->Array.join(eol)
        NodeJs.Fs.writeFileSync(Path.asset(filename), NodeJs.Buffer.fromString(content))
      })

      Async.afterEach(async () => {
        await Registry.removeAndDestroyAll()
        NodeJs.Fs.unlinkSync(Path.asset(filename))
      })

      Async.it(
        "Refine: a multiline record expression with three new goals",
        async () => {
          let ctx = await AgdaMode.makeAndLoad(filename)
          await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(9, 9))

          Assert.deepStrictEqual(
            Editor.Text.getAll(ctx.state.document),
            [
              "module " ++ moduleName ++ " where",
              "",
              "record Long : Set₁ where",
              "  field",
              "    very-long-field-name-1 : Set",
              "    very-long-field-name-2 : Set",
              "    very-long-field-name-3 : Set",
              "",
              "long : Long",
              "long = record",
              "  { very-long-field-name-1 = {!   !}",
              "  ; very-long-field-name-2 = {!   !}",
              "  ; very-long-field-name-3 = {!   !}",
              "  }",
              "",
            ]->Array.join(eol),
          )
          Assert.deepStrictEqual(
            Goals.serializeGoals(ctx.state.goals),
            ["#1 [11:30-37)", "#2 [12:30-37)", "#3 [13:30-37)"],
          )
        },
      )
    })
  })
})
