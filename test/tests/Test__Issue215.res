open Mocha
open Test__Util

// https://github.com/banacorn/agda-mode-vscode/issues/215
describe("issue #215: Auto must not remove the following hole", () => {
  This.timeout(OS.onUnix ? 4000 : 20000)

  let filename = "Issue215.lagda.md"
  let original = ref("")

  Async.beforeEach(async () => original := await File.read(Path.asset(filename)))
  Async.afterEach(async () => await File.write(Path.asset(filename), original.contents))

  Async.it("should keep the second hole after Auto fills the first with a multiline term", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 2)

    await ctx->AgdaMode.execute(Auto(AsIs), ~cursor=VSCode.Position.make(16, 28))

    Assert.deepStrictEqual(
      // Windows checkouts and VS Code use CRLF
      Editor.Text.getAll(ctx.state.document)->String.replaceAll("\r\n", "\n"),
      [
        "# Issue 215 reproduction",
        "",
        "```agda",
        "module Issue215 where",
        "",
        "data Carrier : Set where",
        "  first-long-carrier-element-name : Carrier",
        "  second-long-carrier-element-name : Carrier",
        "",
        "data Target : Set where",
        "  make-target-from-two-long-carrier-elements : Carrier → Carrier → Target",
        "",
        "pair-of-targets : Target → Target → Target",
        "pair-of-targets t _ = t",
        "",
        "result : Target",
        "result = pair-of-targets (make-target-from-two-long-carrier-elements",
        "   first-long-carrier-element-name first-long-carrier-element-name) {!   !}",
        "```",
        "",
      ]->Array.join("\n"),
    )
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)

    await ctx->AgdaMode.quit
  })
})
