open Mocha
open Test__Util

// Regression test for #335. A `?` goal is rewritten to `{!   !}` during load,
// and that edit destroys the highlighting token the hole position was derived
// from, so `holePositions` ends up resolved-but-empty. A second load must not
// reuse that empty value: `Tokens.beginLoad` replaces the Resource so the load
// waits for fresh highlighting. Without it every goal is dropped on reload,
// which is the reported symptom -- repeated loads never recover, only
// "Developer: Reload Window" does.
describe("issue #335: goals survive a reload", () => {
  This.timeout(20000)

  let filename = "Issue335Reload.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  let lineAt = (ctx: AgdaMode.t, lineNo) =>
    ctx.state.document->VSCode.TextDocument.lineAt(lineNo)->VSCode.TextLine.text

  Async.it("expands the question mark into a hole", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(lineAt(ctx, 7), "refl' x = {!   !}")
  })

  Async.it("registers the goal on the first load", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)
  })

  Async.it("keeps the goal registered on a second load", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Load)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)
  })

  Async.it("keeps the goal registered across three loads", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Load)
    await ctx->AgdaMode.execute(Load)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)
  })

  Async.it("still finds the goal at the cursor after a reload", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Load)
    Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(7, 12))
    let goal = ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)
    Assert.deepStrictEqual(goal->Option.isSome, true)
  })
})
