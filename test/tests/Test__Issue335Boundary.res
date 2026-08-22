open Mocha
open Test__Util

// Regression test for #335, from a field report: refining an empty hole whose
// goal is a path introduces `λ i → ?`, and the resulting hole carries unsolved
// boundary constraints. Afterwards the goal must still be registered, or the
// user is left with an undecorated hole that every command refuses to act on.
describe("issue #335: refining a path goal", () => {
  This.timeout(20000)

  let filename = "Issue335Boundary.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  let lineAt = (ctx: AgdaMode.t, lineNo) =>
    ctx.state.document->VSCode.TextDocument.lineAt(lineNo)->VSCode.TextLine.text

  Async.it("registers the goal on load", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)
  })

  Async.it("introduces the interval binder", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(7, 12))
    Assert.deepStrictEqual(lineAt(ctx, 7), "refl' x = λ i → {!   !}")
  })

  Async.it("keeps the goal registered after refining", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(7, 12))
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)
  })

  Async.it("finds the goal at a cursor inside the refined hole", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(7, 12))
    Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(7, 18))
    let goal = ctx.state.goals->Goals.getGoalAtCursor(ctx.state.editor)
    Assert.deepStrictEqual(goal->Option.isSome, true)
  })

  // The field report failed here, on the Give that follows the refine.
  Async.it("gives into the hole the refine produced", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(7, 12))
    await ctx->AgdaMode.execute(Give, ~cursor=VSCode.Position.make(7, 18), ~payload="x")
    Assert.deepStrictEqual(lineAt(ctx, 7), "refl' x = λ i → x")
  })
})
