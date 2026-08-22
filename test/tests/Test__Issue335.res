open Mocha
open Test__Util

// Regression test for #335: after a case split, an interaction command must act
// on the goal it was invoked in and must never edit text outside that goal.
describe("issue #335: stale interaction points after case split", () => {
  This.timeout(20000)

  let filename = "Issue335.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  let lineAt = (ctx: AgdaMode.t, lineNo) =>
    ctx.state.document->VSCode.TextDocument.lineAt(lineNo)->VSCode.TextLine.text

  Async.it("registers both goals produced by the case split", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)

    await ctx->AgdaMode.case(~cursor=VSCode.Position.make(14, 18), ~payload="m")

    Assert.deepStrictEqual(lineAt(ctx, 14), "+-assoc zero n p = {!   !}")
    Assert.deepStrictEqual(lineAt(ctx, 15), "+-assoc (suc m) n p = {!   !}")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 2)
    Assert.deepStrictEqual(
      Goals.serializeGoals(ctx.state.goals),
      ["#0 [15:20-27)", "#1 [16:23-30)"],
    )
  })

  Async.it("refines the first clause without corrupting the second", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await ctx->AgdaMode.case(~cursor=VSCode.Position.make(14, 18), ~payload="m")
    await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(14, 21), ~payload="refl")

    Assert.deepStrictEqual(lineAt(ctx, 14), "+-assoc zero n p = refl")
    Assert.deepStrictEqual(lineAt(ctx, 15), "+-assoc (suc m) n p = {!   !}")
  })

  // The end-to-end tests above only cover the happy path. These drive the goal
  // registry into the state the issue describes -- an index pointing at a range
  // that is not a goal any more -- and check that nothing is written there.
  Async.it("refuses to edit a goal whose range no longer holds a goal", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let document = ctx.state.document
    let offsetAt = (line, column) =>
      document->VSCode.TextDocument.offsetAt(VSCode.Position.make(line, column))

    // Re-register goal #0 three characters to the left of the real goal, so its
    // range reads "= {!   " instead of "{!   !}".
    ctx.state.goals->Goals.addGoalPositions([(offsetAt(14, 13), offsetAt(14, 23))])
    await ctx.state.goals->Goals.resetGoalIndicesOnLoad(ctx.state.editor, [0])
    Assert.deepStrictEqual(ctx.state.goals->Goals.isIntact(document, 0), false)

    let before = Editor.Text.getAll(document)
    Assert.deepStrictEqual(
      await ctx.state.goals->Goals.modify(document, 0, _ => "refl"),
      false,
    )
    Assert.deepStrictEqual(
      await Goals.removeBoundaryAndDestroy(ctx.state.goals, document, 0),
      false,
    )
    Assert.deepStrictEqual(Editor.Text.getAll(document), before)
  })

  Async.it("still edits a goal whose range does hold a goal", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let document = ctx.state.document

    Assert.deepStrictEqual(ctx.state.goals->Goals.isIntact(document, 0), true)
    Assert.deepStrictEqual(
      await ctx.state.goals->Goals.modify(document, 0, _ => "refl"),
      true,
    )
    Assert.deepStrictEqual(lineAt(ctx, 14), "+-assoc m n p = {! refl !}")
  })

  Async.it("drops every interaction point on reset", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)

    ctx.state.goals->Goals.reset
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 0)
    Assert.deepStrictEqual(ctx.state.goals->Goals.getGoalByIndex(0), None)
    Assert.deepStrictEqual(ctx.state.goals->Goals.getRecentlyCaseSplited, None)
  })
})

// Same scenario as above, but with the exact source from the issue report:
// standard-library imports, non-ASCII identifiers and a trailing space after
// the goal.
describe("issue #335: stale interaction points after case split (unicode)", () => {
  This.timeout(20000)

  let filename = "Issue335Unicode.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  let lineAt = (ctx: AgdaMode.t, lineNo) =>
    ctx.state.document->VSCode.TextDocument.lineAt(lineNo)->VSCode.TextLine.text

  Async.it("refines the first clause without corrupting the second", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)

    await ctx->AgdaMode.case(~cursor=VSCode.Position.make(6, 19), ~payload="m")
    Assert.deepStrictEqual(lineAt(ctx, 6), "+-assoc' zero n p = {!   !}")
    Assert.deepStrictEqual(lineAt(ctx, 7), "+-assoc' (suc m) n p = {!   !} ")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 2)

    await ctx->AgdaMode.execute(Refine, ~cursor=VSCode.Position.make(6, 22), ~payload="refl")
    Assert.deepStrictEqual(lineAt(ctx, 6), "+-assoc' zero n p = refl")
    Assert.deepStrictEqual(lineAt(ctx, 7), "+-assoc' (suc m) n p = {!   !} ")
  })
})
