open Mocha
open Test__Util

// Issue #335, per the maintainer's reproduction: edit the file after the case
// split has written its `?` clauses but before the follow-up load expands them
// into `{!   !}`.
//
// `MakeCase` writes the new clauses, then calls `dispatchCommand(Load)`. The
// holes only become tokens during that load, so an edit landing inside it is
// rebased against token state that is still being rebuilt. `ClearHighlighting`
// is the first response of that load, which makes the window addressable
// without relying on a timer.
//
//   AGDA_TEST_GLOB="Test__Issue335CaseSplitEdit*.js" npm test
describe("issue #335: an edit during case split", () => {
  This.timeout(60000)

  let asset = "Issue335CaseSplitEdit.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(asset))))
  Async.afterEach(async () => await File.write(Path.asset(asset), fileContent.contents))

  // Splits `m`, applying `edit` right after the first response `trigger` accepts.
  let caseSplitWithEditInFlight = async (ctx: AgdaMode.t, ~trigger, ~edit) => {
    let editApplied = ref(false)
    ctx.state.middlewares
    ->Array.push(handler => async response => {
      await handler(response)
      if !editApplied.contents && trigger(response) {
        editApplied := true
        if !(await edit()) {
          raise(Failure("the edit was rejected"))
        }
      }
    })
    ->ignore

    await ctx->AgdaMode.case(~cursor=VSCode.Position.make(5, 13), ~payload="m")

    ctx.state.middlewares->Array.pop->ignore

    if !editApplied.contents {
      Assert.fail("the trigger response never arrived, so no edit was made during the split")
    }
  }

  // The load that `MakeCase` dispatches opens with `ClearHighlighting`.
  let afterTheSplitWasWritten = response => response == Response.ClearHighlighting

  // Agda answers the `Case` request with `InteractionPoints` before `MakeCase`,
  // and handling it wipes the goal that `MakeCase` is about to rewrite. Editing
  // in that window used to leave `MakeCase` writing through a stale range.
  let beforeTheSplitWasWritten = response =>
    switch response {
    | Response.InteractionPoints(_) => true
    | _ => false
    }

  // A blank line above the clauses shifts every hole offset that follows.
  let insertBlankLine = (ctx: AgdaMode.t) => () =>
    Editor.Text.insert(ctx.state.document, VSCode.Position.make(1, 0), "\n")

  Async.it("splits into two clauses", async () => {
    let ctx = await AgdaMode.makeAndLoad(asset)
    await caseSplitWithEditInFlight(
      ctx,
      ~trigger=afterTheSplitWasWritten,
      ~edit=async () => true,
    )
    Assert.deepStrictEqual(Goals.size(ctx.state.goals), 2)
  })

  Async.it("registers both goals when an insertion lands during the follow-up load", async () => {
    let ctx = await AgdaMode.makeAndLoad(asset)
    await caseSplitWithEditInFlight(
      ctx,
      ~trigger=afterTheSplitWasWritten,
      ~edit=insertBlankLine(ctx),
    )
    Assert.deepStrictEqual(Goals.size(ctx.state.goals), 2)
  })

  Async.it("keeps every registered goal pointing at real hole text", async () => {
    let ctx = await AgdaMode.makeAndLoad(asset)
    await caseSplitWithEditInFlight(
      ctx,
      ~trigger=afterTheSplitWasWritten,
      ~edit=insertBlankLine(ctx),
    )
    let intact =
      [0, 1]->Array.map(index => ctx.state.goals->Goals.isIntact(ctx.state.document, index))
    Assert.deepStrictEqual(intact, [true, true])
  })

  // An edit landing before the clauses are written no longer leaves `MakeCase`
  // writing through a range that has moved: the edit marks the file as dirty,
  // and the split is typechecked against the text that is actually there.
  Async.it("does not corrupt the source when an edit lands before the split is written", async () => {
    let ctx = await AgdaMode.makeAndLoad(asset)
    await caseSplitWithEditInFlight(
      ctx,
      ~trigger=beforeTheSplitWasWritten,
      ~edit=insertBlankLine(ctx),
    )

    let text = Editor.Text.getAll(ctx.state.document)
    // The split lands on the real clause rather than at the goal's old offsets.
    Assert.ok(text->String.includes("double zero = {!   !}"))
    Assert.ok(text->String.includes("double (suc m) = {!   !}"))
    // The edit itself survives, and nothing above the clause was overwritten.
    Assert.ok(text->String.includes("module Issue335CaseSplitEdit where"))
    Assert.ok(text->String.includes("open import Agda.Builtin.Nat"))
    Assert.ok(text->String.includes("double : Nat \u2192 Nat"))
  })
})
