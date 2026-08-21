open Mocha
open Test__Util

// #335: after a case split, the first hole can end up with no `0` decoration.
//
// A case split writes `?` for each new hole and reloads. `scanAllGoals` is what
// widens each `?` into `{!   !}` and re-ranges its goal; a goal whose range is
// still 1 character wide is given no decoration at all
// (`InternalGoal.make`, `src/Goals.res:172`). The widening is skipped whenever
// the scanner is already busy (`src/Goals.res:844`), so a document change that
// arrives inside that window can leave a `?` unwidened — and its goal
// undecorated.
//
//   AGDA_TEST_GLOB="Test__Issue335*.js" npm test
describe("issue #335: missing goal decoration after a case split", () => {
  This.timeout(20000)

  let filename = "Issue335.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  // Both goals decorated is both goals wider than one character.
  let undecoratedGoals = (goals, document) =>
    goals
    ->Goals.serializeGoals
    ->Array.filterWithIndex((_, i) =>
      switch Goals.getGoalPositionByIndex(goals, i) {
      | Some(start, end) => end - start == 1
      | None => false
      }
    )
    ->Array.map(serialized => (
      serialized,
      switch Goals.getGoalPositionByIndex(goals, 0) {
      | Some(start, end) =>
        Editor.Text.get(
          document,
          VSCode.Range.make(
            VSCode.TextDocument.positionAt(document, start),
            VSCode.TextDocument.positionAt(document, end),
          ),
        )
      | None => ""
      }
    ))

  Async.it("undisturbed: both new goals are widened and decorated", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(19, 20), ~payload="m")

    Assert.deepStrictEqual(
      Goals.serializeGoals(ctx.state.goals),
      ["#0 [20:21-28)", "#1 [21:24-31)"],
    )
    Assert.deepStrictEqual(undecoratedGoals(ctx.state.goals, ctx.state.document), [])
  })

  Async.it("an edit racing the widening leaves a goal undecorated", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    // `InteractionPoints` is the response that registers the new goals and then
    // widens each `?` into `{!   !}`. Starting the handler without awaiting it
    // puts an edit of ours in flight against that widening, which is what a
    // keystroke lands as on a machine slow enough to type on. The edit is
    // appended past every goal, so nothing it does shifts them.
    // The `Case` request answers with `InteractionPoints` of its own, before
    // `MakeCase`. The one that matters is the reload's, after it.
    let sawMakeCase = ref(false)
    let raced = ref(false)
    ctx.state.middlewares
    ->Array.push(handler => async response => {
      switch response {
      | Response.MakeCase(_, _) =>
        sawMakeCase := true
        await handler(response)
      | Response.InteractionPoints(_) if sawMakeCase.contents && !raced.contents =>
        raced := true
        let handling = handler(response)
        let lastLine = VSCode.TextDocument.lineCount(ctx.state.document)
        let _ = await Editor.Text.insert(
          ctx.state.document,
          VSCode.Position.make(lastLine, 0),
          "-- perturbation\n",
        )
        await handling
      | _ => await handler(response)
      }
    })
    ->ignore

    await AgdaMode.case(ctx, ~cursor=VSCode.Position.make(19, 20), ~payload="m")
    ctx.state.middlewares->Array.pop->ignore
    if !raced.contents {
      Assert.fail("the reload's InteractionPoints was never seen, so nothing raced the widening")
    }

    Js.log("goals after case split: " ++ Goals.serializeGoals(ctx.state.goals)->Array.join(", "))
    Js.log(
      "undecorated: " ++
      undecoratedGoals(ctx.state.goals, ctx.state.document)
      ->Array.map(((serialized, text)) => serialized ++ " over " ++ text)
      ->Array.join(", "),
    )

    // A goal one character wide is a goal `InternalGoal.make` gave no
    // decoration to.
    Assert.deepStrictEqual(undecoratedGoals(ctx.state.goals, ctx.state.document), [])
    Assert.deepStrictEqual(
      Goals.serializeGoals(ctx.state.goals),
      ["#0 [20:21-28)", "#1 [21:24-31)"],
    )
    // A `?` left in the source is the widening that went missing.
    let text = Editor.Text.getAll(ctx.state.document)
    Assert.deepStrictEqual(
      text->String.replaceAll("\r", "")->String.split("\n")->Array.sliceToEnd(~start=19)->Array.slice(~start=0, ~end=2),
      ["+-assoc' zero n p = {!   !}", "+-assoc' (suc m) n p = {!   !}"],
    )
  })
})
