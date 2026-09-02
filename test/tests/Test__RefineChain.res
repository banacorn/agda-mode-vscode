open Mocha
open Test__Util

// #346: refining a hole with a 5-field constructor spawns subgoals #1-#5.
// Refining one of those subgoals with a plain in-scope variable (a trivial
// "give", no further subgoals) removes it and shifts everything after it
// left by 6 characters (`{!   !}` -> `i`). Every remaining goal after the
// given one must shift by that same -6 delta. (Needs >= 5 fields to
// trigger the bug this guards against; 4 or fewer did not reproduce it.)
//
//   AGDA_TEST_GLOB="Test__RefineChain*.js" npm test
describe("refine chain: goal tracking survives refining a subgoal", () => {
  This.timeout(20000)

  let filename = "RefineChain.agda"
  let fileContent = ref("")

  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
  Async.afterEach(async () => {
    await File.write(Path.asset(filename), fileContent.contents)
    await Registry.removeAndDestroyAll()
  })

  Async.it(
    "refining #0 with 'node' then refining subgoal #1 with 'i' keeps every remaining goal's range in sync",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)

      Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [11:9-16)"])

      // Step 1: place `node` inside #0 and refine it. `node` is `R`'s only
      // constructor (5 `A` fields), so this spawns 5 new subgoals.
      await ctx->AgdaMode.execute(Refine, ~payload="node", ~cursor=VSCode.Position.make(10, 11))

      Assert.deepStrictEqual(
        Goals.serializeGoals(ctx.state.goals),
        ["#1 [11:14-21)", "#2 [11:22-29)", "#3 [11:30-37)", "#4 [11:38-45)", "#5 [11:46-53)"],
      )

      // Step 2: place `i` inside #1 and refine it. `i : A` matches the
      // field's type exactly, so this is a plain give with no new subgoals --
      // #1 disappears and everything after it in the line shifts left by 6
      // characters (`{!   !}` -> `i`).
      switch Goals.getGoalPositionByIndex(ctx.state.goals, 1) {
      | None => Assert.fail("expected goal #1 (the first new subgoal) to exist")
      | Some(start, _end) =>
        let pos = VSCode.TextDocument.positionAt(ctx.state.document, start + 2)
        await ctx->AgdaMode.execute(Refine, ~payload="i", ~cursor=pos)
      }

      let text = Editor.Text.getAll(ctx.state.document)
      Assert.deepStrictEqual(
        text->String.replaceAll("\r", "")->String.split("\n")->Array.getUnsafe(10),
        "foo i = node i {!   !} {!   !} {!   !} {!   !}",
      )

      // Correct outcome: #2-#5 each shift left by 6 from their step-1
      // ranges, landing exactly on the hole each one actually sits over.
      Assert.deepStrictEqual(
        Goals.serializeGoals(ctx.state.goals),
        ["#2 [11:16-23)", "#3 [11:24-31)", "#4 [11:32-39)", "#5 [11:40-47)"],
      )
    },
  )
})
