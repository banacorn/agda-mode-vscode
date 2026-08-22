open Mocha
open Test__Util

// Issue #335: Agda answers goal-indexed requests from the interaction points it
// built during the last load, and those requests carry no range for it to
// notice staleness with. So once the buffer has been edited, every command that
// depends on Agda's view of the file has to reload before it can be trusted.
//
// `Command.requiresUpToDateLoad` is the list of such commands, and
// `State__Command.dispatchCommand` acts on it. These tests hold the two
// together: the classification is checked directly, and the commands that can
// run without opening a prompt are checked end to end.
//
//   AGDA_TEST_GLOB="Test__Issue335ReloadBeforeCommand*.js" npm test
describe("issue #335: reloading before commands that depend on the load", () => {
  This.timeout(60000)

  let asset = "Issue335ReloadBeforeCommand.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(asset))))
  Async.afterEach(async () => await File.write(Path.asset(asset), fileContent.contents))

  // inside the hole on the last line
  let cursor = VSCode.Position.make(5, 13)

  describe("Command.requiresUpToDateLoad", () => {
    // Anything that asks Agda about the file. `requiresUpToDateLoad` is a total
    // switch, so a new command cannot be added without answering this question.
    let dependsOnTheLoad = [
      Command.Compile,
      Command.ShowConstraints(Simplified),
      Command.SolveConstraints(Simplified),
      Command.ShowGoals(Simplified),
      Command.SearchAbout(Simplified),
      Command.Give,
      Command.Refine,
      Command.ElaborateAndGive(Simplified),
      Command.Auto(Simplified),
      Command.Case,
      Command.HelperFunctionType(Simplified),
      Command.InferType(Simplified),
      Command.Context(Simplified),
      Command.GoalType(Simplified),
      Command.GoalTypeAndContext(Simplified),
      Command.GoalTypeContextAndInferredType(Simplified),
      Command.GoalTypeContextAndCheckedType(Simplified),
      Command.ModuleContents(Simplified),
      Command.ComputeNormalForm(DefaultCompute),
      Command.WhyInScope,
    ]

    // Reloading for these would either be circular or pure overhead.
    let doesNotDependOnTheLoad = [
      Command.Load,
      Command.Quit,
      Command.Restart,
      Command.Refresh,
      Command.NextGoal,
      Command.PreviousGoal,
      Command.ToggleDisplayOfImplicitArguments,
      Command.ToggleDisplayOfIrrelevantArguments,
      Command.SwitchAgdaVersion,
      Command.Escape,
      Command.InputMethod(Activate),
      Command.LookupSymbol,
      Command.OpenDebugBuffer,
      Command.EventFromView(Initialized),
    ]

    it("holds for every command that asks Agda about the file", () => {
      Assert.deepStrictEqual(
        dependsOnTheLoad->Array.map(Command.requiresUpToDateLoad),
        dependsOnTheLoad->Array.map(_ => true),
      )
    })

    it("holds for every command that does not", () => {
      Assert.deepStrictEqual(
        doesNotDependOnTheLoad->Array.map(Command.requiresUpToDateLoad),
        doesNotDependOnTheLoad->Array.map(_ => false),
      )
    })
  })

  describe("dispatching after an edit", () => {
    let commandsDispatchedDuring = async (ctx: AgdaMode.t, run) => {
      let dispatched = []
      let destructor = ctx.channels.log->Chan.on(log =>
        switch log {
        | Log.CommandDispatched(command) => dispatched->Array.push(command)
        | _ => ()
        }
      )
      await run()
      destructor()
      dispatched
    }

    // Every load-sensitive command that answers without opening a prompt once
    // the goal has a payload in it. `SearchAbout` always prompts, so it is
    // covered by the classification tests alone.
    let commands = [
      Command.Compile,
      Command.ShowConstraints(Simplified),
      Command.SolveConstraints(Simplified),
      Command.ShowGoals(Simplified),
      Command.Give,
      Command.Refine,
      Command.ElaborateAndGive(Simplified),
      Command.Auto(Simplified),
      Command.Case,
      Command.HelperFunctionType(Simplified),
      Command.InferType(Simplified),
      Command.Context(Simplified),
      Command.GoalType(Simplified),
      Command.GoalTypeAndContext(Simplified),
      Command.GoalTypeContextAndInferredType(Simplified),
      Command.GoalTypeContextAndCheckedType(Simplified),
      Command.ModuleContents(Simplified),
      Command.ComputeNormalForm(DefaultCompute),
      Command.WhyInScope,
    ]

    commands->Array.forEach(command =>
      Async.it("reloads before " ++ Command.toString(command), async () => {
        let ctx = await AgdaMode.makeAndLoad(asset)
        // Typing into the goal is itself the edit that invalidates the load.
        let dispatched = await commandsDispatchedDuring(ctx, () =>
          ctx->AgdaMode.execute(command, ~cursor, ~payload="Nat")
        )
        Assert.ok(dispatched->Array.some(dispatched => dispatched == Command.Load))
      })
    )

    Async.it("does not reload when the document is untouched", async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let dispatched = await commandsDispatchedDuring(ctx, () =>
        ctx->AgdaMode.execute(Command.GoalTypeAndContext(Simplified))
      )
      Assert.deepStrictEqual(
        dispatched->Array.some(dispatched => dispatched == Command.Load),
        false,
      )
    })
  })
})
