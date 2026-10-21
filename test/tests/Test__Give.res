open Mocha
open Test__Util

describe("agda-mode.give", () => {
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Give.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Give.agda"), fileContent.contents))

  Async.it("should be responded with correct responses", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")

    let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(7, 14), "y")
    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.Give({
        index: 0,
        indexString: "0",
        start: 91,
        end: 98,
      }),
    )

    let filteredResponses = responses->Array.filter(filteredResponse)
    // Agda 2.9 no longer ends the goal list with a line break
    let goals = (await AgdaMode.versionGTE("agda", "2.9.0")) ? "?1 : ℕ" : "?1 : ℕ\n"
    Assert.deepStrictEqual(
      filteredResponses,
      [
        GiveAction(0, GiveNoParen),
        DisplayInfo(AllGoalsWarnings("*All Goals*", goals)),
        InteractionPoints([1]),
      ],
    )
  })

  Async.it("should remove the given goal", async () => {
    let ctx = await AgdaMode.makeAndLoad("Give.agda")
    await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 14), ~payload="y")
    Assert.deepStrictEqual(ctx.state.goals->Goals.size, 1)

    await ctx->AgdaMode.quit
    let actual = await File.read(Path.asset("Give.agda"))
    let expected = await File.read(Path.asset("Give.agda.out"))
    Assert.deepStrictEqual(actual, expected)
  })

  // Test case for GitHub issue #249: give command bug when used twice without reloading
  describe("Issue #249: consecutive give commands", () => {
    let filename = "Issue249.agda"
    let fileContent = ref("")
    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it(
      "should handle consecutive give commands without infinite loops or disabled goals",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)

        // First give command: give "? , ?"
        await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 9), ~payload="? , ?") // position of the goal ?

        // Verify that two new goals were created
        Assert.ok(ctx.state.goals->Goals.size >= 2)

        // Second give command: give "tt" to the first goal without reloading
        await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 9), ~payload="tt") // position of first goal

        // compare file content before and after
        let actual = await File.read(Path.asset(filename))
        let expected = await File.read(Path.asset(filename ++ ".out"))
        Assert.deepStrictEqual(actual, expected)

        await ctx->AgdaMode.quit
      },
    )
  })

  // Issue #77: the given expression should be highlighted right after Give, without reloading
  describe("Issue #77: highlighting of the given expression", () => {
    let filename = "Issue77.agda"
    let fileContent = ref("")
    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    let readTokens = async (ctx: AgdaMode.t) =>
      (await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get)->Array.map(
        Highlighting__SemanticToken.toString,
      )

    // tokens of Issue77.agda outside of the two holes
    let tokensBeforeGive = [
      "(0:0-6) keyword [agda]",
      "(0:7-14) namespace [agda]",
      "(0:15-20) keyword [agda]",
      "(2:0-4) keyword [agda]",
      "(2:5-6) type [agda]",
      "(2:9-12) type [primitive, agda]",
      "(2:13-18) keyword [agda]",
      "(3:2-6) enumMember [agda]",
      "(3:9-10) type [agda]",
      "(4:2-5) enumMember [agda]",
      "(4:8-9) type [agda]",
      "(4:12-13) type [agda]",
      "(6:0-3) function [agda]",
      "(6:6-7) type [agda]",
      "(6:10-11) type [agda]",
      "(6:14-15) type [agda]",
      "(7:0-4) enumMember [agda]",
      "(7:5-6) function [agda]",
      "(7:7-8) variable [agda]",
      "(8:0-3) enumMember [agda]",
      "(8:4-5) variable [agda]",
      "(8:6-7) function [agda]",
      "(8:8-9) variable [agda]",
      "(8:12-15) enumMember [agda]",
    ]

    // "zero + y = {!   !}" becomes "zero + y = suc y"
    // no enumMember token on "suc": see Highlighting__AgdaStaleNameWorkaround
    let tokensAfterGiveNoParen = [
      ...tokensBeforeGive->Array.slice(~start=0, ~end=19),
      "(7:15-16) variable [agda]",
      ...tokensBeforeGive->Array.sliceToEnd(~start=19),
    ]

    // "suc x + y = suc {!   !}" becomes "suc x + y = suc (x + y)"
    let tokensAfterGiveParen = [
      ...tokensBeforeGive,
      "(8:17-18) variable [agda]",
      "(8:19-20) function [agda]",
      "(8:21-22) variable [agda]",
    ]

    Async.it("should highlight the expression right after Give returns GiveNoParen", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 14), ~payload="suc y")

      Assert.deepStrictEqual(
        Editor.Text.getAll(ctx.state.document)
        ->String.splitByRegExp(%re("/\r?\n/"))
        ->Array.get(7)
        ->Option.flatMap(line => line),
        Some("zero + y = suc y"),
      )
      Assert.deepStrictEqual(await readTokens(ctx), tokensAfterGiveNoParen)
    })

    Async.it("should highlight the expression right after Give returns GiveParen", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(8, 19), ~payload="x + y")

      Assert.deepStrictEqual(
        Editor.Text.getAll(ctx.state.document)
        ->String.splitByRegExp(%re("/\r?\n/"))
        ->Array.get(8)
        ->Option.flatMap(line => line),
        Some("suc x + y = suc (x + y)"),
      )
      Assert.deepStrictEqual(await readTokens(ctx), tokensAfterGiveParen)
    })

    Async.it(
      "should highlight the expression right after Give when Give goes through temporary files and Cmd_highlight through Direct",
      async () => {
        let setIPC = value =>
          VSCode.Workspace.getConfiguration(
            Some("agdaMode"),
            None,
          )->VSCode.WorkspaceConfiguration.updateGlobalSettings("highlighting.IPC", value, None)
        await setIPC(Some("Temporary Files"))
        let run = async () => {
          let ctx = await AgdaMode.makeAndLoad(filename)
          await AgdaMode.execute(ctx, Give, ~cursor=VSCode.Position.make(7, 14), ~payload="suc y")
          await readTokens(ctx)
        }
        switch await run() {
        | tokens =>
          await setIPC(None)
          Assert.deepStrictEqual(tokens, tokensAfterGiveNoParen)
        | exception exn =>
          await setIPC(None)
          raise(exn)
        }
      },
    )
  })
})
