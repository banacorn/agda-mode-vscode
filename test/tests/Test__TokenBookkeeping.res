open Mocha
open Test__Util

describe(
  "Token Bookkeeping
",
  () => {
    let filename = "Issue180.agda"
    let fileContent = ref("")
    Async.beforeEach(async () => fileContent := (await File.read(Path.asset(filename))))
    Async.afterEach(async () => await File.write(Path.asset(filename), fileContent.contents))

    Async.it("should work after inserting a newline", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(6, 0), "\n")

      let expected = [
        "(7:0-3) function",
        "(7:6-7) type",
        "(7:10-11) type",
        "(7:14-15) type",
        "(8:0-1) variable",
        "(8:2-3) function",
        "(8:4-5) variable",
      ]

      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      let actual =
        tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(actual, expected)
    })

    Async.it("should work after deleting an empty line", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)

      let _ = await Editor.Text.delete(
        ctx.state.document,
        VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(6, 0)),
      )

      let expected = [
        "(5:0-3) function",
        "(5:6-7) type",
        "(5:10-11) type",
        "(5:14-15) type",
        "(6:0-1) variable",
        "(6:2-3) function",
        "(6:4-5) variable",
      ]
      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      let actual =
        tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(actual, expected)
    })

    Async.it("should work after deleting an existing line", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      let _ = await Editor.Text.delete(
        ctx.state.document,
        VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(7, 0)),
      )

      let expected = ["(5:0-1) variable", "(5:2-3) function", "(5:4-5) variable"]
      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      let actual =
        tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(actual, expected)
    })

    Async.it_only("should remove Tokens after Command.Quit", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      let tokensBefore = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      Js.Console.log("Test: Before quit, have " ++ string_of_int(Array.length(tokensBefore)) ++ " tokens")
      
      Js.Console.log("Test: Starting quit operation")
      await ctx->AgdaMode.quit
      Js.Console.log("Test: Quit operation completed")
      
      Js.Console.log("Test: Getting tokens from context after quit")
      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      Js.Console.log("Test: After quit, context has " ++ string_of_int(Array.length(tokens)) ++ " tokens")
      Assert.deepStrictEqual(tokens->Array.length, 0)
    })

    Async.it("should update tokens & decorations after each Command.Load", async () => {
      // helper function to filter tokens with TerminationProblem aspect
      let filterTerminationProblem = Array.filter(
        _,
        token =>
          token.Tokens.Token.aspects->Array.some(
            aspect => aspect == Highlighting__AgdaAspect.TerminationProblem,
          ),
      )

      let ctx = await AgdaMode.makeAndLoad(filename)
      // should start with no tokens with `TerminationProblem`
      let tokensWithTerminationProblem =
        ctx.state.tokens->Tokens.toTokenArray->filterTerminationProblem
      Assert.deepStrictEqual(tokensWithTerminationProblem->Array.length, 0)
      // should result no decorations
      let decorationRanges =
        ctx.state.tokens->Tokens.toDecorations->Map.values->Iterator.toArray->Array.flat
      Assert.deepStrictEqual(decorationRanges->Array.length, 0)

      // Replace the line
      //  x + y = {!   !}
      // with
      //  x + y = x + y
      // to create tokens with `TerminationProblem`
      let _ = await Editor.Text.replace(
        ctx.state.document,
        VSCode.Range.make(VSCode.Position.make(7, 8), VSCode.Position.make(7, 15)),
        "x + y",
      )
      // should result in 2 tokens with TerminationProblem
      await ctx->AgdaMode.execute(Load)
      let tokensWithTerminationProblem =
        ctx.state.tokens->Tokens.toTokenArray->filterTerminationProblem
      Assert.deepStrictEqual(tokensWithTerminationProblem->Array.length, 2)
      // should result in 2 decorations
      let decorationRanges =
        ctx.state.tokens->Tokens.toDecorations->Map.values->Iterator.toArray->Array.flat
      Assert.deepStrictEqual(decorationRanges->Array.length, 2)

      // Restore the line
      //  x + y = x + y
      // back to
      //  x + y = {!   !}
      let _ = await Editor.Text.replace(
        ctx.state.document,
        VSCode.Range.make(VSCode.Position.make(7, 8), VSCode.Position.make(7, 13)),
        "{!   !}",
      )
      // should result in no tokens with TerminationProblem
      await ctx->AgdaMode.execute(Load)
      let tokensWithTerminationProblem =
        ctx.state.tokens->Tokens.toTokenArray->filterTerminationProblem
      Assert.deepStrictEqual(tokensWithTerminationProblem->Array.length, 0)
      // should result no decorations
      let decorationRanges =
        ctx.state.tokens->Tokens.toDecorations->Map.values->Iterator.toArray->Array.flat
      Assert.deepStrictEqual(decorationRanges->Array.length, 0)
    })
  },
)
