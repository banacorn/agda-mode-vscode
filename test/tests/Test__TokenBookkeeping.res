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

    Async.it("should track token positions correctly after two sequential inserts", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)

      // First insert: push the _+_ type signature and body from lines 6-7 to lines 7-8
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(6, 0), "\n")

      let tokensAfterFirstInsert =
        (await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get)
        ->Array.sliceToEnd(~start=12)
        ->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(
        tokensAfterFirstInsert,
        [
          "(7:0-3) function [agda]",
          "(7:6-7) type [agda]",
          "(7:10-11) type [agda]",
          "(7:14-15) type [agda]",
          "(8:0-1) variable [agda]",
          "(8:2-3) function [agda]",
          "(8:4-5) variable [agda]",
        ],
      )

      // Second insert: push them again from lines 7-8 to lines 8-9
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(7, 0), "\n")

      let tokensAfterSecondInsert =
        (await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get)
        ->Array.sliceToEnd(~start=12)
        ->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(
        tokensAfterSecondInsert,
        [
          "(8:0-3) function [agda]",
          "(8:6-7) type [agda]",
          "(8:10-11) type [agda]",
          "(8:14-15) type [agda]",
          "(9:0-1) variable [agda]",
          "(9:2-3) function [agda]",
          "(9:4-5) variable [agda]",
        ],
      )
    })

    Async.it("should work after inserting a newline", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(6, 0), "\n")

      let expected = [
        "(7:0-3) function [agda]",
        "(7:6-7) type [agda]",
        "(7:10-11) type [agda]",
        "(7:14-15) type [agda]",
        "(8:0-1) variable [agda]",
        "(8:2-3) function [agda]",
        "(8:4-5) variable [agda]",
      ]

      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      let actual =
        tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(actual, expected)
    })

    Async.it(
      "should remove tokens for deleted source and keep remaining positions correct",
      async () => {
        let ctx = await AgdaMode.makeAndLoad(filename)

        // Baseline: 19 tokens total; indices 12-15 are the _+_ type signature on line 6,
        // indices 16-18 are the body (x + y) on line 7.
        let baseline = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
        Assert.deepStrictEqual(baseline->Array.length, 19)

        // Delete the body line (line 7: "x + y = {!   !}").
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(7, 0), VSCode.Position.make(8, 0)),
        )

        // The 3 body tokens (x, +, y on line 7) must be gone; only the type-signature
        // tokens from line 6 remain from index 12 onward.
        let afterDelete =
          (await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get)
          ->Array.sliceToEnd(~start=12)
          ->Array.map(Highlighting__SemanticToken.toString)

        Assert.deepStrictEqual(
          afterDelete,
          [
            "(6:0-3) function [agda]",
            "(6:6-7) type [agda]",
            "(6:10-11) type [agda]",
            "(6:14-15) type [agda]",
          ],
        )

        // A subsequent edit must still shift the surviving tokens correctly.
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(5, 0), "\n")

        let afterInsert =
          (await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get)
          ->Array.sliceToEnd(~start=12)
          ->Array.map(Highlighting__SemanticToken.toString)

        Assert.deepStrictEqual(
          afterInsert,
          [
            "(7:0-3) function [agda]",
            "(7:6-7) type [agda]",
            "(7:10-11) type [agda]",
            "(7:14-15) type [agda]",
          ],
        )
      },
    )

    Async.it("should work after deleting an empty line", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)

      let _ = await Editor.Text.delete(
        ctx.state.document,
        VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(6, 0)),
      )

      let expected = [
        "(5:0-3) function [agda]",
        "(5:6-7) type [agda]",
        "(5:10-11) type [agda]",
        "(5:14-15) type [agda]",
        "(6:0-1) variable [agda]",
        "(6:2-3) function [agda]",
        "(6:4-5) variable [agda]",
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

      let expected = ["(5:0-1) variable [agda]", "(5:2-3) function [agda]", "(5:4-5) variable [agda]"]
      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      let actual =
        tokens->Array.sliceToEnd(~start=12)->Array.map(Highlighting__SemanticToken.toString)

      Assert.deepStrictEqual(actual, expected)
    })

    Async.it("should remove Tokens after Command.Quit", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)
      await ctx->AgdaMode.quit
      let tokens = await ctx.state.tokens->Tokens.getVSCodeTokens->Resource.get
      Assert.deepStrictEqual(tokens->Array.length, 0)
    })

    Async.it("should reset deltas to zero on Command.Load", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)

      // Immediately after load, deltas must be empty.
      Assert.deepStrictEqual(ctx.state.tokens->Tokens.deltasLength, 0)

      // An edit grows deltas.
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(6, 0), "\n")
      Assert.ok(ctx.state.tokens->Tokens.deltasLength > 0)

      // Loading again sends ClearHighlighting which calls Tokens.reset, wiping deltas.
      await ctx->AgdaMode.execute(Load)
      Assert.deepStrictEqual(ctx.state.tokens->Tokens.deltasLength, 0)
    })

    Async.it("should shift decoration ranges correctly after ordinary edits", async () => {
      let ctx = await AgdaMode.makeAndLoad(filename)

      // Introduce a termination problem so that Agda produces decorations on the next load.
      let _ = await Editor.Text.replace(
        ctx.state.document,
        VSCode.Range.make(VSCode.Position.make(7, 8), VSCode.Position.make(7, 15)),
        "x + y",
      )
      await ctx->AgdaMode.execute(Load)

      // Helper: extract decoration ranges as (line, startCol, endCol) tuples, sorted by startCol.
      let getRanges = () =>
        ctx.state.tokens
        ->Tokens.toDecorations
        ->Map.values
        ->Iterator.toArray
        ->Array.flat
        ->Array.toSorted((a, b) =>
          Int.compare(
            VSCode.Range.start(a)->VSCode.Position.character,
            VSCode.Range.start(b)->VSCode.Position.character,
          )
        )
        ->Array.map(r => (
          VSCode.Range.start(r)->VSCode.Position.line,
          VSCode.Range.start(r)->VSCode.Position.character,
          VSCode.Range.end_(r)->VSCode.Position.character,
        ))

      // After second load: capture the 2 decoration ranges.
      let initial = getRanges()
      Assert.deepStrictEqual(initial->Array.length, 2)

      // Insert "\n" at line 0 — every decorated line must shift by exactly 1.
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), "\n")
      let afterFirst = getRanges()
      Assert.deepStrictEqual(afterFirst->Array.length, 2)
      // Each range must have the same columns but its line incremented by 1.
      Assert.deepStrictEqual(
        afterFirst,
        initial->Array.map(((line, startCol, endCol)) => (line + 1, startCol, endCol)),
      )

      // Insert another "\n" at line 0 — shift by 1 more.
      let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), "\n")
      let afterSecond = getRanges()
      Assert.deepStrictEqual(afterSecond->Array.length, 2)
      Assert.deepStrictEqual(
        afterSecond,
        initial->Array.map(((line, startCol, endCol)) => (line + 2, startCol, endCol)),
      )
    })

    Async.it("should update tokens & decorations after each Command.Load", async () => {
      // helper function to filter tokens with TerminationProblem aspect
      let filterTerminationProblem = Array.filter(
        _,
        token =>
          token.Token.aspects->Array.some(
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
