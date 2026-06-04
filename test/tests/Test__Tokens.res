open Mocha
open Test__Util

open Tokens

describe("Tokens", () => {
  This.timeout(10000)
  describe("Token generation", () => {
    Async.it(
      "should emit `onUpdate` event when highlighting is generated",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let (promise, resolve, _) = Util.Promise_.pending()

        let _disposable = ctx.state.tokens->Tokens.onUpdate->Chan.on(resolve)

        ctx.state.tokens->Tokens.generateHighlighting(ctx.state.editor)

        await promise
        Assert.ok(true)
      },
    )

    Async.it(
      "should produce 28 tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->toTokenArray
          ->Array.map(
            token => {
              let range = VSCode.Range.make(
                VSCode.TextDocument.positionAt(ctx.state.document, token.start),
                VSCode.TextDocument.positionAt(ctx.state.document, token.end),
              )
              Editor.Range.toString(range) ++ " " ++ Token.toString(token)
            },
          )
        Assert.deepStrictEqual(Array.length(tokens), 28)
      },
    )

    Async.it(
      "should produce correct tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->toTokenArray
          ->Array.map(
            token => {
              let range = VSCode.Range.make(
                VSCode.TextDocument.positionAt(ctx.state.document, token.start),
                VSCode.TextDocument.positionAt(ctx.state.document, token.end),
              )
              (Editor.Range.toString(range) ++ " " ++ Token.toStringWithoutOffsets(token))
              ->String.replaceRegExp(%re("/ \[src: \d+\]/g"), " [src]")
            },
          )

        Assert.deepStrictEqual(
          tokens,
          [
            "1:1-7 [Keyword]",
            "1:8-22 [Module] [src]",
            "1:23-28 [Keyword]",
            "2:1-5 [Keyword]",
            "2:6-7 [Datatype] [src]",
            "2:8-9 [Symbol]",
            "2:10-13 [Primitive] [src]",
            "2:14-19 [Keyword]",
            "3:3-4 [ConstructorInductive] [src]",
            "3:5-6 [Symbol]",
            "3:7-8 [Datatype] [src]",
            "4:3-4 [ConstructorInductive] [src]",
            "4:5-6 [Symbol]",
            "4:7-8 [Datatype] [src]",
            "4:9-10 [Symbol]",
            "4:11-12 [Datatype] [src]",
            "6:1-4 [Function, Operator] [src]",
            "6:5-6 [Symbol]",
            "6:7-8 [Datatype] [src]",
            "6:9-10 [Symbol]",
            "6:11-12 [Datatype] [src]",
            "6:13-14 [Symbol]",
            "6:15-16 [Datatype] [src]",
            "7:1-2 [Bound] [src]",
            "7:3-4 [Function, Operator] [src]",
            "7:5-6 [Bound] [src]",
            "7:7-8 [Symbol]",
            "7:9-16 [Hole]",
          ],
        )
      },
    )
  })

  describe("`goToDefinition`", () => {
    let fileContent = ref("")
    Async.beforeEach(async () => fileContent := await File.read(Path.asset("Lib.agda")))
    Async.afterEach(async () => await File.write(Path.asset("Lib.agda"), fileContent.contents))

    Async.it(
      "should return the position of the definition",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Lib.agda")
        let filepath = ctx.state.document->VSCode.TextDocument.fileName->Parser.Filepath.make
        let position = VSCode.Position.make(12, 27)

        switch Tokens.goToDefinition(ctx.state.tokens, ctx.state.document)(filepath, position) {
        | None => raise(Failure("No definition found for the given position"))
        | Some(thunk) =>
          let actual = await thunk
          let expected = [
            (
              VSCode.Range.make(VSCode.Position.make(12, 26), VSCode.Position.make(12, 27)),
              filepath->Parser.Filepath.toString,
              VSCode.Position.make(12, 22),
            ),
          ]
          Assert.deepStrictEqual(actual, expected)
        }
      },
    )

    Async.it(
      "should still find the definition after a deletion before the referenced token",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Lib.agda")
        let filepath = ctx.state.document->VSCode.TextDocument.fileName->Parser.Filepath.make

        // Capture the `f` token at (12, 26) and record its raw start offset.
        // We also compute the length of line 11 (the line we are about to delete)
        // so we can assert the exact new offset after rebasing.
        let fTokenBefore =
          Belt.Array.getBy(ctx.state.tokens->toTokenArray, t => {
            let pos = ctx.state.document->VSCode.TextDocument.positionAt(t.start)
            VSCode.Position.line(pos) == 12 && VSCode.Position.character(pos) == 26
          })->Option.getUnsafe
        let fStartBefore = fTokenBefore.start
        let deletedLength =
          ctx.state.document->VSCode.TextDocument.offsetAt(VSCode.Position.make(12, 0)) -
          ctx.state.document->VSCode.TextDocument.offsetAt(VSCode.Position.make(11, 0))

        // Delete zero-based line 11 ("if_then_else_ true  t _ = t\n"), the second
        // if_then_else_ clause. This shifts the third clause from line 12 to line 11,
        // so the `f` token moves from (12, 26)-(12, 27) to (11, 26)-(11, 27).
        let _ = await Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(VSCode.Position.make(11, 0), VSCode.Position.make(12, 0)),
        )

        // After rebasing, the f token at its new position (11, 26) must have its start
        // offset updated to exactly fStartBefore - deletedLength (not the old stale offset).
        // We find it by position search (index-based lookup is invalid after removals shift the array).
        let fToken =
          Belt.Array.getBy(ctx.state.tokens->toTokenArray, t => {
            let pos = ctx.state.document->VSCode.TextDocument.positionAt(t.start)
            VSCode.Position.line(pos) == 11 && VSCode.Position.character(pos) == 26
          })->Option.getUnsafe
        Assert.deepStrictEqual(fToken.start, fStartBefore - deletedLength)

        // goToDefinition at the token's new editor position must return the correct source range.
        switch Tokens.goToDefinition(ctx.state.tokens, ctx.state.document)(
          filepath,
          VSCode.Position.make(11, 27),
        ) {
        | None => raise(Failure("No definition found after deletion"))
        | Some(thunk) =>
          let actual = await thunk
          let expected = [
            (
              VSCode.Range.make(VSCode.Position.make(11, 26), VSCode.Position.make(11, 27)),
              filepath->Parser.Filepath.toString,
              // Source rebasing shifts the Agda source offset by −deletedLength, so the
              // definition resolves to its actual current position (11, 22).
              VSCode.Position.make(11, 22),
            ),
          ]
          Assert.deepStrictEqual(actual, expected)
        }
      },
    )

    Async.it(
      "should still find the definition after an insert before the referenced token",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("Lib.agda")
        let filepath = ctx.state.document->VSCode.TextDocument.fileName->Parser.Filepath.make

        // Insert "\n" at the very beginning of the file — shifts every line down by 1.
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), "\n")

        // The token that was at (12, 26)-(12, 27) is now visible at editor line 13.
        // goToDefinition is called at the new cursor position (13, 27).
        switch Tokens.goToDefinition(ctx.state.tokens, ctx.state.document)(
          filepath,
          VSCode.Position.make(13, 27),
        ) {
        | None => raise(Failure("No definition found after insert"))
        | Some(thunk) =>
          let actual = await thunk
          let expected = [
            (
              // srcRange must reflect the token's current editor position after the insert.
              VSCode.Range.make(VSCode.Position.make(13, 26), VSCode.Position.make(13, 27)),
              filepath->Parser.Filepath.toString,
              // Source rebasing updates the Agda source offset by the same delta as the insert,
              // so the definition resolves to its actual current position (13, 22).
              VSCode.Position.make(13, 22),
            ),
          ]
          Assert.deepStrictEqual(actual, expected)
        }
      },
    )
  })

  describe("Hole positions", () => {
    let fileContent = ref("")
    Async.beforeEach(async () =>
      fileContent := await File.read(Path.asset("GotoDefinition.agda"))
    )
    Async.afterEach(async () =>
      await File.write(Path.asset("GotoDefinition.agda"), fileContent.contents)
    )

    Async.it(
      "should track hole offset correctly after repeated inserts before it",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")

        // GotoDefinition.agda has exactly one hole: {!   !} on line 6.
        let positions0 =
          await ctx.state.tokens->Tokens.getHolePositionsFromLoad->Resource.get
        let entries0 = positions0->Map.entries->Iterator.toArray
        Assert.deepStrictEqual(entries0->Array.length, 1)
        let (holeStart0, holeEnd0) = entries0->Array.at(0)->Option.getUnsafe

        // Insert "\n" at line 5 (before _+_, which is before the hole).
        // The hole's offset must shift by exactly 1 (the length of "\n").
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(5, 0), "\n")
        let positions1 =
          await ctx.state.tokens->Tokens.getHolePositionsFromLoad->Resource.get
        let entries1 = positions1->Map.entries->Iterator.toArray
        Assert.deepStrictEqual(entries1->Array.length, 1)
        let (holeStart1, holeEnd1) = entries1->Array.at(0)->Option.getUnsafe
        Assert.deepStrictEqual(holeEnd1 - holeStart1, holeEnd0 - holeStart0)
        // Absolute check: hole is now on line 7, same column
        Assert.deepStrictEqual(
          ctx.state.document->VSCode.TextDocument.positionAt(holeStart1),
          VSCode.Position.make(7, 8),
        )

        // Insert "\n" at line 6 (now before x+y, still before the hole).
        // The hole's offset must shift by one more.
        let _ = await Editor.Text.insert(ctx.state.document, VSCode.Position.make(6, 0), "\n")
        let positions2 =
          await ctx.state.tokens->Tokens.getHolePositionsFromLoad->Resource.get
        let entries2 = positions2->Map.entries->Iterator.toArray
        Assert.deepStrictEqual(entries2->Array.length, 1)
        let (holeStart2, holeEnd2) = entries2->Array.at(0)->Option.getUnsafe
        Assert.deepStrictEqual(holeEnd2 - holeStart2, holeEnd0 - holeStart0)
        // Absolute check: hole is now on line 8, same column
        Assert.deepStrictEqual(
          ctx.state.document->VSCode.TextDocument.positionAt(holeStart2),
          VSCode.Position.make(8, 8),
        )
      },
    )
  })

  describe("Token tree structure after edits", () => {
    let fileContent = ref("")
    Async.beforeEach(async () =>
      fileContent := await File.read(Path.asset("GotoDefinition.agda"))
    )
    Async.afterEach(async () =>
      await File.write(Path.asset("GotoDefinition.agda"), fileContent.contents)
    )

    Async.it(
      "token array stays sorted and reflects current positions after repeated inserts",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let doc = ctx.state.document

        // Baseline: 28 tokens with strictly ascending start offsets.
        let baseline = ctx.state.tokens->toTokenArray
        Assert.deepStrictEqual(baseline->Array.length, 28)

        let baselineStarts = baseline->Array.map(t => t.start)
        let isStrictlyAscending = arr =>
          arr->Array.everyWithIndex((x, i) =>
            i == 0 || x > arr->Array.getUnsafe(i - 1)
          )
        Assert.ok(baselineStarts->isStrictlyAscending)

        // Find the index of the first token whose start maps to line 1 col 0: the "data" keyword.
        let dataIdx =
          Belt.Array.getIndexBy(baseline, t => {
            let pos = doc->VSCode.TextDocument.positionAt(t.start)
            VSCode.Position.line(pos) == 1 && VSCode.Position.character(pos) == 0
          })->Option.getUnsafe

        // Two inserts before line 1 shift all line-1+ tokens by two offsets.
        let _ = await Editor.Text.insert(doc, VSCode.Position.make(1, 0), "\n")
        let _ = await Editor.Text.insert(doc, VSCode.Position.make(2, 0), "\n")

        // After edits: still 28 tokens in strictly ascending order.
        let afterEdits = ctx.state.tokens->toTokenArray
        Assert.deepStrictEqual(afterEdits->Array.length, 28)
        let afterStarts = afterEdits->Array.map(t => t.start)
        Assert.ok(afterStarts->isStrictlyAscending)

        // The "data" token must now report its current editor position: line 3, col 0.
        // Before rebasing, token.start is the original offset so positionAt returns the wrong line.
        // After rebasing, token.start is the current offset so positionAt returns (3, 0).
        let dataTokenAfter = afterEdits->Array.getUnsafe(dataIdx)
        Assert.deepStrictEqual(
          doc->VSCode.TextDocument.positionAt(dataTokenAfter.start),
          VSCode.Position.make(3, 0),
        )
      },
    )
  })

  describe("Change", () => {
    open FastCheck
    open Property.Sync

    it(
      "`arbitraryBatch` should generate valid changes",
      () => {
        assert_(property1(TokenChange.arbitraryBatch(), xs => TokenChange.areValid(xs)))
      },
    )
  })

  describe("Intervals", () => {
    open FastCheck
    open Property.Sync
    it(
      "`empty` should should be valid",
      () => {
        Assert.deepStrictEqual(TokenIntervals.empty->TokenIntervals.hasError, None)
      },
    )

    it(
      "`applyChanges` should result in correct intervals with changes",
      () => {
        assert_(
          property1(
            TokenChange.arbitraryBatch(),
            changes => {
              let result = TokenIntervals.empty->TokenIntervals.applyChanges(changes)
              TokenIntervals.debugIsValid(result)
              result->TokenIntervals.hasError == None &&
                result->TokenIntervals.isValidWRTChanges(changes)
            },
          ),
        )
      },
    )

    it(
      "`applyChanges` twice should result in correct intervals with changes",
      () => {
        assert_(
          property2(
            TokenChange.arbitraryBatch(),
            TokenChange.arbitraryBatch(),
            (batch1, batch2) => {
              let batches = [batch1, batch2]

              let intervals =
                TokenIntervals.empty
                ->TokenIntervals.applyChanges(batch1)
                ->TokenIntervals.applyChanges(batch2)
              Assert.deepStrictEqual(intervals->TokenIntervals.hasError, None)
              intervals->TokenIntervals.isValidWRTChangeBatches(batches)
            },
          ),
        )
      },
    )
  })
})
