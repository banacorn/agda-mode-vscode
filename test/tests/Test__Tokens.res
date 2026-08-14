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

    Async.it(
      "should resolve cross-file go-to-definition correctly when source file contains non-BMP Unicode",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("CrossFileUnicodeReference.agda")
        let filepath = ctx.state.document->VSCode.TextDocument.fileName->Parser.Filepath.make
        // CrossFileUnicodeSource.agda has "-- 𝐀𝐁" on line 1 before "data MyBool".
        // Each non-BMP char (𝐀, 𝐁) occupies 2 UTF-16 code units but is 1 Agda code point.
        // This shifts VSCode offsets by +2 relative to Agda 1-based offsets for MyBool.
        // Agda stores 1-based offset 48; correct conversion gives VSCode offset 49 → (2, 5).
        // A buggy impl using the referencing doc's converter gives offset 47 → (2, 3) instead.
        let sourceFilepathStr = Path.asset("CrossFileUnicodeSource.agda")

        switch Tokens.goToDefinition(ctx.state.tokens, ctx.state.document)(
          filepath,
          VSCode.Position.make(4, 8),
        ) {
        | None => raise(Failure("No definition found for MyBool in cross-file reference"))
        | Some(thunk) =>
          let actual = await thunk
          let expected = [
            (
              VSCode.Range.make(VSCode.Position.make(4, 7), VSCode.Position.make(4, 13)),
              sourceFilepathStr,
              // line 2 (0-indexed) of CrossFileUnicodeSource.agda: "data MyBool : Set where"
              // "MyBool" starts at col 5 (after "data ")
              VSCode.Position.make(2, 5),
            ),
          ]
          Assert.deepStrictEqual(actual, expected)
        }
      },
    )

    Async.it(
      "should resolve cross-file go-to-definition correctly when source file has CRLF line endings",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("CrossFileCRLFReference.agda")
        let filepath = ctx.state.document->VSCode.TextDocument.fileName->Parser.Filepath.make
        // CrossFileCRLFSource.agda has CRLF line endings. Lines 0 and 1 each end with \r\n,
        // which Agda counts as 1 code point but VSCode counts as 2 UTF-16 code units.
        // This shifts VSCode offsets by +2 relative to Agda 1-based offsets for MyCRLFBool.
        // Agda stores 1-based offset 40; correct conversion gives VSCode offset 41 → (2, 5).
        // A buggy impl using the referencing doc's converter gives offset 39 → (2, 3) instead.
        let sourceFilepathStr = Path.asset("CrossFileCRLFSource.agda")

        switch Tokens.goToDefinition(ctx.state.tokens, ctx.state.document)(
          filepath,
          VSCode.Position.make(4, 8),
        ) {
        | None => raise(Failure("No definition found for MyCRLFBool in cross-file reference"))
        | Some(thunk) =>
          let actual = await thunk
          let expected = [
            (
              VSCode.Range.make(VSCode.Position.make(4, 7), VSCode.Position.make(4, 17)),
              sourceFilepathStr,
              // line 2 (0-indexed) of CrossFileCRLFSource.agda: "data MyCRLFBool : Set where"
              // "MyCRLFBool" starts at col 5 (after "data ")
              VSCode.Position.make(2, 5),
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

  // Aspect merging must not accumulate. `insertWithVSCodeOffsets` merges the
  // aspect lists of two tokens at the same offset with `Array.concat`, which
  // never replaces and never deduplicates, so annotations pile up. Observed
  // in practice as tokens carrying `Function+Deadcode+Function` and
  // `Symbol+Deadcode+Symbol+Symbol` -- Agda cannot have sent `Symbol` three
  // times.
  //
  // WHY THIS MATTERS: Agda legitimately sends overlapping annotations, and
  // how many it sends varies with its own session state. The same file was
  // measured yielding 12 `Deadcode` annotations on a cold load and 132 on a
  // warm one, with the extension faithfully receiving both. That volume is
  // outside our control, so the merge must be insensitive to how many
  // annotations arrive, and to the order they arrive in.
  //
  // SEMANTICS BEING PINNED -- union, not replace. When two annotations hit
  // the same offset, the token keeps the *union* of their aspects, with no
  // duplicates. The alternative (last annotation replaces) is rejected
  // because `Tokens.reset` clears `agdaTokens` at the start of every load,
  // so annotations arriving within one load are additive by construction,
  // and a token genuinely can be both `Function` and `Deadcode`. Note the
  // order-independence property below only holds under union; if replace
  // were ever chosen instead, that property must be deleted, not "fixed".
  describe("aspect merging", () => {
    open FastCheck
    open FastCheck.Arbitrary
    open Property.Sync

    let mkToken = (~start, ~end, ~aspects): Token.t<Tokens.vscodeOffset> => {
      start,
      end,
      aspects,
      isTokenBased: false,
      note: None,
      source: None,
    }

    // a small pool of real aspects, so generated inserts overlap often
    let pool = [
      Highlighting__AgdaAspect.Function,
      Highlighting__AgdaAspect.Deadcode,
      Highlighting__AgdaAspect.Symbol,
      Highlighting__AgdaAspect.ConstructorInductive,
      Highlighting__AgdaAspect.UnsolvedMeta,
    ]
    // no `array` combinator in this binding; build it recursively the way
    // `TokenChange.arbitraryBatch` does
    // Each insert is (offset, endBump, aspectBits). `aspectBits` is decoded
    // into a multi-aspect list, because real Agda annotations carry several
    // aspects at once (`[Function, Deadcode]`), and the merge's
    // `old.aspects == token.aspects` shortcut behaves differently for lists
    // than for singletons. `endBump` varies `end` so the `old.end ==
    // token.end` branch is actually exercised.
    let arbInserts = {
      let rec aux = size =>
        if size == 0 {
          Combinators.constant([])
        } else {
          Combinators.tuple3(
            integerRange(0, 3),
            integerRange(1, 3),
            integerRange(1, 31),
          )->Derive.chain(triple => aux(size - 1)->Derive.map(rest => Array.concat([triple], rest)))
        }
      integerRange(1, 8)->Derive.chain(size => aux(size))
    }

    // decode a bitmask into a non-empty list of distinct aspects
    let aspectsOfBits = bits => {
      let picked = pool->Array.filterWithIndex((_, i) => land(bits, lsl(1, i)) != 0)
      Array.length(picked) == 0 ? [Highlighting__AgdaAspect.Function] : picked
    }

    // build the token store from a generated list of (offset, aspect) inserts
    let build = (inserts, ~reverse) => {
      let tokens = Tokens.make(None)
      let ordered = reverse ? inserts->Array.toReversed : inserts
      ordered->Array.forEach(((offset, endBump, bits)) =>
        tokens->Tokens.insertWithVSCodeOffsets(
          mkToken(~start=offset, ~end=offset + endBump, ~aspects=aspectsOfBits(bits)),
        )
      )
      tokens
    }

    // what the union semantics say the result must be, per offset
    let expectedUnion = inserts => {
      let acc = Map.make()
      inserts->Array.forEach(((offset, _endBump, bits)) => {
        let existing = switch acc->Map.get(offset) { | Some(xs) => xs | None => [] }
        let merged = aspectsOfBits(bits)->Array.reduce(existing, (seen, a) => {
          let name = Highlighting__AgdaAspect.toString(a)
          seen->Array.includes(name) ? seen : Array.concat(seen, [name])
        })
        acc->Map.set(offset, merged)
      })
      acc
      ->Map.entries
      ->Iterator.toArray
      ->Array.map(((k, v)) => (k, v->Array.toSorted(String.compare)))
      ->Array.toSorted(((a, _), (b, _)) => Int.compare(a, b))
    }

    let aspectSets = tokens =>
      tokens
      ->Tokens.toTokenArray
      ->Array.map(t => (
        t.Token.start,
        t.Token.aspects
        ->Array.map(Highlighting__AgdaAspect.toString)
        ->Array.toSorted(String.compare),
      ))
      ->Array.toSorted(((a, _), (b, _)) => Int.compare(a, b))

    let hasDuplicate = xs =>
      xs->Array.reduce((false, []), ((dup, seen), x) =>
        seen->Array.includes(x) ? (true, seen) : (dup, Array.concat(seen, [x]))
      ) |> fst

    it("no token ever accumulates a duplicate aspect", () =>
      assert_(
        property1(arbInserts, inserts => {
          let tokens = build(inserts, ~reverse=false)
          tokens
          ->Tokens.toTokenArray
          ->Array.every(t =>
            !hasDuplicate(t.Token.aspects->Array.map(Highlighting__AgdaAspect.toString))
          )
        }),
      )
    )

    it("re-inserting the same annotations changes nothing (idempotence)", () =>
      // A reload re-sends the same annotations. Applying a batch twice must
      // equal applying it once, or every reload accumulates.
      assert_(
        property1(arbInserts, inserts =>
          aspectSets(build(inserts, ~reverse=false)) ==
            aspectSets(build(Array.concat(inserts, inserts), ~reverse=false))
        ),
      )
    )

    it("a token's aspects are exactly the union of what was inserted", () =>
      // Stronger than "no duplicates": a `replace` implementation would pass
      // the duplicate check while silently dropping aspects. This pins the
      // union positively.
      assert_(
        property1(arbInserts, inserts =>
          aspectSets(build(inserts, ~reverse=false)) == expectedUnion(inserts)
        ),
      )
    )

    it("the result does not depend on the order annotations arrive in", () =>
      assert_(
        property1(arbInserts, inserts =>
          aspectSets(build(inserts, ~reverse=false)) ==
            aspectSets(build(inserts, ~reverse=true))
        ),
      )
    )
  })
})
