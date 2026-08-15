open Mocha
open Test__Util

// An edit made while a load is still in flight is lost from the highlighting
// that load produces.
//
// The Load command saves the document, then sends the request. Agda computes
// its offsets against the text it read at save time. If the user types before
// the answer comes back, the document on screen is no longer that text.
//
// Two things have to be right for the token to land in the correct place, and
// the first test is built so that either one being wrong fails it.
//
//   The offset table. Agda counts code points, VSCode counts UTF-16 units.
//   The table that reconciles them has to be built from the text Agda read.
//   The asset carries an astral character close enough to the meta that a
//   table built from the edited text counts it on the wrong side, which costs
//   one unit.
//
//   The shift. `applyEdit` records each edit, but `rebaseTokens` consumes and
//   clears that record in the same call, so a separate running total has to
//   survive until the answer arrives. Losing it costs the whole padding.
//
// The remaining tests cover the other two branches of
// `TokenIntervals.translateOffset`: a negative delta, and an offset inside a
// removed range.
//
//   AGDA_TEST_GLOB="Test__EditDuringLoad*.js" npm test
describe("edit during an in-flight load", () => {
  This.timeout(60000)

  let asset = "EditDuringLoad.agda"
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset(asset))))
  Async.afterEach(async () => await File.write(Path.asset(asset), fileContent.contents))

  // The unsolved meta `m = _` is the only background-highlighted token in the
  // asset, which keeps the expected value a single pair of offsets.
  let unsolvedMetaOffsets = tokens =>
    tokens
    ->Tokens.toTokenArray
    ->Array.filter(token =>
      token.Token.aspects->Array.some(aspect => Tokens.Aspect.toString(aspect) == "UnsolvedMeta")
    )
    ->Array.map(token => (token.Token.start, token.Token.end))

  // Where go-to-definition would land. `insertTokens` puts same-file source
  // offsets through the same conversion and the same shift as the token's own
  // range, so they have to move with it. Cross-file sources are left as Agda
  // offsets and converted against the other file, so they are excluded here.
  let sameFileSourceOffsets = (tokens, document) => {
    let currentFilepath = document->VSCode.TextDocument.fileName->Parser.Filepath.make
    tokens
    ->Tokens.toTokenArray
    ->Array.filterMap(token =>
      switch token.Token.source {
      | Some((filepath, offset)) if filepath == currentFilepath => Some(offset)
      | _ => None
      }
    )
    ->Array.toSorted((a, b) => Int.toFloat(a - b))
  }

  // Token offsets are one layer below what the reporter of #243 actually saw.
  // These are the ranges handed to VSCode, so they also cover the step from a
  // token to a painted range.
  let decorationRanges = tokens =>
    tokens
    ->Tokens.toDecorations
    ->Map.values
    ->Iterator.toArray
    ->Array.flat
    ->Array.map(range => (
      VSCode.Range.start(range)->VSCode.Position.line,
      VSCode.Range.start(range)->VSCode.Position.character,
      VSCode.Range.end_(range)->VSCode.Position.line,
      VSCode.Range.end_(range)->VSCode.Position.character,
    ))
    ->Array.toSorted(((aLine, aChar, _, _), (bLine, bChar, _, _)) =>
      Int.toFloat(aLine == bLine ? aChar - bChar : aLine - bLine)
    )

  // Reload the asset and run `edit` inside the window between the request and
  // its answer.
  //
  // `ClearHighlighting` is the first response of a load and precedes the
  // highlighting responses, so it marks the start of that window. The
  // middleware awaits the edit before it lets the response through, which
  // keeps the ordering deterministic instead of leaving it to a timer.
  //
  // Fails rather than measuring nothing if the edit never ran, or if the
  // ordering this relies on ever stops holding.
  let reloadWithEditInFlight = async (ctx: AgdaMode.t, edit) => {
    let editApplied = ref(false)
    let responseOrder = []
    ctx.state.middlewares
    ->Array.push(handler => async response => {
      responseOrder->Array.push(Response.toString(response))
      switch response {
      | Response.ClearHighlighting =>
        await handler(response)
        if !editApplied.contents {
          let succeeded = await edit()
          if !succeeded {
            raise(Failure("the edit was rejected"))
          }
          editApplied := true
        }
      | _ => await handler(response)
      }
    })
    ->ignore

    let (loadPromise, resolveLoad, _) = Util.Promise_.pending()
    let disposable = ctx.channels.commandHandled->Chan.on(command =>
      if command == Command.Load {
        resolveLoad()
      }
    )
    let _ = await VSCode.Commands.executeCommand0("agda-mode.load")
    await loadPromise
    disposable()
    // The state is shared between the tests in this file, so leave the
    // middleware stack as it was found.
    ctx.state.middlewares->Array.pop->ignore

    if !editApplied.contents {
      Assert.fail("ClearHighlighting was never observed, so no edit was made during the load")
    }

    let clearAt = responseOrder->Array.findIndex(name => name == "ClearHighlighting")
    let highlightingAt =
      responseOrder->Array.findIndex(name => name->String.startsWith("HighlightingInfo"))
    switch (clearAt, highlightingAt) {
    | (-1, _) => Assert.fail("no ClearHighlighting in " ++ responseOrder->Array.join(", "))
    | (_, -1) => Assert.fail("no HighlightingInfo in " ++ responseOrder->Array.join(", "))
    | (clear, highlighting) => Assert.deepStrictEqual(clear < highlighting, true)
    }
  }

  Async.it(
    "shifts the highlighting forward by an insertion made after the request was sent",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let offsetsBefore = unsolvedMetaOffsets(ctx.state.tokens)
      let rangesBefore = decorationRanges(ctx.state.tokens)
      Assert.deepStrictEqual(Array.length(offsetsBefore), 1)
      Assert.deepStrictEqual(Array.length(rangesBefore), 1)

      // A comment line is inert to Agda, so the only thing it changes is
      // where every following character sits. One whole line, so the expected
      // decoration range is the same columns one line further down.
      let padding = "-- padding\n"
      let documentLengthBefore = Editor.Text.getAll(ctx.state.document)->String.length

      await reloadWithEditInFlight(ctx, () =>
        Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), padding)
      )
      // VSCode normalizes inserted newlines to the document's EOL convention.
      // Measure the resulting edit instead of assuming the LF in `padding`
      // occupies one UTF-16 unit (it is CRLF on Windows).
      let paddingLength = Editor.Text.getAll(ctx.state.document)->String.length - documentLengthBefore

      // Offsets are in UTF-16 units, so both the table and the shift show up
      // here: the table is worth one unit, the shift the whole padding.
      Assert.deepStrictEqual(
        unsolvedMetaOffsets(ctx.state.tokens),
        offsetsBefore->Array.map(((start, end)) => (start + paddingLength, end + paddingLength)),
      )

      Assert.deepStrictEqual(
        decorationRanges(ctx.state.tokens),
        rangesBefore->Array.map((((startLine, startChar, endLine, endChar)) => (
          startLine + 1,
          startChar,
          endLine + 1,
          endChar,
        ))),
      )
    },
  )

  Async.it(
    "shifts the highlighting back by a deletion made after the request was sent",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let offsetsBefore = unsolvedMetaOffsets(ctx.state.tokens)
      let rangesBefore = decorationRanges(ctx.state.tokens)
      Assert.deepStrictEqual(Array.length(offsetsBefore), 1)
      Assert.deepStrictEqual(Array.length(rangesBefore), 1)

      // Remove a whole comment line above the meta. Its length is read from
      // the document rather than written down here, so rewording the asset's
      // comments cannot silently change what this asserts.
      let lines = Editor.Text.getAll(ctx.state.document)->String.split("\n")
      let lineIndex = lines->Array.findIndex(line => line->String.startsWith("-- The astral"))
      if lineIndex < 0 {
        Assert.fail("the asset no longer has the comment line this test deletes")
      }
      let lineLength = switch lines->Array.get(lineIndex) {
      | Some(line) => String.length(line)
      | None => 0
      }
      // The line plus its newline.
      let deletedLength = lineLength + 1

      await reloadWithEditInFlight(ctx, () =>
        Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(
            VSCode.Position.make(lineIndex, 0),
            VSCode.Position.make(lineIndex + 1, 0),
          ),
        )
      )

      Assert.deepStrictEqual(
        unsolvedMetaOffsets(ctx.state.tokens),
        offsetsBefore->Array.map(((start, end)) => (start - deletedLength, end - deletedLength)),
      )

      Assert.deepStrictEqual(
        decorationRanges(ctx.state.tokens),
        rangesBefore->Array.map((((startLine, startChar, endLine, endChar)) => (
          startLine - 1,
          startChar,
          endLine - 1,
          endChar,
        ))),
      )
    },
  )

  Async.it(
    "collapses the highlighting when the edit removes the token itself",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let offsetsBefore = unsolvedMetaOffsets(ctx.state.tokens)
      Assert.deepStrictEqual(Array.length(offsetsBefore), 1)
      let (metaStart, metaEnd) = switch offsetsBefore->Array.get(0) {
      | Some(pair) => pair
      | None => (0, 0)
      }

      // Delete the `_` together with the space in front of it. Agda still
      // reports a meta there, because it read the file before this happened,
      // but the character it describes is gone.
      //
      // The removal has to start *before* the token, not at it. Both endpoints
      // then land at the front of the removed range: the start through the
      // branch that collapses an offset inside a removal, the end through the
      // ordinary negative delta. Deleting only the `_` would leave the token
      // start equal to the removal start, where collapsing and not collapsing
      // give the same answer and the branch goes untested.
      let removalStart = metaStart - 1
      await reloadWithEditInFlight(ctx, () =>
        Editor.Text.delete(
          ctx.state.document,
          VSCode.Range.make(
            ctx.state.document->VSCode.TextDocument.positionAt(removalStart),
            ctx.state.document->VSCode.TextDocument.positionAt(metaEnd),
          ),
        )
      )

      Assert.deepStrictEqual(unsolvedMetaOffsets(ctx.state.tokens), [(removalStart, removalStart)])
    },
  )

  Async.it(
    "adds up two edits made in the same window",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let offsetsBefore = unsolvedMetaOffsets(ctx.state.tokens)
      let rangesBefore = decorationRanges(ctx.state.tokens)
      Assert.deepStrictEqual(Array.length(offsetsBefore), 1)
      Assert.deepStrictEqual(Array.length(rangesBefore), 1)

      // Two separate edits, at two different places, so the running total has
      // to accumulate rather than record only the most recent one. Placing
      // them apart also makes the walk pass through more than one interval.
      let firstPadding = "-- first\n"
      let secondPadding = "-- second\n"
      let documentLengthBefore = Editor.Text.getAll(ctx.state.document)->String.length

      await reloadWithEditInFlight(ctx, async () => {
        let first = await Editor.Text.insert(
          ctx.state.document,
          VSCode.Position.make(0, 0),
          firstPadding,
        )
        // Read the line index after the first edit, since that one moved it.
        let lines = Editor.Text.getAll(ctx.state.document)->String.split("\n")
        let signatureLine = lines->Array.findIndex(line => line->String.startsWith("m : "))
        if signatureLine < 0 {
          Assert.fail("the asset no longer has the signature line this test inserts above")
        }
        let second = await Editor.Text.insert(
          ctx.state.document,
          VSCode.Position.make(signatureLine, 0),
          secondPadding,
        )
        first && second
      })
      let totalPadding = Editor.Text.getAll(ctx.state.document)->String.length - documentLengthBefore

      Assert.deepStrictEqual(
        unsolvedMetaOffsets(ctx.state.tokens),
        offsetsBefore->Array.map(((start, end)) => (start + totalPadding, end + totalPadding)),
      )

      // One whole line each, both above the meta.
      Assert.deepStrictEqual(
        decorationRanges(ctx.state.tokens),
        rangesBefore->Array.map((((startLine, startChar, endLine, endChar)) => (
          startLine + 2,
          startChar,
          endLine + 2,
          endChar,
        ))),
      )
    },
  )

  Async.it(
    "shifts the go-to-definition offsets by the same edit",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let sourcesBefore = sameFileSourceOffsets(ctx.state.tokens, ctx.state.document)
      // `m : ℕ` and the constructors refer to `ℕ` in this same file, so there
      // is something to shift. Assert it, or a change to the asset could empty
      // this list and leave the test passing on nothing.
      Assert.deepStrictEqual(Array.length(sourcesBefore) > 0, true)

      let padding = "-- padding\n"
      let documentLengthBefore = Editor.Text.getAll(ctx.state.document)->String.length

      await reloadWithEditInFlight(ctx, () =>
        Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), padding)
      )
      let paddingLength = Editor.Text.getAll(ctx.state.document)->String.length - documentLengthBefore

      Assert.deepStrictEqual(
        sameFileSourceOffsets(ctx.state.tokens, ctx.state.document),
        sourcesBefore->Array.map(offset => offset + paddingLength),
      )
    },
  )

  Async.it(
    "stops shifting once the load is over",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)

      let padding = "-- padding\n"
      await reloadWithEditInFlight(ctx, () =>
        Editor.Text.insert(ctx.state.document, VSCode.Position.make(0, 0), padding)
      )

      // The load is finished, so the window is closed and the correction that
      // belonged to it must not apply to anything else. What breaks otherwise
      // is a response that arrives between two loads: `beginLoad` resets both
      // pieces of state, so a missing `endLoad` is invisible to a reload and
      // only shows up here.
      //
      // Rather than drive a second Agda command, this calls the same function
      // a response would. Agda offsets 0 to 1 fall inside the padding, which
      // Agda never saw and no token covers, so the result is unambiguous.
      // A window left open would convert with the stale table and add the
      // padding, landing the token at 11 instead.
      let probe: Token.t<Tokens.agdaOffset> = {
        start: 0,
        end: 1,
        aspects: [Tokens.Aspect.UnsolvedConstraint],
        isTokenBased: false,
        note: None,
        source: None,
      }
      Tokens.insertTokens(ctx.state.tokens, ctx.state.editor, [probe])

      let probeOffsets =
        ctx.state.tokens
        ->Tokens.toTokenArray
        ->Array.filter(token =>
          token.Token.aspects->Array.some(aspect =>
            Tokens.Aspect.toString(aspect) == "UnsolvedConstraint"
          )
        )
        ->Array.map(token => (token.Token.start, token.Token.end))
      Assert.deepStrictEqual(probeOffsets, [(0, 1)])
    },
  )

  Async.it(
    "keeps an edit made between the save and the snapshot",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)

      // `save` is a no-op on a clean document and fires no event, so dirty the
      // buffer first. This edit is made outside any load and is rebased by the
      // ordinary path, so the offsets read below already account for it.
      let dirtying = await Editor.Text.insert(
        ctx.state.document,
        VSCode.Position.make(0, 0),
        "-- dirty\n",
      )
      if !dirtying {
        raise(Failure("could not dirty the document"))
      }

      let offsetsBefore = unsolvedMetaOffsets(ctx.state.tokens)
      Assert.deepStrictEqual(Array.length(offsetsBefore), 1)

      // Typed through VSCode's own `type` command, which inserts at the
      // cursor. `Editor.Text.insert` goes through `workspace.applyEdit`, which
      // is slower than a keypress and lands after the snapshot, so the window
      // closes before the edit arrives and the defect goes unseen.
      //
      // No newline, so nothing triggers auto indent. The cursor sits at the
      // start of the file, so this extends the comment line already there.
      let padding = "-- pad"
      Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(0, 0))
      let documentLengthBefore = Editor.Text.getAll(ctx.state.document)->String.length

      // The other tests in this file edit on `ClearHighlighting`, which is
      // well after the request goes out. This one reaches an earlier window.
      //
      // Load saves the document, awaits `showTextDocumentWithShowOptions`,
      // and only then records the baseline. `onDidSaveTextDocument` fires
      // inside that await. Agda reads what `save` wrote, so an edit here has
      // to be corrected for exactly like any other, and the baseline has to
      // be the saved text rather than the text this edit produces.
      //
      // The handler cannot await, so the edit is started and not waited on.
      // That only decides whether the edit lands inside the window. If it
      // lands late it is corrected by the ordinary path and the test passes,
      // so this can miss the defect but cannot report one that is not there.
      let editStarted = ref(false)
      let typing = ref(None)
      let saveDisposable = VSCode.Workspace.onDidSaveTextDocument(_ =>
        if !editStarted.contents {
          editStarted := true
          typing := Some(VSCode.Commands.executeCommand1("type", {"text": padding}))
        }
      )

      let (loadPromise, resolveLoad, _) = Util.Promise_.pending()
      let disposable = ctx.channels.commandHandled->Chan.on(command =>
        if command == Command.Load {
          resolveLoad()
        }
      )
      let _ = await VSCode.Commands.executeCommand0("agda-mode.load")
      await loadPromise
      disposable()
      let _ = saveDisposable->VSCode.Disposable.dispose

      if !editStarted.contents {
        Assert.fail("the save event never fired, so no edit was made")
      }

      // The handler could not await, so the typing may still be in flight.
      // Settle it here, or it lands during whichever test runs next.
      switch typing.contents {
      | Some(promise) => await promise
      | None => ()
      }
      let paddingLength = Editor.Text.getAll(ctx.state.document)->String.length - documentLengthBefore

      Assert.deepStrictEqual(
        unsolvedMetaOffsets(ctx.state.tokens),
        offsetsBefore->Array.map(((start, end)) => (start + paddingLength, end + paddingLength)),
      )
    },
  )
})
