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
  // middleware handles the clear, applies the edit, and holds highlighting
  // responses until the edit finishes, keeping the ordering deterministic
  // instead of leaving it to a timer.
  //
  // Fails rather than measuring nothing if the edit never ran, or if the
  // ordering this relies on ever stops holding.
  let reloadWithEditInFlight = async (ctx: AgdaMode.t, edit) => {
    let editStarted = ref(false)
    let editApplied = ref(false)
    let responseOrder = []
    // NonLast responses are handled concurrently by Connection__Scheduler.
    // Awaiting the ClearHighlighting handler therefore does not hold back a
    // HighlightingInfo response that has already arrived. Gate highlighting
    // explicitly so it cannot insert the stale token until the edit event has
    // updated `editsSinceLoad`.
    let (editFinished, resolveEditFinished, _) = Util.Promise_.pending()
    ctx.state.middlewares
    ->Array.push(handler => async response => {
      responseOrder->Array.push(Response.toString(response))
      switch response {
      | Response.ClearHighlighting =>
        let shouldEdit = !editStarted.contents
        if shouldEdit {
          editStarted := true
        }
        await handler(response)
        if shouldEdit {
          let succeeded = await edit()
          if !succeeded {
            raise(Failure("the edit was rejected"))
          }
          editApplied := true
          resolveEditFinished()
        }
      | Response.HighlightingInfoDirect(_, _)
      | Response.HighlightingInfoIndirect(_)
      | Response.HighlightingInfoIndirectJSON(_) =>
        if editStarted.contents {
          await editFinished
        }
        await handler(response)
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

// #264: a load used to save and replace the shared offset snapshot before
// waiting for the connection. Finishing the first load then erased the second
// load's edit history. Its answer registered the opening `{` of an expanded hole
// as a one-character `?`, and scanAllGoals kept expanding that character forever.
module EditLimit = {
  type t
  let install: unit => t = %raw(`function() {
    const vscode = require("vscode");
    const original = vscode.workspace.applyEdit;
    let count = 0;
    vscode.workspace.applyEdit = function(...args) {
      if (++count > 40) {
        throw new Error("#264: more than 40 edits while completing two loads");
      }
      return original.apply(this, args);
    };
    return { restore() { vscode.workspace.applyEdit = original; } };
  }`)
  @send external restore: t => unit = "restore"
}

// RequestSent means the second load has reached the connection queue. Hold the
// first answer until then, so the bad ordering is deterministic. The test does
// not require the second load to actually run before the first one finishes.
let withOverlappingReload = async (
  ctx: AgdaMode.t,
  ~afterMakeCase=false,
  ~outsideResponse=false,
  operation,
) => {
  let sawMakeCase = ref(!afterMakeCase)
  let raced = ref(false)
  let extraLoad = ref(None)
  let (queued, resolveQueued, _) = Util.Promise_.pending()
  let (start, resolveStart, _) = Util.Promise_.pending()
  // A user command does not inherit a response's reentrant connection owner.
  // Create this async chain outside the middleware to exercise that path too.
  let outsideLoad = if outsideResponse {
    let run = async () => {
      await start
      await executeCommand("agda-mode.load")
    }
    Some(run())
  } else {
    None
  }
  let disposeLog = ctx.channels.log->Chan.on(log => {
    switch log {
    | RequestSent(Load) if raced.contents => resolveQueued()
    | _ => ()
    }
  })
  ctx.state.middlewares
  ->Array.push(handler => async response => {
    switch response {
    | Response.MakeCase(_, _) => sawMakeCase := true
    | _ => ()
    }
    switch response {
    | Response.InteractionPoints(_) if sawMakeCase.contents && !raced.contents =>
      raced := true
      extraLoad :=
        switch outsideLoad {
        | None => Some(executeCommand("agda-mode.load"))
        | Some(load) =>
          resolveStart()
          Some(load)
        }
      await queued
    | _ => ()
    }
    await handler(response)
  })
  ->ignore
  // Stop a broken build before it edits indefinitely. No edit is altered below
  // that limit, and the assertions check the whole document and goal ranges.
  let guard = EditLimit.install()
  let result = try {
    await operation()
    switch extraLoad.contents {
    | None => Assert.fail("the overlapping reload was never started")
    | Some(load) =>
      switch await load {
      | Some(Ok(_)) => ()
      | _ => Assert.fail("the overlapping reload failed")
      }
    }
    await Goals.waitUntilNotBusy(ctx.state.goals)
    Ok()
  } catch {
  | exn => Error(exn)
  }
  guard->EditLimit.restore
  disposeLog()
  ctx.state.middlewares->Array.pop->ignore
  switch result {
  | Ok() => ()
  | Error(exn) => raise(exn)
  }
}

let split = async (ctx: AgdaMode.t, payload) => {
  let goal = Goals.getGoalByIndex(ctx.state.goals, 0)->Option.getExn
  await AgdaMode.case(
    ctx,
    ~cursor=VSCode.TextDocument.positionAt(ctx.state.document, goal.start + 2),
    ~payload,
  )
}

// Submit the empty variable prompt, which introduces the function argument.
let introduceArgument = async (ctx: AgdaMode.t) => {
  let editor = await File.open_(ctx.filepath)
  let goal = Goals.getGoalByIndex(ctx.state.goals, 0)->Option.getExn
  Editor.Cursor.set(editor, VSCode.TextDocument.positionAt(ctx.state.document, goal.start + 2))
  let (loaded, resolveLoaded, _) = Util.Promise_.pending()
  let dispose = ctx.channels.commandHandled->Chan.on(command => {
    if command == Load {
      resolveLoaded()
    }
  })
  let command = executeCommand("agda-mode.case")
  let rec waitForPrompt = async () => {
    if ctx.state.panelCache.prompt->Option.isNone {
      await wait(5)
      await waitForPrompt()
    }
  }
  await waitForPrompt()
  switch ctx.state.panelCache.prompt {
  | Some((_, _, callback)) => await callback(PromptSuccess(""))
  | None => Assert.fail("the case-split prompt was never opened")
  }
  await loaded
  let _ = await command
  dispose()
}

describe("issue #264: overlapping loads", () => {
  This.timeout(20000)
  // Each fixture with its content before the tests edit it.
  let fixtures =
    ["Issue264Overlap.agda", "Issue264PlainReloads.agda"]->Array.map(filename => (
      filename,
      NodeJs.Fs.readFileSync(Path.asset(filename))->NodeJs.Buffer.toString,
    ))
  let original = filename =>
    fixtures->Array.find(((name, _)) => name == filename)->Option.getExn->snd
  let withLF = text => text->String.replaceAll("\r\n", "\n")
  Async.afterEach(async () => {
    await Registry.removeAndDestroyAll()
    let _ = await fixtures
    ->Array.map(((filename, content)) => File.write(Path.asset(filename), content))
    ->Promise.all
  })

  // Both tests that split Issue264Overlap.agda twice end with the same clause and goal.
  let assertSplitTwice = (ctx: AgdaMode.t) => {
    Assert.deepStrictEqual(
      Editor.Text.getAll(ctx.state.document)->withLF,
      original("Issue264Overlap.agda")
      ->withLF
      ->String.replace("test = ?", "test (fst₁ , fst₂ , snd₁) = {!   !}"),
    )
    Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [6:29-36)"])
  }

  Async.it(
    "keeps the hole intact when reloading during the second dependent-record split",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Issue264Overlap.agda")
      await introduceArgument(ctx)
      await split(ctx, "x")
      await withOverlappingReload(ctx, ~afterMakeCase=true, () => split(ctx, "x"))
      assertSplitTwice(ctx)
    },
  )

  Async.it(
    "keeps a newly typed goal intact across two ordinary reloads without a case split",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Issue264PlainReloads.agda")
      let document = ctx.state.document
      // A new, unregistered question mark is the only prerequisite; a record
      // and a MakeCase response are not needed to trigger the snapshot collision.
      let appended = "\ng : Nat\ng = ?\n"
      let position = VSCode.TextDocument.positionAt(
        document,
        Editor.Text.getAll(document)->String.length,
      )
      let applied = await Editor.Text.insert(document, position, appended)
      Assert.deepStrictEqual(applied, true)
      await withOverlappingReload(ctx, ~outsideResponse=true, () => AgdaMode.execute(ctx, Load))
      Assert.deepStrictEqual(
        Editor.Text.getAll(document)->withLF,
        (original("Issue264PlainReloads.agda")->withLF ++ appended)->String.replaceAll("?", "{!   !}"),
      )
      Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [4:5-12)", "#1 [7:5-12)"])
    },
  )
  Async.it(
    "lets the automatic reload finish when a user reload is queued behind Case",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Issue264Overlap.agda")
      await introduceArgument(ctx)
      await split(ctx, "x")
      // Queue the user load at Case's InteractionPoints, before MakeCase asks
      // for its own reentrant load. Taking the load lock before the connection
      // lock would deadlock: user load waits for Case, Case waits for user load.
      await withOverlappingReload(ctx, ~outsideResponse=true, () => split(ctx, "x"))
      assertSplitTwice(ctx)
    },
  )

  Async.it("lets a Load finish after the previous Load throws", async () => {
    let ctx = await AgdaMode.makeAndLoad("Issue264PlainReloads.agda")
    // Throw once from a Last response, which makes the request itself reject.
    // The load must still release its place in the load queue.
    let thrown = ref(false)
    ctx.state.middlewares
    ->Array.push(handler => async response => {
      switch response {
      | Response.CompleteHighlightingAndMakePromptReappear if !thrown.contents =>
        thrown := true
        raise(Failure("#264: a response handler throws during Load"))
      | _ => await handler(response)
      }
    })
    ->ignore
    let rejected = switch await executeCommand("agda-mode.load") {
    | _ => false
    | exception _ => true
    }
    ctx.state.middlewares->Array.pop->ignore
    Assert.deepStrictEqual((thrown.contents, rejected), (true, true))

    await AgdaMode.execute(ctx, Load)
    Assert.deepStrictEqual(
      Editor.Text.getAll(ctx.state.document)->withLF,
      original("Issue264PlainReloads.agda")->withLF->String.replaceAll("?", "{!   !}"),
    )
    Assert.deepStrictEqual(Goals.serializeGoals(ctx.state.goals), ["#0 [4:5-12)"])
  })
})
