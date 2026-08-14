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
// this test is built so that either one being wrong fails it.
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

  Async.it(
    "shifts the highlighting by an edit made after the request was sent",
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
      let paddingLength = String.length(padding)

      // `ClearHighlighting` is the first response of a load and precedes the
      // highlighting responses, so this puts the edit inside the window
      // between the request and its answer. The middleware awaits the edit
      // before it lets the response through, which keeps the ordering
      // deterministic instead of leaving it to a timer.
      let editApplied = ref(false)
      let responseOrder = []
      ctx.state.middlewares
      ->Array.push(handler => async response => {
        responseOrder->Array.push(Response.toString(response))
        switch response {
        | Response.ClearHighlighting =>
          await handler(response)
          if !editApplied.contents {
            let succeeded = await Editor.Text.insert(
              ctx.state.document,
              VSCode.Position.make(0, 0),
              padding,
            )
            if !succeeded {
              raise(Failure("could not insert the padding line"))
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

      // Without this the assertion could pass on a load that never edited
      // anything, which is the one way it could be green while broken.
      if !editApplied.contents {
        Assert.fail("ClearHighlighting was never observed, so no edit was made during the load")
      }

      // The edit only sits inside the window if `ClearHighlighting` really did
      // arrive before the highlighting. That ordering is relied on above, so
      // assert it rather than assume it: if it ever changed, everything below
      // would still pass while measuring nothing.
      let clearAt = responseOrder->Array.findIndex(name => name == "ClearHighlighting")
      let highlightingAt =
        responseOrder->Array.findIndex(name => name->String.startsWith("HighlightingInfo"))
      switch (clearAt, highlightingAt) {
      | (-1, _) => Assert.fail("no ClearHighlighting in " ++ responseOrder->Array.join(", "))
      | (_, -1) => Assert.fail("no HighlightingInfo in " ++ responseOrder->Array.join(", "))
      | (clear, highlighting) => Assert.deepStrictEqual(clear < highlighting, true)
      }

      // Offsets are in UTF-16 units, so both the table and the shift show up
      // here: the table is worth one unit, the shift the whole padding.
      Assert.deepStrictEqual(
        unsolvedMetaOffsets(ctx.state.tokens),
        offsetsBefore->Array.map(((start, end)) => (start + paddingLength, end + paddingLength)),
      )

      // The painted range moves down one line and keeps its columns.
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
})
