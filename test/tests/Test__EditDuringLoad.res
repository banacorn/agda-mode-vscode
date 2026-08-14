open Mocha
open Test__Util

// An edit made while a load is still in flight is lost from the highlighting
// that load produces.
//
// The Load command saves the document, then sends the request. Agda computes
// its offsets against the text it read at save time. If the user types before
// the answer comes back, the document on screen is no longer that text.
// `insertTokens` converts Agda's offsets against the document as it is now,
// so every token lands short by the length of the edit.
//
// Nothing records the difference. `applyEdit` does update `self.deltas` on
// each keystroke, but `rebaseTokens` consumes and clears them in the same
// call (`Tokens.res`), so by the time the response arrives there is no record
// left that the document moved.
//
// The test inserts a whole line at the top of the file, so the correct
// outcome is unambiguous: every token in the file must shift by exactly the
// length of that line.
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

  Async.it(
    "shifts the highlighting by an edit made after the request was sent",
    async () => {
      let ctx = await AgdaMode.makeAndLoad(asset)
      let before = unsolvedMetaOffsets(ctx.state.tokens)
      Assert.deepStrictEqual(Array.length(before), 1)

      // A comment line is inert to Agda, so the only thing it changes is
      // where every following character sits.
      let padding = "-- padding\n"
      let paddingLength = String.length(padding)

      // `ClearHighlighting` is the first response of a load and always
      // precedes the highlighting responses, so this puts the edit inside the
      // window between the request and its answer. The middleware awaits the
      // edit before it lets the response through, which keeps the ordering
      // deterministic instead of leaving it to a timer.
      let editApplied = ref(false)
      ctx.state.middlewares
      ->Array.push(handler => async response =>
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
      )
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

      let expected = before->Array.map(((start, end)) => (
        start + paddingLength,
        end + paddingLength,
      ))
      Assert.deepStrictEqual(unsolvedMetaOffsets(ctx.state.tokens), expected)
    },
  )
})
