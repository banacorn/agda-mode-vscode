open Mocha
open Test__Util

// Regression guard for #243 ("background highlighting does not reevaluate
// upon loading file with C-c C-l as it should").
//
// ROOT CAUSE, confirmed by comparing v0.5.7 (the version the issue reporter
// said last worked correctly) against current code -- not by guessing at
// races. In v0.5.7, `State__Response.res`'s `ClearHighlighting` handler ran:
//     state.tokens->Tokens.clear
//     state.highlighting->Highlighting.clear
//   where `Highlighting.clear` (src/Highlighting.res) was:
//     let clear = self => {
//       self.decorations->Array.forEach(((d,_)) => Editor.Decoration.destroy(d))
//       self.decorations = []
//     }
//   i.e. every current decoration was destroyed IMMEDIATELY and
//   UNCONDITIONALLY the instant Agda signalled "clear highlighting", before
//   any new highlighting was even requested.
//
// Commit 36e49e2c ("Replace the Highlighting module with the Tokens
// module", part of the v0.5.7..v0.6.0 range) merged `Highlighting` into
// `Tokens` and dropped that immediate clear. Today, `ClearHighlighting`
// (State__Response.res) only calls `Tokens.reset`, which resets bookkeeping
// (agdaTokens/deltas/vscodeTokens) but never touches `self.decorations` --
// see `Tokens.res`'s `reset`. The on-screen decorations now only get
// cleared as a *side effect* of `generateHighlighting`, which only runs
// later, inside the `CompleteHighlightingAndMakePromptReappear` handler.
//
// The first test below proves this gap directly and FAILS on current
// `master`: reload a file whose error is left completely unchanged (so
// there's no confounding from the separate, legitimate "editing inside a
// highlighted token's range removes it" mechanism) and check the decoration
// count right after `ClearHighlighting` is handled, before
// `CompleteHighlightingAndMakePromptReappear` has had any chance to run. In
// v0.5.7 this was always 0. On current code it's still whatever it was
// before the reload.
//
// This is exactly the missing safety net the heisenbug needed: as long as
// `CompleteHighlightingAndMakePromptReappear`/`generateHighlighting`
// eventually runs, decorations still end up correct (see the second test,
// which passes) -- but there is no longer anything that guarantees that. In
// v0.5.7, decorations couldn't be stuck stale no matter what happened
// afterwards, because they were already gone. Now, any disruption to the
// rest of that response cycle (slow typechecking, a dropped/reordered
// response, anything) leaves the *old* highlighting on screen with no
// recovery until the file is closed and reopened -- matching every
// symptom in the report.
//   AGDA_TEST_GLOB="Test__ReloadHighlightingReevaluation*.js" npm test
describe("reload highlighting reevaluation (#243)", () => {
  let fileContent = ref("")
  Async.beforeEach(async () =>
    fileContent := (await File.read(Path.asset("ReloadHighlighting.agda")))
  )
  Async.afterEach(async () =>
    await File.write(Path.asset("ReloadHighlighting.agda"), fileContent.contents)
  )

  let refEqual: ('a, 'a) => bool = %raw(`function(a, b) { return a === b }`)

  let decorationRangeCount = tokens =>
    tokens
    ->Tokens.toDecorations
    ->Map.values
    ->Iterator.toArray
    ->Array.map(Array.length)
    ->Array.reduce(0, (a, b) => a + b)

  Async.it(
    "FAILS on master: ClearHighlighting should clear decorations immediately, not leave that to generateHighlighting later",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("ReloadHighlighting.agda")
      Assert.equal(decorationRangeCount(ctx.state.tokens) > 0, true)

      // reload with the error left untouched -- isolates Tokens.reset's
      // behavior from the separate edit-based token-removal mechanism
      // (editing inside a highlighted token's range legitimately clears it
      // on its own, which would confound this check)
      let countRightAfterClear = ref(None)
      let liveTypesRightAfterClear = ref(None)
      let respDisposable = ctx.channels.responseHandled->Chan.on(response => {
        switch response {
        | Response.ClearHighlighting =>
          countRightAfterClear := Some(decorationRangeCount(ctx.state.tokens))
          liveTypesRightAfterClear := Some(Tokens.liveDecorationTypes(ctx.state.tokens))
        | _ => ()
        }
      })

      let (loadPromise, resolveLoad, _) = Util.Promise_.pending()
      let doneDisposable = ctx.channels.commandHandled->Chan.on(command => {
        if command == Command.Load {
          resolveLoad()
        }
      })
      let _ = await VSCode.Commands.executeCommand0("agda-mode.load")
      await loadPromise
      respDisposable()
      doneDisposable()

      switch (countRightAfterClear.contents, liveTypesRightAfterClear.contents) {
      | (a, b) if a == None || b == None =>
        Assert.fail(
          "ClearHighlighting was never observed (count=" ++
          (a == None ? "None" : "Some") ++
          " liveTypes=" ++ (b == None ? "None" : "Some") ++ ")",
        )
      | (Some(count), Some(liveTypes)) =>
        // v0.5.7 behavior: 0, immediately, unconditionally. Currently: still
        // whatever it was before this reload, since `Tokens.reset` never
        // touches `self.decorations`.
        // Asserted as a pair so a failure reports both numbers at once,
        // instead of the first one masking the second.
        //
        // ...and the decoration types must be DISPOSED, not merely dropped
        // from the map. Emptying `decorations` alone would satisfy the check
        // above while leaving every decoration painted in every editor, with
        // nothing left holding a reference to remove them -- strictly worse
        // than the current bug. VS Code cannot report what is on screen, so
        // this counter is the only mechanical way to tell the two apart.
        // `Editor.Decoration.destroy` (i.e. `dispose`) is the right tool: it
        // needs no `TextEditor`, which matters because `reset` has none, and
        // it removes the decoration from every editor showing the document.
        Assert.deepStrictEqual((count, liveTypes), (0, 0))
      }
    },
  )

  Async.it(
    "passes today: when generateHighlighting *does* run to completion, the end state is still correct",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("ReloadHighlighting.agda")
      Assert.equal(decorationRangeCount(ctx.state.tokens) > 0, true)

      let fixed = "module ReloadHighlighting where\n\ndata ℕ : Set where\n  zero : ℕ\n  suc : ℕ → ℕ\n\nbad : ℕ\nbad = zero\n"
      await File.write(Path.asset("ReloadHighlighting.agda"), fixed)

      let (loadPromise, resolveLoad, _) = Util.Promise_.pending()
      let disposable = ctx.channels.commandHandled->Chan.on(command => {
        if command == Command.Load {
          resolveLoad()
        }
      })
      let _ = await VSCode.Commands.executeCommand0("agda-mode.load")
      await loadPromise
      disposable()

      switch VSCode.Window.activeTextEditor {
      | None => Assert.fail("expected an active text editor after Load")
      | Some(activeEditor) => Assert.equal(refEqual(ctx.state.editor, activeEditor), true)
      }
      Assert.equal(decorationRangeCount(ctx.state.tokens), 0)
    },
  )
})
