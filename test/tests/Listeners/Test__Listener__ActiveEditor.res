open Mocha
open Test__Util

// Routing test for `ExtensionEvents.onOpenEditor` (Main.res) -- the listener
// that refreshes `state.editor`/`state.document` on tab switch-back and
// dispatches `Refresh`. This is what keeps `state.editor` trustworthy for
// every other routing listener (see #300).
//
// NOTE: assertions here read `ctx.state` (returned across the command
// boundary by `AgdaMode.makeAndLoad`, a live reference to the running
// extension's own `State.t`) rather than calling `Registry.get` from test
// code. Test files compile to `lib/js` and are `require`d directly by the
// Mocha runner, a separate module graph from the webpack-bundled
// `dist/app.bundle.js` the real extension runs as (see
// `Test__DoubleActivation.res`) -- so a `Registry` referenced from a test
// file is a different, permanently-empty dict, never the one the extension
// actually routes through.
describe("onOpenEditor routing", () => {
  // restore Goals.agda after each test: loading expands its "?" holes to
  // "{!   !}" in the buffer, and a later `Load` would save that to disk
  let fileContent = ref("")
  Async.beforeEach(async () => fileContent := (await File.read(Path.asset("Goals.agda"))))
  Async.afterEach(async () => await File.write(Path.asset("Goals.agda"), fileContent.contents))

  // `onOpenEditor` (Main.res) dispatches `Refresh` synchronously off
  // `onDidChangeActiveTextEditor`, so the correct way to observe it is a
  // barrier registered on `channels.log` before the switch that should
  // trigger it -- not a fixed sleep afterwards. A sleep is a race: on a
  // slow CI runner (this used to time out specifically on Windows, where
  // process/VS Code scheduling is slower) the listener can still be pending
  // when the sleep ends, so the test can fail without the code being wrong.
  let refreshDispatchedBarrier = channel =>
    Log.on(channel, log =>
      switch log {
      | Log.CommandDispatched(Refresh) => true
      | _ => false
      }
    )

  Async.it(
    "switching back to a loaded Agda file replaces state.editor/state.document and dispatches Refresh",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Goals.agda")
      // capture the editor, not to touch again -- this is exactly the
      // handle that goes stale, kept only to prove it gets replaced
      let oldEditor = ctx.state.editor

      let refreshDispatched = refreshDispatchedBarrier(ctx.channels.log)

      let _ = await staleAndRefreshEditor("Goals.agda", "InputMethod.agda")

      await refreshDispatched

      Assert.equal(ctx.state.editor !== oldEditor, true)
      Assert.equal(ctx.state.document->VSCode.TextDocument.fileName, ctx.state.id)
    },
  )

  Async.it("switching to a non-Agda editor does not dispatch Refresh", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let stopCollecting = Log.collect(ctx.channels.log)

    let _ = await File.open_(Path.asset("test-unicode-positions.js"))

    // There's no event to positively await for the absence of a dispatch, so
    // switch back to the Agda file to force one definite, observable Refresh
    // through the same listener. VS Code delivers active-editor-change
    // events to the extension host in order, so once this fires, the
    // non-Agda switch above has already been fully processed by
    // `onOpenEditor` (or not at all, per the isAgda filter) -- no sleep
    // needed to know the earlier switch is done being handled.
    let refreshDispatched = refreshDispatchedBarrier(ctx.channels.log)
    let _ = await File.open_(Path.asset("Goals.agda"))
    await refreshDispatched

    // exactly one Refresh (the flush above) proves the non-Agda switch
    // didn't dispatch one of its own
    let refreshCount =
      stopCollecting(~filter=log =>
        switch log {
        | Log.CommandDispatched(Refresh) => true
        | _ => false
        }
      )->Array.length
    Assert.equal(refreshCount, 1)
  })
})
