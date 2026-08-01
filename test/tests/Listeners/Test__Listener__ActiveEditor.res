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

  Async.it(
    "switching back to a loaded Agda file replaces state.editor/state.document and dispatches Refresh",
    async () => {
      let ctx = await AgdaMode.makeAndLoad("Goals.agda")
      // capture the editor, not to touch again -- this is exactly the
      // handle that goes stale, kept only to prove it gets replaced
      let oldEditor = ctx.state.editor

      let stopCollecting = Log.collect(ctx.channels.log)

      let _ = await staleAndRefreshEditor("Goals.agda", "InputMethod.agda")

      // onOpenEditor's callback does a Registry lookup and dispatch; give it
      // time to land before asserting
      await wait(100)

      let dispatchedRefresh =
        stopCollecting()->Array.some(log =>
          switch log {
          | Log.CommandDispatched(Refresh) => true
          | _ => false
          }
        )
      Assert.equal(dispatchedRefresh, true)

      Assert.equal(ctx.state.editor !== oldEditor, true)
      Assert.equal(ctx.state.document->VSCode.TextDocument.fileName, ctx.state.id)
    },
  )

  Async.it("switching to a non-Agda editor does not dispatch Refresh", async () => {
    let ctx = await AgdaMode.makeAndLoad("Goals.agda")
    let stopCollecting = Log.collect(ctx.channels.log)

    let _ = await File.open_(Path.asset("test-unicode-positions.js"))
    await wait(100)

    let dispatchedRefresh =
      stopCollecting()->Array.some(log =>
        switch log {
        | Log.CommandDispatched(Refresh) => true
        | _ => false
        }
      )
    Assert.equal(dispatchedRefresh, false)
  })
})
