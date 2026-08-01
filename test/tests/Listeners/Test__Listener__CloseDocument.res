open Mocha
open Test__Util

// Routing test for `ExtensionEvents.onCloseDocument` (Main.res) -- closing an
// `.agda` document should destroy its state, all the way through: goal
// cleanup, connection release, and removal from the Registry.
//
// NOTE: this observes destruction via the "destruction complete" log on
// `channels.log` (a live reference to the running extension's own channel,
// returned across the command boundary by `openNoLoad`/`activateExtension`)
// rather than calling `Registry.get` from test code -- see the note atop
// `Test__Listener__ActiveEditor.res` for why a `Registry` referenced
// directly from a test file can never observe the real extension's state.
// That log message is only ever reached via `State.destroy`, itself only
// reachable from `Registry.removeAndDestroy`, itself only called from
// `onCloseDocument` -- so observing it is exactly the routing proof needed.
//
// Uses `openNoLoad` rather than a real `AgdaMode.makeAndLoad` -- a real load
// leaves Goals.agda's buffer dirty (holes expanded to placeholders), and
// closing a dirty editor blocks on VS Code's "save changes?" prompt instead
// of actually closing, which this test doesn't want to deal with.
//
// Closes via `workbench.action.closeAllEditors`, not `closeActiveEditor` --
// empirically, `closeActiveEditor` leaves the document sitting in
// `vscode.workspace.textDocuments` indefinitely in this test harness (its
// `onDidCloseTextDocument` never fires), while `closeAllEditors` disposes it
// reliably.
describe("onCloseDocument routing", () => {
  Async.it("closing a loaded Agda document destroys its state", async () => {
    let (_editor, channels) = await openNoLoad("InputMethod.agda")

    let destructionComplete = Log.on(
      channels.log,
      log =>
        switch log {
        | Log.Others("State.destroy: Connection released, destruction complete") => true
        | _ => false
        },
    )

    let _ = await VSCode.Commands.executeCommand0("workbench.action.closeAllEditors")

    await destructionComplete
  })
})
