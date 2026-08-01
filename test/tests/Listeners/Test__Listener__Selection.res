open Mocha
open Test__Util

// Routing test for `ExtensionEvents.onSelectionChange` (Main.res). A
// single-state test can't prove routing -- `channels.inputMethod` is shared
// across every state, so "an event happened" doesn't say which state
// produced it. This drives two loaded states and proves the selection
// reaches the state whose document it belongs to, not whichever state
// happens to have an active input method.
describe("onSelectionChange routing", () => {
  Async.it(
    "a selection in a loaded file with no active input method emits nothing, even with another file's IM active",
    async () => {
      let (_editorA, channels) = await openNoLoad("InputMethod.agda")
      let (editorB, _channels) = await openNoLoad("Goals.agda")

      // focus back on A and activate its input method
      let _ = await File.open_(Path.asset("InputMethod.agda"))
      let imActivated = channels.inputMethod->Chan.once
      let _ = await VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
      let _ = await imActivated

      // focus B -- its input method was never activated
      let _ = await File.open_(Path.asset("Goals.agda"))

      let emitted = ref(false)
      let stopWatching = channels.inputMethod->Chan.on(_ => emitted := true)

      // a real selection change in B -- if it were misrouted to A's (active)
      // input method instead of B's (inactive) one, this would emit
      Editor.Selection.setMany(
        editorB,
        [VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(0, 1))],
      )

      await wait(100)
      stopWatching()

      Assert.equal(emitted.contents, false)

      // deactivate A's input method -- it's left active on a shared channel
      // that later, unrelated tests also assert on
      let _ = await File.open_(Path.asset("InputMethod.agda"))
      let _ = await VSCode.Commands.executeCommand0("agda-mode.escape")
    },
  )
})
