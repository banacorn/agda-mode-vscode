open Mocha
open Test__EditorLayout__Util

module Harness = Test__EditorLayout__Harness

// Notation: `|` is left/right, `/` is top/bottom, and the number is the share
// of the parent's slot in percent. The Agda panel is named `Agda`.
// `workbench.editor.splitSizing` is `split` for these tests, so VS Code halves
// the active group when it adds a group.
describe("issue #129: Agda: Load must not discard the editor layout", () => {
  This.timeout(60000)

  describe("one group", () => {
    let layout = row([leaf(100., "Load")])

    Async.it(
      "places the panel below with bottom",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Load",
          ~position=Config.View.Bottom,
          ~expected="[Load(70) / Agda(30)]",
        ),
    )

    Async.it(
      "places the panel to the right with right",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Load",
          ~position=Config.View.Right,
          ~expected="[Load(50) | Agda(50)]",
        ),
    )
  })
})
