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

  describe("side by side", () => {
    let layout = row([leaf(50., "Load"), leaf(50., "Issue328")])

    Async.it(
      "adds the panel beside the last group with right",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue328",
          ~position=Config.View.Right,
          ~expected="[Load(50) | Issue328(25) | Agda(25)]",
        ),
    )

    Async.it(
      "nests the panel below the last group with bottom",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue328",
          ~position=Config.View.Bottom,
          ~expected="[Load(50) | [Issue328(70) / Agda(30)](50)]",
        ),
    )

    // the groups after the active one move over when the panel appears
    Async.it(
      "adds the panel beside the first group with right",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Load",
          ~position=Config.View.Right,
          ~expected="[Load(25) | Agda(25) | Issue328(50)]",
        ),
    )

    Async.it(
      "nests the panel below the first group with bottom",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Load",
          ~position=Config.View.Bottom,
          ~expected="[[Load(70) / Agda(30)](50) | Issue328(50)]",
        ),
    )
  })

  describe("stacked", () => {
    let layout = col([leaf(50., "Load"), leaf(50., "Issue328")])

    Async.it(
      "adds the panel below the last group with bottom",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue328",
          ~position=Config.View.Bottom,
          ~expected="[Load(50) / Issue328(35) / Agda(15)]",
        ),
    )

    Async.it(
      "nests the panel beside the last group with right",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue328",
          ~position=Config.View.Right,
          ~expected="[Load(50) / [Issue328(50) | Agda(50)](50)]",
        ),
    )
  })

  describe("a stack beside a group", () => {
    let layout = row([
      leaf(50., "Load"),
      nested(50., col([leaf(50., "Issue328"), leaf(50., "Issue180")])),
    ])

    Async.it(
      "nests the panel beside the active group inside the stack with right",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue180",
          ~position=Config.View.Right,
          ~expected="[Load(50) | [Issue328(50) / [Issue180(50) | Agda(50)](50)](50)]",
        ),
    )

    Async.it(
      "adds the panel below the active group inside the stack with bottom",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue180",
          ~position=Config.View.Bottom,
          ~expected="[Load(50) | [Issue328(50) / Issue180(35) / Agda(15)](50)]",
        ),
    )
  })
})
