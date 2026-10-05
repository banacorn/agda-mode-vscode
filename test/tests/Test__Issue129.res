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

  describe("settings of the user", () => {
    let layout = row([leaf(50., "Load"), leaf(50., "Issue328")])

    // the sizes belong to VS Code, so only the shape is compared
    Async.it(
      "keeps VS Code's own sizing under the default splitSizing",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue328",
          ~position=Config.View.Right,
          ~splitSizing="auto",
          ~shapeOnly=true,
          ~expected="[Load | Issue328 | Agda]",
        ),
    )

    Async.it(
      "puts the panel to the right even when the side direction is down",
      async () =>
        await Harness.check(
          ~layout,
          ~active="Issue328",
          ~position=Config.View.Right,
          ~settings=[Harness.stringSetting("workbench.editor", "openSideBySideDirection", "down")],
          ~expected="[Load(50) | Issue328(25) | Agda(25)]",
        ),
    )
  })

  describe("the panel's lifecycle", () => {
    let layout = row([leaf(50., "Load"), leaf(50., "Issue328")])

    Async.it(
      "leaves the layout alone when a second Load reuses the panel",
      async () =>
        await Harness.withScenario(~position=Config.View.Right, async channels => {
          await Harness.build(~active="Issue328", layout)
          let first = await Harness.loadAndSettle(channels)
          let second = await Harness.loadAndSettle(~panelOpens=false, channels)
          Assert.deepStrictEqual(show(second.layout), show(first.layout))
        }),
    )

    Async.it(
      "places the panel again after it was closed",
      async () =>
        await Harness.withScenario(~position=Config.View.Right, async channels => {
          await Harness.build(~active="Issue328", layout)
          let _ = await Harness.loadAndSettle(channels)

          // the user closes the panel; the extension destroys its states
          let destroyed = Log.on(channels.log, Harness.isDestructionComplete)
          await Harness.closePanelTabs()
          await Harness.panelClosed(Harness.hangGuardMs)
          await Harness.withHangGuard(~what="the states to be destroyed", destroyed)

          // what VS Code made of the layout is the new baseline
          let baseline = await Harness.capture()
          await Harness.build(~active="Issue328", baseline.layout)

          // a new webview tab has to open for this to settle
          let after = await Harness.loadAndSettle(channels)
          Assert.deepStrictEqual(showShape(after.layout), "[Load | Issue328 | Agda]")
        }),
    )
  })

  describe("focus and membership", () => {
    Async.it(
      "keeps focus on the original editor and gives the panel a group of its own",
      async () =>
        await Harness.withScenario(~position=Config.View.Right, async channels => {
          let layout = row([leaf(50., "Load"), leaf(50., "Issue328")])
          await Harness.build(~active="Issue328", layout)
          let after = await Harness.loadAndSettle(channels)
          Assert.deepStrictEqual(
            (
              after.activeEditor,
              after.activeGroup,
              Harness.leaves(after.layout)->Array.filter(name => name->String.includes("Agda")),
            ),
            (Some("Issue328.agda"), "Issue328", ["Agda"]),
          )
        }),
    )
  })

  describe("an empty group", () => {
    Async.it(
      "is left alone when the panel goes below the active group",
      async () =>
        await Harness.check(
          ~layout=row([leaf(50., "Load"), leaf(50., "Empty")]),
          ~active="Load",
          ~position=Config.View.Bottom,
          ~settings=[Harness.boolSetting("workbench.editor", "closeEmptyGroups", false)],
          ~expected="[[Load(70) / Agda(30)](50) | Empty(50)]",
        ),
    )
  })
})
