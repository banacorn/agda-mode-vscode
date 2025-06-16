open Mocha
open Test__Util

describe("EventFromView.JumpToTarget", () => {
  let filename = "ModuleContents.agda"
  let filepath = Path.asset(filename)

  Async.it("should set the cursor at the correct position", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    let link = Link.SrcLoc(
      Range(
        Some(filepath),
        [
          {
            start: {pos: 49, line: 5, col: 8},
            end_: {pos: 56, line: 5, col: 15},
          },
        ],
      ),
    )

    await ctx.state->State__Command.dispatchCommand(EventFromView(JumpToTarget(link)))
    let cursorPosition = Editor.Cursor.get(ctx.state.editor)
    Assert.deepStrictEqual(cursorPosition, VSCode.Position.make(4, 14))
  })

  Async.it("should set the cursor at the correct hole", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    let link = Link.Hole(0)

    await ctx.state->State__Command.dispatchCommand(EventFromView(JumpToTarget(link)))
    let cursorPosition = Editor.Cursor.get(ctx.state.editor)
    Assert.deepStrictEqual(cursorPosition, VSCode.Position.make(4, 10))
  })
})
