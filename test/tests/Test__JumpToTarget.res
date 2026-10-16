open Mocha
open Test__Util

describe("EventFromView.JumpToTarget", () => {
  let filename = "ModuleContents.agda"
  let filepath = Path.asset(filename)

  let interval = (line, startCol, endCol): Common.AgdaInterval.t => {
    start: {pos: 0, line, col: startCol},
    end_: {pos: 0, line, col: endCol},
  }

  // what a jump could change: the active editor's file, the files of the
  // visible editors, and the selections of the loaded editor
  let snapshot = (ctx: AgdaMode.t) => (
    VSCode.Window.activeTextEditor->Option.map(editor =>
      editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    ),
    VSCode.Window.visibleTextEditors->Array.map(editor =>
      editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName
    ),
    Editor.Selection.getMany(ctx.state.editor),
  )

  let jump = async (ctx: AgdaMode.t, link) =>
    await ctx.state->State__Command.dispatchCommand(EventFromView(JumpToTarget(link)))

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

  Async.it("should change nothing when the source location has no range", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(0, 0))
    let before = snapshot(ctx)

    await jump(ctx, Link.SrcLoc(NoRange))

    Assert.deepStrictEqual(snapshot(ctx), before)
  })

  Async.it("should change nothing when the source location has no file", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(0, 0))
    let before = snapshot(ctx)

    await jump(ctx, Link.SrcLoc(Range(None, [interval(5, 1, 5)])))

    Assert.deepStrictEqual(snapshot(ctx), before)
  })

  Async.it("should select every interval of the source location", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    await jump(ctx, Link.SrcLoc(Range(Some(filepath), [interval(5, 1, 5), interval(6, 1, 5)])))

    Assert.deepStrictEqual(
      Editor.Selection.getMany(ctx.state.editor),
      [
        VSCode.Range.make(VSCode.Position.make(4, 0), VSCode.Position.make(4, 4)),
        VSCode.Range.make(VSCode.Position.make(5, 0), VSCode.Position.make(5, 4)),
      ],
    )
  })

  Async.it("should keep the same editors when jumping within the current file", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let (activeBefore, visibleBefore, _) = snapshot(ctx)

    await jump(ctx, Link.SrcLoc(Range(Some(filepath), [interval(5, 1, 5)])))

    let (activeAfter, visibleAfter, _) = snapshot(ctx)
    Assert.deepStrictEqual((activeAfter, visibleAfter), (activeBefore, visibleBefore))
  })

  Async.it("should set the cursor at a hole other than the first", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)

    await jump(ctx, Link.Hole(1))

    Assert.deepStrictEqual(Editor.Cursor.get(ctx.state.editor), VSCode.Position.make(5, 10))
  })

  Async.it("should change nothing when the hole does not exist", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(0, 0))
    let before = snapshot(ctx)

    await jump(ctx, Link.Hole(5))

    Assert.deepStrictEqual(snapshot(ctx), before)
  })
})
