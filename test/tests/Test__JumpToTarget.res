open Mocha
open Test__Util

@module("node:fs/promises") external mkdtemp: string => promise<string> = "mkdtemp"
@module("node:fs/promises") external symlink: (string, string) => promise<unit> = "symlink"

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

  Async.it("should open another file, focus it and select the source location", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let otherFilepath = Path.asset("Lib.agda")

    // `Bool` in `data Bool : Set where`
    await jump(ctx, Link.SrcLoc(Range(Some(otherFilepath), [interval(3, 6, 10)])))

    Assert.deepStrictEqual(
      VSCode.Window.activeTextEditor->Option.map(editor => (
        editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
        Editor.Selection.getMany(editor),
      )),
      Some((
        otherFilepath,
        [VSCode.Range.make(VSCode.Position.make(2, 5), VSCode.Position.make(2, 9))],
      )),
    )
  })

  Async.it("should open another file without moving its cursor when the source location has no intervals", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let otherFilepath = Path.asset("Lib.agda")
    // put the other file's cursor somewhere known, then go back to the loaded file
    let otherEditor = await File.open_(otherFilepath)
    Editor.Cursor.set(otherEditor, VSCode.Position.make(3, 2))
    let _ = await File.open_(filepath)

    await jump(ctx, Link.SrcLoc(Range(Some(otherFilepath), [])))

    Assert.deepStrictEqual(
      VSCode.Window.activeTextEditor->Option.map(editor => (
        editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
        Editor.Selection.getMany(editor),
      )),
      Some((
        otherFilepath,
        [VSCode.Range.make(VSCode.Position.make(3, 2), VSCode.Position.make(3, 2))],
      )),
    )
  })

  Async.it("should reuse the editor of a file already open in another column", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let otherFilepath = Path.asset("Lib.agda")
    let otherDocument = await VSCode.Workspace.openTextDocumentWithFileName(otherFilepath)
    // column Two holds the Agda panel, so open the other file in column Three,
    // keeping the focus on the loaded file
    let showInColumnThree = (~preserveFocus) =>
      VSCode.Window.showTextDocumentWithShowOptions(
        otherDocument,
        Some(
          VSCode.TextDocumentShowOptions.make(~viewColumn=VSCode.ViewColumn.Three, ~preserveFocus, ()),
        ),
      )
    let _ = await showInColumnThree(~preserveFocus=true)
    let visibleEditors = () =>
      VSCode.Window.visibleTextEditors->Array.map(editor => (
        editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
        editor->VSCode.TextEditor.viewColumn,
      ))
    let visibleBefore = visibleEditors()

    let result = try {
      // `Bool` in `data Bool : Set where`
      await jump(ctx, Link.SrcLoc(Range(Some(otherFilepath), [interval(3, 6, 10)])))
      Ok((
        VSCode.Window.activeTextEditor->Option.map(editor => (
          editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName,
          editor->VSCode.TextEditor.viewColumn,
          Editor.Selection.getMany(editor),
        )),
        visibleEditors(),
      ))
    } catch {
    | exn => Error(exn)
    }
    // close the editor in column Three, so later tests start without that column
    let _ = await showInColumnThree(~preserveFocus=false)
    let _ = await VSCode.Commands.executeCommand0("workbench.action.closeActiveEditor")

    switch result {
    | Error(exn) => raise(exn)
    | Ok(after) =>
      Assert.deepStrictEqual(
        after,
        (
          Some((
            otherFilepath,
            Some(VSCode.ViewColumn.Three),
            [VSCode.Range.make(VSCode.Position.make(2, 5), VSCode.Position.make(2, 9))],
          )),
          visibleBefore,
        ),
      )
    }
  })

  Async.it("should select the source location in the current file when reached through a symlink", async () => {
    let ctx = await AgdaMode.makeAndLoad(filename)
    let tempDir = await mkdtemp(NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-mode-jump-"]))
    let symlinkPath = NodeJs.Path.join([tempDir, filename])
    await symlink(filepath, symlinkPath)
    let (_, visibleBefore, _) = snapshot(ctx)

    let result = try {
      await jump(ctx, Link.SrcLoc(Range(Some(symlinkPath), [interval(5, 1, 5)])))
      Ok(snapshot(ctx))
    } catch {
    | exn => Error(exn)
    }
    let _ = await FS.deleteRecursive(VSCode.Uri.file(tempDir))

    switch result {
    | Error(exn) => raise(exn)
    | Ok(after) =>
      Assert.deepStrictEqual(
        after,
        (
          Some(filepath),
          visibleBefore,
          [VSCode.Range.make(VSCode.Position.make(4, 0), VSCode.Position.make(4, 4))],
        ),
      )
    }
  })

  Async.it("should reveal a source location far down the file", async () => {
    let ctx = await AgdaMode.makeAndLoad("JumpToTargetFar.agda")
    // `target` on the last but one line, below the filler comments
    let target = VSCode.Range.make(VSCode.Position.make(301, 0), VSCode.Position.make(301, 6))
    let isVisible = () =>
      ctx.state.editor
      ->VSCode.TextEditor.visibleRanges
      ->Array.some(visible => visible->VSCode.Range.containsRange(target))
    // the editor reports its new visible ranges only after it has scrolled,
    // so wait for that (or give up after a while)
    let waitUntilVisible = () =>
      Promise.make((resolve, _) =>
        if isVisible() {
          resolve(true)
        } else {
          let disposable = ref(None)
          let finish = () => {
            disposable.contents->Option.forEach(VSCode.Disposable.dispose)
            resolve(isVisible())
          }
          let timeout = setTimeout(finish, 2000)
          disposable :=
            Some(
              VSCode.Window.onDidChangeTextEditorVisibleRanges(_ =>
                if isVisible() {
                  clearTimeout(timeout)
                  finish()
                }
              ),
            )
        }
      )

    Editor.Cursor.set(ctx.state.editor, VSCode.Position.make(0, 0))
    ctx.state.editor->VSCode.TextEditor.revealRange(
      VSCode.Range.make(VSCode.Position.make(0, 0), VSCode.Position.make(0, 0)),
      None,
    )
    Assert.deepStrictEqual(isVisible(), false)

    await jump(ctx, Link.SrcLoc(Range(Some(Path.asset("JumpToTargetFar.agda")), [interval(302, 1, 7)])))

    Assert.deepStrictEqual(await waitUntilVisible(), true)
  })
})
