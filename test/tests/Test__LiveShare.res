open Mocha
open Test__Util

module TextEncoder = {
  type t
  @new external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Uint8Array.t = "encode"
}

// VS Code refuses a content provider for "vsls", one of its own schemes,
// but accepts a file system provider for it
@get external byteLength: Uint8Array.t => int = "byteLength"
external toFileSystemProvider: {..} => VSCode.FileSystemProvider.t = "%identity"

// serves every path under the "vsls" scheme as a file with this content
let registerGuestFileSystem = content => {
  let bytes = TextEncoder.make()->TextEncoder.encode(content)
  VSCode.Workspace.registerFileSystemProvider(
    "vsls",
    toFileSystemProvider({
      "onDidChangeFile": _ => VSCode.Disposable.make(() => ()),
      "watch": (_, _) => VSCode.Disposable.make(() => ()),
      "stat": _ => {"type": 1, "ctime": 0, "mtime": 0, "size": byteLength(bytes)},
      "readDirectory": _ => [],
      "createDirectory": _ => (),
      "readFile": _ => bytes,
      "writeFile": (_, _, _) => (),
      "delete": (_, _) => (),
      "rename": (_, _, _) => (),
    }),
    None,
  )
}

describe("Live Share", () => {
  describe("Main.refuseCommand", () => {
    Async.it("should refuse a vsls: document with the message", async () => {
      let fileSystem = registerGuestFileSystem("module A where\n")
      let document = await VSCode.Workspace.openTextDocument(
        VSCode.Uri.parse("vsls:/cohesion/A.agda"),
      )
      fileSystem->VSCode.Disposable.dispose->ignore
      Assert.deepStrictEqual(
        Main.refuseCommand(document),
        Some("Agda commands are not available to a Live Share guest. Ask the host to run them."),
      )
    })

    Async.it("should accept a file: document", async () => {
      let document = await VSCode.Workspace.openTextDocument(
        VSCode.Uri.file(Path.asset("Lib.agda")),
      )
      Assert.deepStrictEqual(Main.refuseCommand(document), None)
    })
  })

  describe("Commands on a vsls: document", () => {
    let fileSystem = ref(None)
    let document = ref(None)

    Async.before(async () => {
      fileSystem := Some(registerGuestFileSystem("module A where\n"))
      document :=
        Some(await VSCode.Workspace.openTextDocument(VSCode.Uri.parse("vsls:/cohesion/A.agda")))
    })

    // destroy any State a command created for the vsls: document
    Async.afterEach(async () => {
      let _ = await VSCode.Window.showTextDocument(document.contents->Option.getExn, ())
      let _ = await executeCommand("agda-mode.quit")
    })

    Async.after(async () => {
      let _ = await VSCode.Commands.executeCommand0("workbench.action.closeActiveEditor")
      fileSystem.contents->Option.forEach(VSCode.Disposable.dispose)
    })

    Command.names->Array.forEach(((_, name)) =>
      Async.it("should not run agda-mode." ++ name, async () => {
        let channels = await activateExtension(None)
        let _ = await VSCode.Window.showTextDocument(document.contents->Option.getExn, ())

        let handled = []
        let destructor = channels.commandHandled->Chan.on(command => handled->Array.push(command))
        let result = await executeCommand("agda-mode." ++ name)
        destructor()

        Assert.deepStrictEqual((result->Option.map(_ => ()), handled), (None, []))
      })
    )
  })

  describe("A vsls: document with the same path as a loaded file", () => {
    // a real load, see Test__Listener__ActiveEditor.res
    This.timeout(OS.onUnix ? 5000 : 15000)

    Async.it("should leave the state of the loaded file untouched", async () => {
      let ctx = await AgdaMode.makeAndLoad("Load.agda")
      let localDocument = ctx.state.document
      let goals = Goals.serializeGoals(ctx.state.goals)
      let removed = []
      let _ = ctx.state.onRemoveFromRegistry->Chan.on(() => removed->Array.push())
      let stopCollecting = Log.collect(ctx.channels.log)

      // open, change, select in and close the guest's copy of the loaded file
      let fileSystem = registerGuestFileSystem("module Load where\n\na : {!   !}\na = {!   !}\n")
      let guestEditor = await VSCode.Window.showTextDocumentWithUri(
        VSCode.Uri.file(ctx.filepath)->VSCode.Uri.with_({scheme: "vsls"}),
        None,
      )
      let guestDocument = guestEditor->VSCode.TextEditor.document
      let _ = await Editor.Text.insert(guestDocument, VSCode.Position.make(0, 0), "-- guest\n")
      Editor.Cursor.set(guestEditor, VSCode.Position.make(1, 0))
      let closed = Promise.make((resolve, _) => {
        let listener = ref(None)
        listener :=
          Some(
            VSCode.Workspace.onDidCloseTextDocument(closedDocument =>
              if closedDocument === guestDocument {
                listener.contents->Option.forEach(VSCode.Disposable.dispose)
                resolve()
              }
            ),
          )
      })
      let _ = await VSCode.Commands.executeCommand0("workbench.action.revertAndCloseActiveEditor")
      // VS Code keeps a document opened through the API for 3 minutes, or until
      // 60 newer ones push it out, before it raises onDidCloseTextDocument
      for i in 0 to 59 {
        let _ = await VSCode.Workspace.openTextDocument(
          VSCode.Uri.parse("vsls:/evict/" ++ Int.toString(i) ++ ".txt"),
        )
      }
      await closed
      fileSystem->VSCode.Disposable.dispose->ignore
      // closing the guest's editor makes the loaded file's editor active again,
      // which rebinds the state to a new editor of that file and refreshes it
      let logs = stopCollecting()

      // a following Load still works on the same state
      let _ = await File.open_(ctx.filepath)
      let reloaded = switch await executeCommand("agda-mode.load") {
      | Some(Ok(state)) => state === ctx.state
      | _ => false
      }

      let result = (
        ctx.state.document === localDocument,
        ctx.state.editor->VSCode.TextEditor.document === localDocument,
        Goals.serializeGoals(ctx.state.goals),
        removed,
        logs,
        reloaded,
      )
      await AgdaMode.quit(ctx)
      Assert.deepStrictEqual(
        result,
        (true, true, goals, [], [Log.CommandDispatched(Refresh)], true),
      )
    })
  })
})
