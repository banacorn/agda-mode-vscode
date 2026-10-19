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
})
