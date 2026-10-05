open Mocha

// Which directory a native Agda process starts in (issue #152).
// The paths are never touched on disk; they only need to differ from the
// extension host's own cwd, so a stub that returns that cwd cannot pass.
describe("Connection.workingDirectory", () => {
  let root = NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-mode-working-directory"])

  it("uses the workspace folder when the document is inside one", () => {
    let workspace = NodeJs.Path.join([root, "workspace"])
    let actual = Connection.workingDirectory(
      ~workspaceFolderPath=Some(workspace),
      ~documentPath=NodeJs.Path.join([workspace, "src", "Main.agda"]),
    )
    Assert.deepStrictEqual(actual, workspace)
  })

  it("uses the document's own directory for a loose file", () => {
    let directory = NodeJs.Path.join([root, "loose"])
    let actual = Connection.workingDirectory(
      ~workspaceFolderPath=None,
      ~documentPath=NodeJs.Path.join([directory, "Loose.agda"]),
    )
    Assert.deepStrictEqual(actual, directory)
  })
})
