open Mocha
open Test__Util

// https://github.com/banacorn/agda-mode-vscode/issues/152
// Agda must start in the document's directory, so a relative path written by
// a `Reflection.External` command lands next to the document. Needs Agda 2.8
// (`Agda.Builtin.Reflection.External`) and `touch`, so it runs on Unix only.
describe("issue #152: Reflection.External writes relative to the document", () => {
  This.timeout(20000)

  let marker = "issue-152-created-by-reflection"
  let markerInAssets = Path.asset(marker)
  // where the marker lands if Agda starts in the extension host's directory
  let markerInHostCwd = NodeJs.Path.join([NodeJs.Process.cwd(NodeJs.Process.process), marker])
  let interfaceUri = VSCode.Uri.file(Path.asset("Issue152.agdai"))
  let env = NodeJs.Process.process->NodeJs.Process.env
  let originalAgdaDir = env->Dict.get("AGDA_DIR")

  let deleteMarkers = async () => {
    let _ = await FS.deleteRecursive(VSCode.Uri.file(markerInAssets))
    let _ = await FS.deleteRecursive(VSCode.Uri.file(markerInHostCwd))
  }

  Async.beforeEach(async () => {
    // Agda 2.8 only runs executables listed in `$AGDA_DIR/executables`
    env->Dict.set(
      "AGDA_DIR",
      VSCode.Uri.joinPath(Path.extensionUri, ["test/issue-152-agda"])->VSCode.Uri.fsPath,
    )
    // force Agda to evaluate the macro instead of accepting a cached interface
    let _ = await FS.deleteRecursive(interfaceUri)
    await deleteMarkers()
    await Registry.removeAndDestroyAll()
  })

  Async.afterEach(async () => {
    let _ = await FS.deleteRecursive(interfaceUri)
    await deleteMarkers()
    await Registry.removeAndDestroyAll()
    switch originalAgdaDir {
    | Some(value) => env->Dict.set("AGDA_DIR", value)
    | None => Dict.delete(env, "AGDA_DIR")
    }
  })

  Async.it("loads a module whose reflected command writes a relative file", async () => {
    if !OS.onUnix || !(await AgdaMode.versionGTE("agda", "2.8.0")) {
      This.skip()
    } else {
      let ctx = await AgdaMode.makeAndLoad("Issue152.agda")
      let header = ctx.state.panelCache.display->Option.map(((header, _body)) => header)
      let landedInAssets = NodeJs.Fs.existsSync(markerInAssets)
      let landedInHostCwd = NodeJs.Fs.existsSync(markerInHostCwd)
      await ctx->AgdaMode.quit

      Assert.deepStrictEqual(
        (header, landedInAssets, landedInHostCwd),
        (Some(View.Header.Success("*All Done*")), true, false),
      )
    }
  })
})
