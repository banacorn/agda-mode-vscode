open Mocha
open Test__Util

describe("issue #316 cached .agdai with --allow-unsolved-metas", () => {
  This.timeout(20000)

  let fixtureDir = Path.asset("Issue316")
  let cacheHoleFile = "Issue316/CacheHole.agda"
  let buildUri = VSCode.Uri.file(NodeJs.Path.join([fixtureDir, "_build"]))

  let runCachedBuild = (cwd: string): unit =>
    %raw(`function(cwd) {
      require("node:child_process").execFileSync("agda", ["Main.agda"], {
        cwd,
        stdio: "pipe",
      });
      return;
    }`)(cwd)

  Async.beforeEach(async () => {
    let _ = await FS.deleteRecursive(buildUri)
  })

  Async.afterEach(async () => {
    let _ = await FS.deleteRecursive(buildUri)
    await Registry.removeAndDestroyAll()
  })

  Async.it(
    "should display a warning when Agda returns no goals but holes are present",
    async () => {
      runCachedBuild(fixtureDir)

      let ctx = await AgdaMode.makeAndLoad(cacheHoleFile)

      try {
        let display = ctx.state.panelCache.display
        Assert.deepStrictEqual(
          display->Option.map(((header, _body)) => header),
          Some(View.Header.Warning("Stale holes detected")),
        )
        await ctx->AgdaMode.quit
      } catch {
      | exn =>
        await ctx->AgdaMode.quit
        raise(exn)
      }
    },
  )
})
