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

  Async.it("should still expose holes after loading CacheHole.agda", async () => {
    runCachedBuild(fixtureDir)

    let ctx = await AgdaMode.makeAndLoad(cacheHoleFile)

    try {
      let goals = Goals.serializeGoals(ctx.state.goals)
      if goals->Array.length != 2 {
        Assert.fail(
          "expected 2 goals after load even when .agdai was cached from --allow-unsolved-metas, got " ++
          string_of_int(goals->Array.length),
        )
      }
      await ctx->AgdaMode.quit
    } catch {
    | exn =>
      await ctx->AgdaMode.quit
      raise(exn)
    }
  })
})
