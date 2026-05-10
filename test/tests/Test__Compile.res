open Mocha
open Test__Util

describe("agda-mode.compile", () => {
  This.timeout(60000)
  let filename = "Compile.agda"
  let fileContent = ref("")
  let ctxRef: ref<option<AgdaMode.t>> = ref(None)
  let savedBackend = ref(Config.backendInTestingMode.contents)
  Async.beforeEach(async () => {
    fileContent := (await File.read(Path.asset(filename)))
    savedBackend := Config.backendInTestingMode.contents
  })
  Async.afterEach(async () => {
    Config.backendInTestingMode := savedBackend.contents
    await File.write(Path.asset(filename), fileContent.contents)
    switch ctxRef.contents {
    | Some(ctx) =>
      ctxRef := None
      await ctx->AgdaMode.quit
    | None => ()
    }
  })

  Async.it("should preserve GHC and GHCNoMain backend settings", async () => {
    Config.backendInTestingMode := "GHC"
    Assert.strictEqual(Config.getBackend(), "GHC")

    Config.backendInTestingMode := "GHCNoMain"
    Assert.strictEqual(Config.getBackend(), "GHCNoMain")
  })

  Async.it("should compile with GHC backend when backend is set to GHC", async () => {
    // GHC compilation on Windows CI takes too long (>60s) for Agda 2.7.0+                                                          
    if not(OS.onUnix) {                                                                                                             
      This.skip()                                                                                                                   
    }                                                                                

    switch await Connection__Command.search("ghc") {
    | Ok(_) => ()
    | Error(_) => This.skip()
    }

    Config.backendInTestingMode := "GHC"
    let ctx = await AgdaMode.makeAndLoad(filename)
    ctxRef := Some(ctx)

    let responses = await ctx.state->State__Connection.sendRequestAndCollectResponses(
      Request.Compile,
    )

    let compilationOkMsg = responses->Array.filterMap(response =>
      switch response {
      | Response.DisplayInfo(Response.DisplayInfo.CompilationOk(msg)) => Some(msg)
      | _ => None
      }
    )

    // should receive exactly one CompilationOk response
    Assert.strictEqual(Array.length(compilationOkMsg), 1)

    // the backend in the message should be "GHC", not "GHCNoMain"
    switch compilationOkMsg[0] {
    | None => Assert.fail("no CompilationOk response")
    | Some(msg) =>
      if not(msg->String.includes("GHC")) || msg->String.includes("GHCNoMain") {
        Assert.fail("Expected backend GHC but got: " ++ msg)
      }
    }
  })
})
