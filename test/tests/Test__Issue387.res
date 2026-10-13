open Mocha
open Test__Util

// #387: `Agda: Quit` must finish even while Agda is still busy typechecking.
//
// Quitting tears the connection down, and that teardown has to end whatever
// Agda is doing at the time. It must not wait on a request that is still in
// flight. The test loads a file Agda never finishes checking, quits in the
// middle of that, and expects the state to be destroyed.
//
//   AGDA_TEST_GLOB="Test__Issue387*.js" npm test
describe("issue #387: Quit while Agda is busy", () => {
  This.timeout(30000)

  Async.it("finishes destroying the state", async () => {
    let candidate = await AgdaMode.commandExists("agda")
    let channels = await activateExtension(Some(candidate))
    let _ = await File.open_(Path.asset("Issue387.agda"))

    // Agda is busy once the first `RunningInfo` has been handled.
    // Quitting any earlier would reach an idle Agda, which proves nothing.
    let (checking, resolveChecking, _) = Util.Promise_.pending()
    let unsubscribe = channels.responseHandled->Chan.on(response =>
      switch response {
      | Response.RunningInfo(_, _) => resolveChecking()
      | _ => ()
      }
    )
    // The load never finishes, so it is not awaited.
    let _ = executeCommand("agda-mode.load")
    await checking
    unsubscribe()

    let destroyed =
      Log.on(channels.log, log =>
        switch log {
        | Log.Others("State.destroy: Connection released, destruction complete") => true
        | _ => false
        }
      )->Promise.thenResolve(_ => "destroyed")
    let timeout = Util.Promise_.setTimeout(5000)->Promise.thenResolve(_ => "timed out")

    // Not awaited either: if quitting hangs, so would this.
    let _ = executeCommand("agda-mode.quit")

    Assert.deepStrictEqual(await Promise.race([destroyed, timeout]), "destroyed")
  })
})
