open Mocha
open Test__Util

// Issue #78: agdaModeTyping binds the keys of the input method, such as `(`, `{` and the arrows.
// It must follow the input method of the active editor, or these keys are swallowed in another one.
//
// The commands are run directly, so the `when` clauses of the keybindings are not covered here.

describe("Issue #78: agdaModeTyping follows the active editor", () => {
  // A has the input method on, B is never loaded, C has a state with the input method off
  let fileA = Path.asset("Issue78A.agda")
  let fileB = Path.asset("Issue78B.agda")
  let fileC = Path.asset("Issue78C.agda")
  let plain = Path.asset("Issue78.txt")

  // runs `action`, waits for the first Context log after it, and returns the last value logged by then
  let contextAfter = async (channels: State.channels, action) => {
    let values = []
    let (logged, resolve, _) = Util.Promise_.pending()
    let destructor = channels.log->Chan.on(log =>
      switch log {
      | Log.Context(value) =>
        values->Array.push(value)
        resolve()
      | _ => ()
      }
    )
    let _ = await action()
    await logged
    await Util.Promise_.setTimeout(0)
    destructor()
    values->Array.at(-1)
  }

  // opens A and turns its input method on
  let activateInA = async () => {
    let channels = await activateExtension(None)
    let value = await contextAfter(channels, async () => {
      let _ = await File.open_(fileA)
      await VSCode.Commands.executeCommand0("agda-mode.input-symbol[Activate]")
    })
    Assert.deepStrictEqual(value, Some(true))
    channels
  }

  // turn the input method of A off and close every editor
  Async.afterEach(async () => {
    let _ = await File.open_(fileA)
    let _ = await VSCode.Commands.executeCommand0("agda-mode.escape")
    let _ = await VSCode.Commands.executeCommand0("workbench.action.closeAllEditors")
  })

  Async.it("should be false after switching to an unloaded Agda file", async () => {
    let channels = await activateInA()
    let value = await contextAfter(channels, () => File.open_(fileB))
    Assert.deepStrictEqual(value, Some(false))
  })

  Async.it("should be false after switching to a loaded Agda file with the input method off", async () => {
    let _ = await openNoLoad("Issue78C.agda")
    let channels = await activateInA()
    let value = await contextAfter(channels, () => File.open_(fileC))
    Assert.deepStrictEqual(value, Some(false))
  })

  Async.it("should be false after switching to a non-Agda file", async () => {
    let channels = await activateInA()
    let value = await contextAfter(channels, () => File.open_(plain))
    Assert.deepStrictEqual(value, Some(false))
  })

  Async.it("should be true after switching back to the Agda file with the input method on", async () => {
    let channels = await activateInA()
    let away = await contextAfter(channels, () => File.open_(fileB))
    let back = await contextAfter(channels, () => File.open_(fileA))
    Assert.deepStrictEqual((away, back), (Some(false), Some(true)))
  })

  Async.it("should be false after the last editor is closed", async () => {
    let channels = await activateInA()
    let value = await contextAfter(channels, () =>
      VSCode.Commands.executeCommand0("workbench.action.closeAllEditors")
    )
    Assert.deepStrictEqual(value, Some(false))
  })

  // C keeps a state, so Quit and Restart of A do not destroy and recreate the panel,
  // whose editor changes would log Context on their own

  Async.it("should be false after Quit", async () => {
    let _ = await openNoLoad("Issue78C.agda")
    let channels = await activateInA()
    let value = await contextAfter(channels, () => executeCommand("agda-mode.quit"))
    Assert.deepStrictEqual(value, Some(false))
  })

  describe("Restart", () => {
    // a real load, and another one by Restart
    This.timeout(OS.onUnix ? 10000 : 30000)

    Async.it("should be false after Restart", async () => {
      let ctx = await AgdaMode.makeAndLoad("Issue78A.agda")
      let _ = await openNoLoad("Issue78C.agda")
      let channels = await activateInA()
      let value = await contextAfter(channels, () => executeCommand("agda-mode.restart"))
      await AgdaMode.quit(ctx)
      Assert.deepStrictEqual(value, Some(false))
    })
  })
})
