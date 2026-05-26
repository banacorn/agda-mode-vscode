open Mocha
open Test__Util

// The test harness suffers from a double activation problem:
//
// Activation A: VS Code loads the webpack bundle (dist/app.bundle.js) and calls
//               Main.activate, which registers command handlers wired to channels_A.
//
// Activation B: Test code calls activateExtension(), which calls Main.activateWithoutContext
//               from the lib/js build, creating a separate channels_B and trying to
//               register the same commands again.
//
// The two activations are independent Node modules with no shared state, so channels_A
// and channels_B are completely different EventEmitter objects. Commands dispatched
// through VS Code fire on whichever channels the registered handler captured — but
// test code is subscribed to the other set. Events never arrive.
//
// The fix: activateExtension() should call extension->VSCode.Extension.activate instead
// of Main.activateWithoutContext, so it receives the same channels the live extension
// already created, rather than spinning up a parallel second activation.

describe("Double activation", () => {
  Async.it(
    "channels from activateExtension should be the same as channels from the live extension",
    async () => {
      let candidate = await AgdaMode.commandExists("agda")
      let testChannels = await activateExtension(Some(candidate))

      let extension = switch VSCode.Extensions.getExtension("banacorn.agda-mode") {
      | None => raise(Failure("Extension banacorn.agda-mode not found"))
      | Some(ext) => ext
      }
      let exports = Obj.magic(await extension->VSCode.Extension.activate)

      Assert.deepStrictEqual(testChannels === exports["channels"], true)
    },
  )
})
