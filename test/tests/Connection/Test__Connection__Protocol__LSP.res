open Mocha

// The race this test pins down is like a courier delivering one last
// package as a warehouse closes for the night:
//   1. The courier checks that the warehouse is open, then pauses before
//      reaching the loading bay.
//   2. While paused, the crew closes the warehouse and hauls the loading
//      bay away.
//   3. The courier resumes and tries to use a bay that no longer exists.
//   4. The delivery fails right there, with an error that just says
//      "starting the delivery failed".
// The test deliberately lets the crew finish closing up before the courier
// resumes. Even in that order, the courier should handle finding the
// warehouse closed without raising an alarm. Today it raises the analogous
// "starting the delivery failed" alarm; the real error is
// "Starting server failed", the same one from the logs.
//
// In real terms: the warehouse is the shared `LanguageClient`, open for
// business means its `$state` is `Running`, the loading bay is
// `activeConnection()`, and the crew closing up is `stop()`. The courier is
// vscode-languageclient's own automatic `textDocument/didClose` send. Its
// real `sendNotification` checks the warehouse is open, then pauses
// (`await`s `$start()`) before actually reaching the bay:
//
//   async sendNotification(type, params) {
//     if (state === StartFailed || state === Stopping || state === Stopped) {
//       return Promise.reject(...);
//     }
//     const connection = await this.$start();
//     return connection.sendNotification(type, params);
//   }
//
//   async $start() {
//     await this.start();
//     const connection = this.activeConnection();
//     if (connection === undefined) {
//       throw new Error(`Starting server failed`);
//     }
//     return connection;
//   }
//
// If agda-mode-vscode's `Registry__Connection.release` closes up the
// warehouse (via `Connection__Protocol__LSP.destroy` calling
// `LanguageClient.stop()`) while the courier is paused, `$start()` throws
// `Starting server failed`, the exact stack trace from the real VS Code
// extension-host logs.
//
// The closing-up side of this test drives the real production stack:
// Registry__Connection, Connection__Core, Connection__Endpoint__ALS,
// Connection__Protocol__LSP, and the real `LanguageClient.stop` binding.
// The courier's side calls through the real
// `Binding.LanguageClient.sendNotification` external, the same one
// `Connection__Protocol__LSP.sendNotification` calls, but that external is
// just a method dispatch; the actual checking-then-stepping-away logic
// belongs to the fake warehouse built below, standing in for
// vscode-languageclient's own code, which this codebase does not own or
// control. Calling through the real external matters because it exercises
// the same notification seam used in production. How long the courier
// stays paused is an explicit, test-controlled gate rather than a timer,
// so the race plays out the same way every run instead of only sometimes.

module LSP = Connection__Protocol__LSP
module Binding = Connection__Protocol__LSP__Binding

// Builds the fake warehouse: a loading bay (`activeConnection`), whether
// it's open for business (`state`), and a crew that can close it up
// (`stop`). Exposes the methods `Connection__Protocol__LSP__Binding.LanguageClient`
// actually calls. Its `sendNotification` is the courier: it checks the
// warehouse is open, then pauses on an explicit, settable gate
// (`__pendingGate`) standing in for the real courier's `await this.start()`.
let makeFakeLanguageClient: unit => Binding.LanguageClient.t = %raw(`function () {
  let activeConnection = { sendNotification: function () { return Promise.resolve(); } }
  let state = "Running"
  const client = {
    __stopCallCount: 0,
    start: function () { state = "Running"; return Promise.resolve() },
    stop: function (_timeout) {
      // The crew closes up for the night: closes the warehouse, hauls the
      // loading bay away, done. __stopCallCount is how the assertions
      // below check the crew closed up once, not zero times, not twice.
      client.__stopCallCount += 1
      state = "Stopping"
      activeConnection = undefined
      state = "Stopped"
      return Promise.resolve()
    },
    dispose: function () { return Promise.resolve(0) },
    onNotification: function () { return { dispose: function () {} } },
    sendRequest: function () { return Promise.resolve({}) },
    onRequest: function () { return { dispose: function () {} } },
    __pendingGate: Promise.resolve(),
    sendNotification: function (type, params) {
      const gate = client.__pendingGate
      return (async () => {
        if (state === "StartFailed" || state === "Stopping" || state === "Stopped") {
          throw new Error("Client is not running")
        }
        // The courier pauses before reaching the loading bay: the async
        // gap inside the real $start(). By the time they resume, the crew
        // may already have closed up.
        await gate
        const connection = activeConnection
        if (connection === undefined) {
          throw new Error("Starting server failed")
        }
        return connection.sendNotification(type, params)
      })()
    },
  }
  return client
}`)

// Sets how long the courier stays paused, i.e. exactly when they resume
// toward the loading bay.
let setPendingGate: (Binding.LanguageClient.t, promise<unit>) => unit = %raw(`function (client, gate) {
  client.__pendingGate = gate
}`)

// How many times the crew has closed up so far.
let getStopCallCount: Binding.LanguageClient.t => int = %raw(`function (client) {
  return client.__stopCallCount
}`)

// Wraps the fake client into an (unsafely cast) `Connection__Protocol__LSP.t`.
// `LSP.t` is opaque outside its module, but its runtime shape is just this
// record, the same unsafe-cast-via-%raw approach
// `Test__Registry__Connection.res` already uses for `Connection.t`.
let makeFakeLSPConnection: (
  Binding.LanguageClient.t,
  Connection__Transport.t,
  Chan.t<Js.Exn.t>,
  Chan.t<Js.Json.t>,
) => LSP.t = %raw(`function (client, method, errorChan, notificationChan) {
  return {
    client: client,
    id: "agda",
    name: "Agda Language Server",
    method: method,
    errorChan: errorChan,
    notificationChan: notificationChan,
  }
}`)

// Wraps the fake client as the ALS `Connection.t` variant agda-mode-vscode
// actually hands around.
let makeFakeALSConnection = (fakeClient: Binding.LanguageClient.t): Connection.t => {
  let lspConnection = makeFakeLSPConnection(
    fakeClient,
    Connection__Transport.ViaPipe("mock-als-path", []),
    Chan.make(),
    Chan.make(),
  )
  let alsConnection: Connection__Endpoint__ALS.t = {
    client: lspConnection,
    agdaVersion: "2.6.4",
    alsVersion: Some("4.0.0"),
    method: Connection__Transport.ViaPipe("mock-als-path", []),
  }
  Connection.ALS(
    alsConnection,
    "mock-als-path",
    {agdaVersion: "2.6.4", alsVersion: Some("4.0.0"), lspOptions: None},
  )
}

describe("ALS didClose lifecycle race (reproduction)", () => {
  Async.it(
    "an in-flight automatic didClose notification must not fail with 'Starting server failed' when the closing document releases the last connection user",
    async () => {
      await Registry__Connection.shutdown()

      let fakeClient = makeFakeLanguageClient()
      let alsConnection = makeFakeALSConnection(fakeClient)

      // The file signs on as the warehouse's only owner, so it will also
      // be the one whose closing tells the crew to close up.
      let acquireResult = await Registry__Connection.acquire("owner1", async () => Ok(alsConnection))
      Assert.deepStrictEqual(acquireResult, Ok(alsConnection))

      // The courier arrives for the last delivery: checks the warehouse is
      // open (it is), then pauses before reaching the loading bay. The gate
      // is what holds them paused for exactly as long as this test needs.
      let (gate, openGate, _) = Util.Promise_.pending()
      fakeClient->setPendingGate(gate)
      let didCloseNotification =
        fakeClient->Binding.LanguageClient.sendNotification("textDocument/didClose", Js.Json.null)

      // The file's contract ends. It's the last one, so agda-mode-vscode's
      // close listener drives Registry__Connection.terminate all the way to
      // Connection__Protocol__LSP.destroy, which calls the real
      // LanguageClient.stop binding: the crew closes up for the night.
      // Awaited fully before the gate opens, so the closing has definitely
      // happened by the time the courier resumes.
      await Registry__Connection.release("owner1")

      // The crew must have closed up already, and only once, before the
      // courier gets a chance to resume. Pinned here rather than after the
      // delivery resolves, so a fix can't pass by reordering things so the
      // crew closes up only after the courier is done.
      Assert.deepStrictEqual(fakeClient->getStopCallCount, 1)

      // The courier resumes. The loading bay is gone.
      openGate()

      let outcome = switch await didCloseNotification {
      | () => Ok()
      | exception Js.Exn.Error(e) => Error(e->Js.Exn.message->Option.getOr("unknown error"))
      }

      // The courier should handle finding the warehouse closed without
      // raising an alarm. Today it doesn't: it reports the exact
      // "Starting server failed" error from the real VS Code
      // extension-host logs.
      Assert.deepStrictEqual(outcome, Ok())

      // The crew didn't close up a second time during the delivery attempt.
      Assert.deepStrictEqual(fakeClient->getStopCallCount, 1)
    },
  )
})
