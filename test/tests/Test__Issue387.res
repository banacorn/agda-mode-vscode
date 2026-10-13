open Mocha
open Test__Util

module Sys = {
  type execOptions = {encoding: string}
  @module("node:child_process")
  external execFileSync: (string, array<string>, execOptions) => string = "execFileSync"
  @val @scope("process") external kill: (int, string) => unit = "kill"
  @val @scope("process") external pid: int = "pid"
}

// `ps` pads the start time of a process differently from one call to the next
let squeeze = text => text->String.trim->String.replaceRegExp(%re("/\s+/g"), " ")

// When the process started, as `ps` prints it. The text depends on the locale, so
// it is only ever compared with what the same call printed before.
let startOf = pid =>
  squeeze(Sys.execFileSync("ps", ["-p", Int.toString(pid), "-o", "lstart="], {encoding: "utf8"}))

// The interaction processes that descend from this process, each as its PID and
// the time it started. The tests run inside the extension host, which is also
// where the extension spawns Agda, so another Agda on the machine is never among
// them. `ps` is run without a shell, so no shell's own command line can match.
//
// NOTE: `%re` takes the regex as it is written, so `\s` is whitespace here, and
// `\\s` would be a backslash followed by an `s`.
let ownAgdaProcesses = (): array<(int, string)> =>
  if OS.onUnix {
    try {
      let rows =
        Sys.execFileSync("ps", ["-axo", "pid=,ppid=,command="], {encoding: "utf8"})
        ->String.split("\n")
        ->Array.filterMap(line =>
          switch String.match(line, %re("/^\s*(\d+)\s+(\d+)\s+(.*)$/")) {
          | Some([_, Some(pid), Some(ppid), Some(command)]) =>
            switch (Int.fromString(pid), Int.fromString(ppid)) {
            | (Some(pid), Some(ppid)) => Some((pid, ppid, command))
            | _ => None
            }
          | _ => None
          }
        )
      let descendants = ref([Sys.pid])
      rows->Array.forEach(_ =>
        rows->Array.forEach(((pid, ppid, _)) =>
          if descendants.contents->Array.includes(ppid) && !(descendants.contents->Array.includes(pid)) {
            descendants := Array.concat(descendants.contents, [pid])
          }
        )
      )
      rows
      ->Array.filter(((pid, _, command)) =>
        descendants.contents->Array.includes(pid) && String.includes(command, "agda --interaction")
      )
      ->Array.filterMap(((pid, _, _)) =>
        try {
          Some((pid, startOf(pid)))
        } catch {
        | _ => None // gone since the listing
        }
      )
    } catch {
    | _ => []
    }
  } else {
    []
  }

// Kills the process if it is still the interaction process that started at
// `start`. A PID can be reused once its process is gone, so the start time and
// the command line are both checked again first.
let killIfStillAgda = ((pid, start)) =>
  try {
    let command = Sys.execFileSync("ps", ["-p", Int.toString(pid), "-o", "command="], {encoding: "utf8"})
    if startOf(pid) == start && String.includes(command, "agda --interaction") {
      Sys.kill(pid, "SIGKILL")
    }
  } catch {
  | _ => () // `ps` exits with 1 when the process is already gone
  }

// The extension runs from its webpack bundle while the tests import `lib/js`, so
// `Registry` and `Registry__Connection` here are not the ones the extension uses.
// Resetting them from a test does nothing to the live connection, which every
// earlier test leaves behind with a user still attached, and a connection with
// another user is not terminated when this test's state quits. Only commands
// reach the live registry, so the connection is reset through them:
//   - restart shuts the live connection down, whoever still uses it, and loads
//     the open file on a new one
//   - quitting that file's state releases the new connection's only user, which
//     terminates it
let resetLiveConnection = async () => {
  // a file without holes, so loading it does not edit the document
  let _ = await File.open_(Path.asset("InputMethod.agda"))
  let _ = await executeCommand("agda-mode.restart")
  let _ = await executeCommand("agda-mode.quit")
}

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

  // settles once the destruction of the state has completed
  let destroyed = ref(Promise.resolve())
  // the interaction processes this test started, recorded while they still
  // descend from this process
  let recordedAgda = ref([])

  // Start with the live connection to itself: this test's state must be its only
  // user, or quitting would leave the connection, and the request on it, alive.
  Async.beforeEach(async () => {
    let candidate = await AgdaMode.commandExists("agda")
    let _ = await activateExtension(Some(candidate))
    await resetLiveConnection()
  })

  // If quitting hung, a busy Agda is still running on the live connection and
  // would hold up every test after this one.
  Async.afterEach(async () => {
    // Quitting kills the shell the extension spawned Agda through, which leaves
    // Agda running with no parent: it is no longer one of this process's
    // descendants, so it can only be found by the PIDs recorded beforehand.
    recordedAgda.contents->Array.forEach(killIfStillAgda)
    // the teardown that was stuck ends now that its process is gone
    let settled = await Promise.race([
      destroyed.contents->Promise.thenResolve(_ => true),
      Util.Promise_.setTimeout(5000)->Promise.thenResolve(_ => false),
    ])
    if !settled {
      // resetting the connection would wait on the same teardown
      raise(Failure("the connection is still being destroyed after its Agda was killed"))
    }
    await resetLiveConnection()
  })

  Async.it("finishes destroying the state", async () => {
    destroyed := Promise.resolve()
    recordedAgda := []

    let channels = await activateExtension(None)
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
    // The load never finishes, so it is not awaited. Tearing the connection
    // down ends it with an error, which is of no interest here.
    executeCommand("agda-mode.load")
    ->Promise.catch(_ => Promise.resolve(None))
    ->ignore
    await checking
    unsubscribe()
    recordedAgda := ownAgdaProcesses()
    // An empty record would let a hung Quit go uncleaned without anyone noticing,
    // which is how a broken `ps` parser once hid on every platform.
    if OS.onUnix && Array.length(recordedAgda.contents) == 0 {
      raise(Failure("found no interaction process to record while Agda was busy"))
    }

    destroyed :=
      Log.on(channels.log, log =>
        switch log {
        | Log.Others("State.destroy: Connection released, destruction complete") => true
        | _ => false
        }
      )
    let timeout = Util.Promise_.setTimeout(5000)->Promise.thenResolve(_ => "timed out")

    // Not awaited either: if quitting hangs, so would this.
    executeCommand("agda-mode.quit")
    ->Promise.catch(_ => Promise.resolve(None))
    ->ignore

    Assert.deepStrictEqual(
      await Promise.race([destroyed.contents->Promise.thenResolve(_ => "destroyed"), timeout]),
      "destroyed",
    )
  })
})
