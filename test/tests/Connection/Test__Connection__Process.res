open Mocha

module Process = Connection__Transport__Process

type agdaEndpointOutcome =
  | Settled(result<unit, Connection__Error.CommWithAgda.t>)
  | TimedOut

module StatusIntrospection = {
  type t
  @get external status: Process.t => t = "status"
  @get external tag: t => option<string> = "TAG"
}

module Fs = {
  type rmOptions = {recursive: bool, force: bool}
  @val @scope("process") external execPath: string = "execPath"
  @module("node:fs") external mkdtempSync: string => string = "mkdtempSync"
  @module("node:fs") external realpathSync: string => string = "realpathSync"
  @module("node:fs") external writeFileSync: (string, string) => unit = "writeFileSync"
  @module("node:fs") external rmSync: (string, rmOptions) => unit = "rmSync"
}

type cwdProbe = {
  exitCode: option<int>,
  stdout: string,
  stderr: string,
  error: option<string>,
}

// Spawn `path args` through `Process.make ~cwd` and report what the child
// printed. The child is always unsubscribed from and destroyed.
let probeChild = async (~shell, ~cwd, path, args): cwdProbe => {
  let process = Process.make(~shell, ~cwd, path, args)
  let stdout = ref("")
  let stderr = ref("")
  let (exited, resolve, _) = Util.Promise_.pending()
  let unsubscribe = process->Process.onOutput(output =>
    switch output {
    | Stdout(chunk) => stdout := stdout.contents ++ chunk
    | Stderr(chunk) => stderr := stderr.contents ++ chunk
    | Event(OnExit(code)) => resolve({exitCode: Some(code), stdout: "", stderr: "", error: None})
    | Event(event) =>
      resolve({exitCode: None, stdout: "", stderr: "", error: Some(Process.Event.toString(event))})
    }
  )
  let timeout =
    Util.Promise_.setTimeout(8000)->Promise.thenResolve(_ => {
      exitCode: None,
      stdout: "",
      stderr: "",
      error: Some("timed out"),
    })
  let outcome = await Promise.race([exited, timeout])
  unsubscribe()
  let _ = await Process.destroy(process)
  {...outcome, stdout: stdout.contents, stderr: stderr.contents}
}

// Run `f` with a fresh temporary directory, removed even if `f` throws.
let withTempDir = async f => {
  let dir = Fs.mkdtempSync(NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-mode-cwd-"]))
  let cleanup = () => Fs.rmSync(dir, {recursive: true, force: true})
  switch await f(dir) {
  | result =>
    cleanup()
    result
  | exception exn =>
    cleanup()
    raise(exn)
  }
}

describe("Process Interface", () => {
  Async.it(
    "Agda endpoint should settle a request when the process writes to stderr",
    async () => {
      let restoreSpawn: unit => unit = %raw(`(() => {
        const cp = require("node:child_process");
        const originalSpawn = cp.spawn;

        cp.spawn = function () {
          const handlers = {};
          let stderrData;

          return {
            stdout: {
              on: function () {
                return this;
              },
            },
            stderr: {
              on: function (event, cb) {
                if (event === "data") stderrData = cb;
                return this;
              },
            },
            stdin: {
              write: function () {
                stderrData(Buffer.from("backend failed\n"));
                return true;
              },
            },
            pid: 454545,
            on: function (event, cb) {
              handlers[event] = cb;
              return this;
            },
            kill: function () {
              if (handlers["close"]) handlers["close"](1);
              return true;
            },
          };
        };

        return () => {
          cp.spawn = originalSpawn;
        };
      })()`)

      let error = ref(None)
      let outcome = ref(TimedOut)
      let _ = switch await (async () => {
        let endpoint = await Connection__Endpoint__Agda.make(
          ~cwd=NodeJs.Process.cwd(NodeJs.Process.process),
          "fake-agda",
          "2.8.0",
        )
        let completion = endpoint
          ->Connection__Endpoint__Agda.sendRequest("request", _response => Promise.resolve())
          ->Promise.thenResolve(result => Settled(result))
        let timeout = Util.Promise_.setTimeout(250)->Promise.thenResolve(_ => TimedOut)
        outcome := (await Promise.race([completion, timeout]))
        await endpoint->Connection__Endpoint__Agda.destroy
      })() {
      | _ => ()
      | exception exn =>
        error := Some(exn)
        ()
      }

      restoreSpawn()
      error.contents->Option.forEach(exn => raise(exn))

      switch outcome.contents {
      | Settled(Error(_)) => Assert.ok(true)
      | Settled(Ok()) => Assert.fail("Expected stderr to produce an endpoint error")
      | TimedOut => Assert.fail("Agda endpoint request remained pending after stderr")
      }
    },
  )

  describe("`~cwd`", () => {
    // The child reports its own working directory, so this runs the real
    // `spawn` on every platform. Both branches of `Process.make` are covered.
    Async.it("direct spawn starts the child in the requested directory", async () => {
      let actual = await withTempDir(async dir => {
        let probe = await probeChild(
          ~shell=false,
          ~cwd=dir,
          Fs.execPath,
          ["-e", "process.stdout.write(process.cwd())"],
        )
        ({...probe, stdout: Fs.realpathSync(probe.stdout)}, Fs.realpathSync(dir))
      })
      let (probe, expectedCwd) = actual
      Assert.deepStrictEqual(
        probe,
        {exitCode: Some(0), stdout: expectedCwd, stderr: "", error: None},
      )
    })

    Async.it("shell spawn starts the child in the requested directory", async () => {
      let actual = await withTempDir(async dir => {
        // the script lives outside `dir`, so `dir` can only be the cwd
        let scriptDir = Fs.mkdtempSync(NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-mode-cwd-script-"]))
        let script = NodeJs.Path.join([scriptDir, "cwd.js"])
        Fs.writeFileSync(script, "process.stdout.write(process.cwd())")
        let probe = switch await probeChild(
          ~shell=true,
          ~cwd=dir,
          Fs.execPath,
          ["\"" ++ script ++ "\""],
        ) {
        | probe =>
          Fs.rmSync(scriptDir, {recursive: true, force: true})
          probe
        | exception exn =>
          Fs.rmSync(scriptDir, {recursive: true, force: true})
          raise(exn)
        }
        ({...probe, stdout: Fs.realpathSync(probe.stdout)}, Fs.realpathSync(dir))
      })
      let (probe, expectedCwd) = actual
      Assert.deepStrictEqual(
        probe,
        {exitCode: Some(0), stdout: expectedCwd, stderr: "", error: None},
      )
    })
  })

  describe("Use `echo` as the testing subject", () => {
    // TODO: fix this test case on Windows
    Async.it_skip(
      "should trigger `close`",
      async () => {
        let process = Process.make("echo", ["hello"])
        let (promise, resolve, reject) = Util.Promise_.pending()
        let destructor = process->Process.onOutput(
          output => {
            switch output {
            | Stdout("hello\n") => ()
            | Stdout("hello\r\n") => ()
            | Stdout(output) => reject(Js.Exn.raiseError("wrong output: " ++ output))
            | Stderr(err) => resolve(Js.Exn.raiseError("Stderr: " ++ err))
            | Event(OnExit(0)) => resolve()
            | Event(event) => resolve(Js.Exn.raiseError("Event: " ++ Process.Event.toString(event)))
            }
          },
        )

        await promise
        destructor() // destroy the process after the test
      },
    )
  })
  describe("Use a non-existing command as the testing subject", () => {
    Async.it(
      "should trigger receive something from stderr",
      async () => {
        let process = Process.make("echooo", ["hello"])
        let (promise, resolve, reject) = Util.Promise_.pending()

        let destructor = process->Process.onOutput(
          output => {
            switch output {
            | Stdout(output) => reject(Js.Exn.raiseError("wrong output: " ++ output))
            | Stderr(_) => resolve()
            | Event(event) => reject(Js.Exn.raiseError("Event: " ++ Process.Event.toString(event)))
            }
          },
        )
        await promise
        destructor() // destroy the process after the test
      },
    )
  })

  Async.it(
    "destroy should wait for process close before resolving",
    async () => {
      // Patch child_process.spawn with a fake process that emits `close` after a delay.
      let restoreSpawn: unit => unit = %raw(`(() => {
        const cp = require("node:child_process");
        const originalSpawn = cp.spawn;
        globalThis.__agdaModeProcessCloseFired = false;

        cp.spawn = function () {
          const handlers = {};
          const mkStream = () => ({
            on: function (_event, _cb) {
              return this;
            },
          });

          return {
            stdout: mkStream(),
            stderr: mkStream(),
            stdin: {
              write: function () {
                return true;
              },
            },
            pid: 424242,
            on: function (event, cb) {
              handlers[event] = cb;
              return this;
            },
            kill: function (_signal) {
              setTimeout(() => {
                globalThis.__agdaModeProcessCloseFired = true;
                if (handlers["close"]) {
                  handlers["close"](137);
                }
              }, 50);
              return true;
            },
          };
        };

        return () => {
          cp.spawn = originalSpawn;
          delete globalThis.__agdaModeProcessCloseFired;
        };
      })()`)

      let closeFiredWhenDestroyResolved = ref(false)
      let error = ref(None)

      let _ = switch await (async () => {
        let process = Process.make(~shell=false, "fake-process", [])
        let _ = await process->Process.destroy
        closeFiredWhenDestroyResolved := %raw(`globalThis.__agdaModeProcessCloseFired === true`)
      })() {
      | _ => ()
      | exception exn =>
        error := Some(exn)
        ()
      }

      restoreSpawn()
      error.contents->Option.forEach(exn => raise(exn))

      Assert.deepStrictEqual(closeFiredWhenDestroyResolved.contents, true)
    },
  )

  Async.it(
    "destroy should not remain in destroying state after destroy resolves",
    async () => {
      // Reuse the same deterministic fake process setup to control close timing.
      let restoreSpawn: unit => unit = %raw(`(() => {
        const cp = require("node:child_process");
        const originalSpawn = cp.spawn;

        cp.spawn = function () {
          const handlers = {};
          const mkStream = () => ({
            on: function (_event, _cb) {
              return this;
            },
          });

          return {
            stdout: mkStream(),
            stderr: mkStream(),
            stdin: {
              write: function () {
                return true;
              },
            },
            pid: 434343,
            on: function (event, cb) {
              handlers[event] = cb;
              return this;
            },
            kill: function (_signal) {
              setTimeout(() => {
                if (handlers["close"]) {
                  handlers["close"](137);
                }
              }, 10);
              return true;
            },
          };
        };

        return () => {
          cp.spawn = originalSpawn;
        };
      })()`)

      let finalTag = ref(None)
      let error = ref(None)

      let _ = switch await (async () => {
        let process = Process.make(~shell=false, "fake-process", [])
        let _ = await process->Process.destroy
        finalTag := process->StatusIntrospection.status->StatusIntrospection.tag
      })() {
      | _ => ()
      | exception exn =>
        error := Some(exn)
        ()
      }

      restoreSpawn()
      error.contents->Option.forEach(exn => raise(exn))

      switch finalTag.contents {
      | Some("Destroying") =>
        Assert.fail("Expected a stable post-destroy state, but status remained Destroying")
      | _ => Assert.ok(true)
      }
    },
  )

  //   describe("Use `node` as the testing subject", () => {
  //     Async.it(
  //       "should behave normally",
  //       async () => {
  //         switch await Source.Search.run("node") {
  //         | Error(err) => reject(Js.Exn.raiseError(Search.Path.Error.toString))
  //         | Ok(path) => {
  //             let process = Process.make("path", [])
  //             let (promise, resolve, reject) = Promise.pending()

  //             let destructor = process->Process.onOutput(
  //               output =>
  //                 switch output {
  //                 | Stdout("2") => resolve(Ok())
  //                 | Stdout(_) => reject(Js.Exn.raiseError("wrong answer"))
  //                 | Stderr(err) => reject(Js.Exn.raiseError("Stderr: " ++ err))
  //                 | Event(event) =>
  //                   reject(Js.Exn.raiseError("Event: " ++ snd(Process.Event.toString(event))))
  //                 },
  //             )

  //             // let sent = process->Process.send("1 + 1")
  //             // Assert.ok(sent)

  //             // process->Process.destroy->Promise.flatMap(_ => {
  //             //   method()
  //             //   promise
  //             // })

  //             promise->Promise.tap(_ => method())
  //           }

  //         //   let search: t => Promise.t<result<Method.t, Error.t>>
  //         //   ->Promise.mapError(Search.Path.Error.toString)
  //         //   ->Promise.flatMapOk(path => {
  //         //     let process = Process.make("path", [])
  //         //     let (promise, resolve) = Promise.pending()

  //         //     let method = process->Process.onOutput(output =>
  //         //       switch output {
  //         //       | Stdout("2") => resolve(Ok())
  //         //       | Stdout(_) => resolve(Error("wrong answer"))
  //         //       | Stderr(err) => resolve(Error("Stderr: " ++ err))
  //         //       | Event(event) => resolve(Error("Event: " ++ snd(Process.Event.toString(event))))
  //         //       }
  //         //     )

  //         // let sent = process->Process.send("1 + 1")
  //         // Assert.ok(sent)

  //         // process->Process.destroy->Promise.flatMap(_ => {
  //         //   method()
  //         //   promise
  //         // })

  //         //     promise->Promise.tap(_ => method())
  //         //   })
  //         }
  //       },
  //     )
  //   })
})
