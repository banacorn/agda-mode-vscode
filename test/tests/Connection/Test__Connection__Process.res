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
        let endpoint = await Connection__Endpoint__Agda.make("fake-agda", "2.8.0")
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
