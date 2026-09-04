open Mocha

module Command = Connection__Command

describe("Connection__Command", () => {
  Async.it(
    "should retry a bounded number of times and kill each hung process when discovery keeps timing out",
    async () => {
      // Patch child_process.spawn with a fake process that never responds, simulating
      // `where.exe`/`command -v` hanging on a loaded runner. Each spawned process is
      // tracked so the test can verify both the retry count and that every hung
      // process was actually killed (not left orphaned).
      let restoreSpawn: unit => unit = %raw(`(() => {
        const cp = require("node:child_process");
        const originalSpawn = cp.spawn;
        let spawnCount = 0;
        const killedPids = [];

        cp.spawn = function () {
          spawnCount++;
          const pid = 1000 + spawnCount;
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
            pid,
            on: function (event, cb) {
              handlers[event] = cb;
              return this;
            },
            kill: function (_signal) {
              killedPids.push(pid);
              // resolve close shortly after being killed, like a real process would
              setTimeout(() => {
                if (handlers["close"]) handlers["close"](137);
              }, 5);
              return true;
            },
          };
        };

        globalThis.__agdaModeSpawnCount = () => spawnCount;
        globalThis.__agdaModeKilledPids = () => killedPids.slice();

        return () => {
          cp.spawn = originalSpawn;
          delete globalThis.__agdaModeSpawnCount;
          delete globalThis.__agdaModeKilledPids;
        };
      })()`)

      let result = ref(None)
      let error = ref(None)

      let _ = switch await (async () => {
        result :=
          Some(await Command.searchWith(~timeout=30, ~retries=2, ~backoff=10, "agda", []))
      })() {
      | _ => ()
      | exception exn =>
        error := Some(exn)
        ()
      }

      // give the last `kill`'s deferred `close` a chance to fire before restoring spawn
      await Util.Promise_.setTimeout(20)

      let spawnCount: int = %raw(`globalThis.__agdaModeSpawnCount()`)
      let killedPids: array<int> = %raw(`globalThis.__agdaModeKilledPids()`)

      restoreSpawn()
      error.contents->Option.forEach(exn => raise(exn))

      // one initial attempt plus two retries
      Assert.deepStrictEqual(spawnCount, 3)
      // every hung process must have been terminated, none left orphaned
      Assert.deepStrictEqual(Array.length(killedPids), 3)

      switch result.contents {
      | Some(Error(Command.Error.SomethingWentWrong(ProcessHanging(30)))) => Assert.ok(true)
      | Some(other) =>
        Assert.fail(
          "Expected all retries to time out with ProcessHanging(30), got: " ++
          Command.Error.toString(
            switch other {
            | Error(e) => e
            | Ok(_) => Command.Error.InternalError
            },
          ),
        )
      | None => Assert.fail("searchWith did not resolve")
      }
    },
  )
})
