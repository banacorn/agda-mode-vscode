open Mocha
open Test__Util

describe("Connection", () => {
  describe("URI.parse", () => {
    Async.it(
      "should be able to parse URIs with lsp: as the protocol",
      async () => {
        let actual = Connection__URI.parse("lsp://path/to/als")
        let expected = Connection__URI.URL(NodeJs.Url.make("lsp://path/to/als"))
        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should be able to parse file paths",
      async () => {
        let actual = Connection__URI.parse("path/to/als")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("path/to/als")
        } else {
          Connection__URI.Filepath("path\\to\\als")
        }
        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should be able to parse convert \"/c/path/to/agda\" to \"c:/path/to/agda\" on Windows",
      async () => {
        let actual = Connection__URI.parse("/c/path/to/agda")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("/c/path/to/agda")
        } else {
          Connection__URI.Filepath("c:\\path\\to\\agda")
        }

        Assert.deepEqual(actual, expected)

        let actual = Connection__URI.parse("/d/path/to/agda")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("/d/path/to/agda")
        } else {
          Connection__URI.Filepath("d:\\path\\to\\agda")
        }

        Assert.deepEqual(actual, expected)
      },
    )
  })

  describe("Target", () => {
    let agdaMockPath = ref("")
    let agdaMockTarget = ref(None)

    Async.before(
      async () => {
        // setup the Agda mock
        agdaMockPath := (await Target.Agda.mock(~version="2.7.0.1", ~name="agda-mock"))

        switch await Connection.Target.fromRawPath(agdaMockPath.contents) {
        | Ok(target) => agdaMockTarget := Some(target)
        | Error(_) => failwith("Got error when trying to construct a mock for Agda ")
        }
      },
    )

    Async.it(
      "should return the previously picked connection",
      async () => {
        // access the Agda mock
        let agdaMockTarget = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the momento
        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))

        let paths = [agdaMockPath.contents, "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Ok(agdaMockTarget)

        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when there's no previously picked connection",
      async () => {
        // setup the momento
        let memento = State__Memento.make(None)
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = if OS.onUnix {
          Error([
            Connection__Target.Error.ValidationError("path/to/agda", NotFound("path/to/agda")),
            Connection__Target.Error.ValidationError("path/to/als", NotFound("path/to/als")),
          ])
        } else {
          Error([
            Connection__Target.Error.ValidationError("path\\to\\agda", NotFound("path\\to\\agda")),
            Connection__Target.Error.ValidationError("path\\to\\als", NotFound("path\\to\\als")),
          ])
        }

        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when the previously picked connection is not in the supplied paths",
      async () => {
        // access the Agda mock
        let agdaMockTarget = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the momento
        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = if OS.onUnix {
          Error([
            Connection__Target.Error.ValidationError("path/to/agda", NotFound("path/to/agda")),
            Connection__Target.Error.ValidationError("path/to/als", NotFound("path/to/als")),
          ])
        } else {
          Error([
            Connection__Target.Error.ValidationError("path\\to\\agda", NotFound("path\\to\\agda")),
            Connection__Target.Error.ValidationError("path\\to\\als", NotFound("path\\to\\als")),
          ])
        }

        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should return the first usable connection target when the previously picked connection is invalid or not in the supplied paths",
      async () => {
        // access the Agda mock
        let agdaMockTarget = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the momento
        let memento = State__Memento.make(None)
        let paths =
          [
            "path/to/non-existent-agda",
            agdaMockPath.contents,
            "path/to/non-existent-als",
          ]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Ok(agdaMockTarget)

        Assert.deepEqual(actual, expected)
      },
    )

    Async.after(
      async () => {
        // cleanup the Agda mock
        switch agdaMockTarget.contents {
        | Some(target) =>
          Target.Agda.destroy(target)
          agdaMockTarget := None
        | None => ()
        }
      },
    )
  })

  describe("Command searching", () => {

    Async.it(
      "should be able to find itself (`which` or `where`), although it's not a valid target",
      async () => {
        if OS.onUnix {
          switch await Connection__Command.search("which") {
          | Ok(_output) => failwith("should not be a valid target")
          | Error(Connection__Command.Error.NotFound(_)) => failwith("expected to find `which`")
          | Error(SomethingWentWrong(_)) => failwith("expected to find `which`")
          | Error(NotValidTarget(_, _, _)) => ()
          }
        } else {
          switch await Connection__Command.search("where") {
          | Ok(_output) => failwith("should not be a valid target")
          | Error(Connection__Command.Error.NotFound(_)) => failwith("expected to find `where`")
          | Error(SomethingWentWrong(_)) => failwith("expected to find `where`")
          | Error(NotValidTarget(_, _, _)) => ()
          }
        }
      },
    )
    Async.it(
      "should return an error when the command is not found",
      async () => {
        switch await Connection__Command.search("non-existent-command") {
        | Ok(_output) => failwith("expected to not find `non-existent-command`")
        | Error(_) => ()
        }
      },
    )
  })

  describe("findCommands", () => {
    Async.it(
      "should return the connection when a command is found",
      async () => {
        let commands = ["agda", "als"]
        switch await Connection.findCommands(commands) {
        | Ok(_) => ()
        | Error(error) =>
          // let (header, body) = Connection.Error.toString(CommandsNotFound(error))
          // failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
          failwith("expected to find `agda` or `als`")
        }
      },
    )

    Async.it(
      "should return an error when the command is not found",
      async () => {
        let commands = ["non-existent-command"]
        switch await Connection.findCommands(commands) {
        | Ok(_) => failwith("expected to not find `non-existent-command`")
        | Error(_) => ()
        }
      },
    )
  })

  describe("make", () => {
    Async.it(
      "Memento: [] / paths: [] / commands: ['agda', 'als']",
      async () => {
        // get the Target of `agda` first
        let target = switch await Connection.findCommands(["agda"]) {
        | Ok(target) => target
        | Error(_) => failwith("expected to find `agda`")
        }

        // remove all paths in the config
        await Config.Connection.setAgdaPaths([])
        let paths = Config.Connection.getAgdaPaths()

        let memento = State__Memento.make(None)
        let commands = ["agda", "als"]
        let platform = await Connection__Download__Platform.determine()
        let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Undecided
        let downloadLatestALS = async _ => Error(
          Connection__Download__Error.CannotFindCompatibleALSRelease,
        ) // NOTE: temporary
        switch await Connection.make(
          memento,
          paths,
          commands,
          platform,
          getDownloadPolicy,
          downloadLatestALS,
        ) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }

        Assert.deepEqual(
          Config.Connection.getAgdaPaths(),
          [target]->Array.map(Connection__Target.toURI),
        )

        let pathIsNowInConfig =
          Config.Connection.getAgdaPaths()->Util.Array.includes(target->Connection__Target.toURI)
        Assert.ok(pathIsNowInConfig)

        switch await Connection.Target.getPicked(memento, Config.Connection.getAgdaPaths()) {
        | Error(_) => failwith("expected to find the picked connection")
        | Ok(picked) => Assert.deepStrictEqual(picked, target)
        }
      },
    )

    Async.it(
      "Memento: [] / paths: ['some/other/path'] / commands: ['agda', 'als']",
      async () => {
        // get the Target of `agda` first
        let target = switch await Connection.findCommands(["agda"]) {
        | Ok(target) => target
        | Error(_) => failwith("expected to find `agda`")
        }

        await Config.Connection.setAgdaPaths(["some/other/path"]->Array.map(Connection__URI.parse))
        let paths = Config.Connection.getAgdaPaths()

        let memento = State__Memento.make(None)
        let commands = ["agda", "als"]
        let platform = await Connection__Download__Platform.determine()
        let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Undecided
        let downloadLatestALS = async _ => Error(
          Connection__Download__Error.CannotFindCompatibleALSRelease,
        ) // NOTE: temporary
        switch await Connection.make(
          memento,
          paths,
          commands,
          platform,
          getDownloadPolicy,
          downloadLatestALS,
        ) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }

        Assert.deepEqual(
          Config.Connection.getAgdaPaths(),
          [...paths, target->Connection__Target.toURI],
        )

        let pathIsNowInConfig =
          Config.Connection.getAgdaPaths()->Util.Array.includes(target->Connection__Target.toURI)
        Assert.ok(pathIsNowInConfig)

        switch await Connection.Target.getPicked(memento, Config.Connection.getAgdaPaths()) {
        | Error(_) => failwith("expected to find the picked connection")
        | Ok(picked) => Assert.deepStrictEqual(picked, target)
        }
      },
    )

    Async.it(
      "Memento: [] / paths: ['agda', 'others'] / commands: ['agda', 'als']",
      async () => {
        // get the Target of `agda` first
        let target = switch await Connection.findCommands(["agda"]) {
        | Ok(target) => target
        | Error(_) => failwith("expected to find `agda`")
        }

        await Config.Connection.setAgdaPaths([
          target->Connection.Target.toURI,
          Connection__URI.parse("some/other/path"),
        ])
        let paths = Config.Connection.getAgdaPaths()

        let memento = State__Memento.make(None)
        let commands = ["agda", "als"]
        let platform = await Connection__Download__Platform.determine()
        let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Undecided
        let downloadLatestALS = async _ => Error(
          Connection__Download__Error.CannotFindCompatibleALSRelease,
        ) // NOTE: temporary
        switch await Connection.make(
          memento,
          paths,
          commands,
          platform,
          getDownloadPolicy,
          downloadLatestALS,
        ) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }

        Assert.deepEqual(Config.Connection.getAgdaPaths(), paths)

        switch await Connection.Target.getPicked(memento, Config.Connection.getAgdaPaths()) {
        | Error(_) => failwith("expected to find the picked connection")
        | Ok(picked) => Assert.deepStrictEqual(picked, target)
        }
      },
    )

    describe(
      "No local installation",
      () => {
        // input
        let memento = State__Memento.make(None)
        let paths = [Connection__URI.parse("some/other/path")]
        let commands = ["non-existent-command"]
        // expected output
        let expectedAttempts = {
          Connection.Error.Aggregated.targets: [
            {
              uri: Connection__URI.parse("some/other/path"),
              error: Connection__Target.Error.ValidationError(
                "some/other/path",
                NotFound("some/other/path"),
              ),
            },
          ],
          commands: [NotFound("non-existent-command")],
        }

        Async.it(
          "should throw the `PlatformNotSupported` error when the platform is not supported",
          async () => {
            let platform = {
              "os": "non-existent-os",
              "dist": "non-existent-dist",
              "codename": "non-existent-codename",
              "release": "non-existent-release",
            }
            await Config.Connection.DownloadPolicy.set(Undecided)
            let getDownloadPolicyCount = ref(0)
            let getDownloadPolicy = async () => {
              getDownloadPolicyCount := getDownloadPolicyCount.contents + 1
              Config.Connection.DownloadPolicy.Undecided
            }
            let downloadLatestALS = async _ => Error(
              Connection__Download__Error.CannotFindCompatibleALSRelease,
            ) // don't care
            let result = await Connection.make(
              memento,
              paths,
              commands,
              Error(platform),
              getDownloadPolicy,
              downloadLatestALS,
            )

            Assert.deepEqual(
              result,
              Error(Aggregated(PlatformNotSupported(expectedAttempts, platform))),
            )

            // should not ask the user for download policy
            Assert.deepEqual(getDownloadPolicyCount.contents, 0)
          },
        )

        Async.it(
          "should throw the `NoDownloadALS` error when the inital download policy is `No`",
          async () => {
            let platform = Connection__Download__Platform.Windows

            await Config.Connection.DownloadPolicy.set(No)
            let getDownloadPolicyCount = ref(0)
            let getDownloadPolicy = async () => {
              getDownloadPolicyCount := getDownloadPolicyCount.contents + 1
              Config.Connection.DownloadPolicy.No // don't care, shouldn't be invoked
            }
            let downloadLatestALS = async _ => Error(
              Connection__Download__Error.CannotFindCompatibleALSRelease,
            ) // don't care
            let result = await Connection.make(
              memento,
              paths,
              commands,
              Ok(platform),
              getDownloadPolicy,
              downloadLatestALS,
            )
            Assert.deepEqual(result, Error(Aggregated(NoDownloadALS(expectedAttempts))))

            let policy = Config.Connection.DownloadPolicy.get()
            Assert.deepEqual(policy, Config.Connection.DownloadPolicy.No)

            // should not ask the user for download policy
            Assert.deepEqual(getDownloadPolicyCount.contents, 0)
          },
        )

        Async.it(
          "should throw the `NoDownloadALS` error when the user clicked `cancel` on the download dialog",
          async () => {
            let platform = Connection__Download__Platform.Windows

            await Config.Connection.DownloadPolicy.set(Undecided)
            let getDownloadPolicyCount = ref(0)
            let getDownloadPolicy = async () => {
              getDownloadPolicyCount := getDownloadPolicyCount.contents + 1
              Config.Connection.DownloadPolicy.Undecided
            }
            let downloadLatestALS = async _ => Error(
              Connection__Download__Error.CannotFindCompatibleALSRelease,
            ) // don't care
            let result = await Connection.make(
              memento,
              paths,
              commands,
              Ok(platform),
              getDownloadPolicy,
              downloadLatestALS,
            )
            Assert.deepEqual(result, Error(Aggregated(NoDownloadALS(expectedAttempts))))

            let policy = Config.Connection.DownloadPolicy.get()
            Assert.deepEqual(policy, Config.Connection.DownloadPolicy.No)

            // should ask the user for download policy exactly once
            Assert.deepEqual(getDownloadPolicyCount.contents, 1)
          },
        )

        Async.it(
          "should throw the `DownloadALS` error when the download policy is `Yes` but the download fails",
          async () => {
            let platform = Connection__Download__Platform.Windows

            await Config.Connection.DownloadPolicy.set(Undecided)
            let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let downloadLatestALS = async _ => Error(
              Connection__Download__Error.CannotFindCompatibleALSRelease,
            )
            let result = await Connection.make(
              memento,
              paths,
              commands,
              Ok(platform),
              getDownloadPolicy,
              downloadLatestALS,
            )
            Assert.deepEqual(
              result,
              Error(
                Aggregated(
                  DownloadALS(
                    expectedAttempts,
                    Connection__Download__Error.CannotFindCompatibleALSRelease,
                  ),
                ),
              ),
            )

            let policy = Config.Connection.DownloadPolicy.get()
            Assert.deepEqual(policy, Config.Connection.DownloadPolicy.Yes)
          },
        )
      },
    )
  })
})
