open Mocha
open Test__Util

let getAgdaTarget = async () => {
  switch await Connection.findCommands(["agda"]) {
  | Ok(target) => target
  | Error(_) => failwith("expected to find `agda`")
  }
}

describe("Connection", () => {
  This.timeout(10000)
  describe("URI.parse", () => {
    Async.it(
      "should be able to parse URIs with lsp: as the protocol",
      async () => {
        let actual = Connection__URI.parse("lsp://path/to/als")
        let expected = Connection__URI.URL(NodeJs.Url.make("lsp://path/to/als"))
        Assert.deepStrictEqual(actual, expected)
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
        Assert.deepStrictEqual(actual, expected)
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

        Assert.deepStrictEqual(actual, expected)

        let actual = Connection__URI.parse("/d/path/to/agda")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("/d/path/to/agda")
        } else {
          Connection__URI.Filepath("d:\\path\\to\\agda")
        }

        Assert.deepStrictEqual(actual, expected)
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

        // setup the memento
        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))

        let paths = [agdaMockPath.contents, "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Ok(agdaMockTarget)

        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when there's no previously picked connection",
      async () => {
        // setup the memento
        let memento = State__Memento.make(None)
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        if OS.onUnix {
          let expected = Error([
            Connection__Target.Error.SomethingWentWrong(
              Connection.URI.parse("path/to/agda"),
              NotFound("path/to/agda"),
            ),
            Connection__Target.Error.SomethingWentWrong(
              Connection.URI.parse("path/to/als"),
              NotFound("path/to/als"),
            ),
          ])
          Assert.deepStrictEqual(actual, expected)
        } else {
          // let expected = Error([
          //   Connection__Target.Error.SomethingWentWrong(
          //     Connection.URI.parse("path\\to\\agda"),
          //     NotFound("path\\to\\agda"),
          //   ),
          //   Connection__Target.Error.SomethingWentWrong(
          //     Connection.URI.parse("path\\to\\als"),
          //     NotFound("path\\to\\als"),
          //   ),
          // ])
          switch actual {
          | Ok(_) => Assert.fail("expected an error, got Ok")
          | Error([
              Connection__Target.Error.SomethingWentWrong(_, _),
              Connection__Target.Error.SomethingWentWrong(_, _),
            ]) =>
            Assert.ok(true)
          | _ => Assert.fail("expected an error, got something else")
          }
        }
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

        // setup the memento
        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        if OS.onUnix {
          let expected = Error([
            Connection__Target.Error.SomethingWentWrong(
              Connection.URI.parse("path/to/agda"),
              NotFound("path/to/agda"),
            ),
            Connection__Target.Error.SomethingWentWrong(
              Connection.URI.parse("path/to/als"),
              NotFound("path/to/als"),
            ),
          ])
          Assert.deepStrictEqual(actual, expected)
        } else {
          // let expected = Error([
          //   Connection__Target.Error.SomethingWentWrong(
          //     Connection.URI.parse("path\\to\\agda"),
          //     NotFound("path\\to\\agda"),
          //   ),
          //   Connection__Target.Error.SomethingWentWrong(
          //     Connection.URI.parse("path\\to\\als"),
          //     NotFound("path\\to\\als"),
          //   ),
          // ])
          switch actual {
          | Ok(_) => Assert.fail("expected an error, got Ok")
          | Error([
              Connection__Target.Error.SomethingWentWrong(_, _),
              Connection__Target.Error.SomethingWentWrong(_, _),
            ]) =>
            Assert.ok(true)
          | _ => Assert.fail("expected an error, got something else")
          }
        }
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

        // setup the memento
        let memento = State__Memento.make(None)
        let paths =
          [
            "path/to/non-existent-agda",
            agdaMockPath.contents,
            "path/to/non-existent-als",
          ]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Ok(agdaMockTarget)

        Assert.deepStrictEqual(actual, expected)
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
        | Error(_) => failwith("expected to find `agda` or `als`")
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

  describe("`fromPathsAndCommands`", () => {
    Async.it(
      "should find commands when no paths are given",
      async () => {
        let memento = State__Memento.make(None)
        let paths = []
        let commands = ["agda", "als"]
        let result = await Connection.fromPathsAndCommands(memento, paths, commands)

        let expected = await getAgdaTarget()

        Assert.deepStrictEqual(result, Ok(expected))
      },
    )

    Async.it(
      "should find commands even if all paths given are wrong",
      async () => {
        let memento = State__Memento.make(None)
        let paths = [Connection__URI.parse("some/other/paths")]
        let commands = ["agda", "als"]
        let result = await Connection.fromPathsAndCommands(memento, paths, commands)

        let expected = await getAgdaTarget()

        Assert.deepStrictEqual(result, Ok(expected))
      },
    )

    Async.it(
      "should find the command with its path given",
      async () => {
        let agdaTarget = await getAgdaTarget()

        let memento = State__Memento.make(None)
        let paths = [
          Connection__Target.toURI(agdaTarget),
          Connection__URI.parse("some/other/paths"),
        ]
        let commands = ["non-existent-command", "agda", "als"]
        let result = await Connection.fromPathsAndCommands(memento, paths, commands)

        Assert.deepStrictEqual(result, Ok(agdaTarget))
      },
    )

    Async.it(
      "should throw an error when the command is not found",
      async () => {
        let memento = State__Memento.make(None)
        let paths = [Connection__URI.parse("some/other/paths")]
        let commands = ["non-existent-command"]
        let result = await Connection.fromPathsAndCommands(memento, paths, commands)

        if OS.onUnix {
          let expected = {
            Connection__Error.Aggregated.Attempts.targets: [
              {
                SomethingWentWrong(
                  Connection.URI.parse("some/other/paths"),
                  if OS.onUnix {
                    NotFound("some/other/paths")
                  } else {
                    NotFound("some\\other\\paths")
                  },
                )
              },
            ],
            commands: [Connection__Command.Error.NotFound("non-existent-command")],
          }

          Assert.deepStrictEqual(result, Error(expected))
        } else {
          // let expected = {
          //   Connection__Error.Aggregated.Attempts.targets: [
          //     {
          //       SomethingWentWrong(
          //         Connection.URI.parse("some/other/paths"),
          //         if OS.onUnix {
          //           NotFound("some/other/paths")
          //         } else {
          //           NotFound("some\\other\\paths")
          //         },
          //       )
          //     },
          //   ],
          //   commands: [Connection__Command.Error.NotFound("non-existent-command")],
          // }

          // Assert.deepStrictEqual(result, Error(expected))

          switch result {
          | Ok(_) => Assert.fail("expected an error, got Ok")
          | Error(error) =>
            switch error.targets {
            | [Connection__Target.Error.SomethingWentWrong(_, _)] => Assert.ok(true)
            | _ => Assert.fail("expected an error with a single target error")
            }
          }
        }
      },
    )
  })

  describe("`fromDownloads`", () => {
    let attempts = {
      Connection__Error.Aggregated.Attempts.targets: [],
      commands: [],
    }

    let agdaMockTarget = ref(None)

    Async.before(
      async () => {
        // setup the Agda mock
        let path = await Target.Agda.mock(~version="2.7.0.1", ~name="agda-mock")

        switch await Connection.Target.fromRawPath(path) {
        | Ok(target) => agdaMockTarget := Some(target)
        | Error(_) => failwith("Got error when trying to construct a mock for Agda ")
        }
      },
    )

    Async.after(
      async () => {
        await Config.Connection.setAgdaPaths([])
        await Connection.Target.setPicked(State__Memento.make(None), None)

        // cleanup the Agda mock
        switch agdaMockTarget.contents {
        | Some(target) =>
          Target.Agda.destroy(target)
          agdaMockTarget := None
        | None => ()
        }
      },
    )

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
        let alreadyDownloaded = async () => None // don't care
        let downloadLatestALS = async _ => Error(
          Connection__Download.Error.CannotFindCompatibleALSRelease,
        ) // don't care
        let result = await Connection.fromDownloads(
          attempts,
          Error(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )

        Assert.deepStrictEqual(result, Error(Aggregated(PlatformNotSupported(attempts, platform))))

        // should not ask the user for download policy
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
      },
    )

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
        let alreadyDownloaded = async () => None // don't care
        let downloadLatestALS = async _ => Error(
          Connection__Download.Error.CannotFindCompatibleALSRelease,
        ) // don't care
        let result = await Connection.fromDownloads(
          attempts,
          Error(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )

        Assert.deepStrictEqual(result, Error(Aggregated(PlatformNotSupported(attempts, platform))))

        // should not ask the user for download policy
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the initial download policy is `No`",
      async () => {
        let platform = Connection__Download__Platform.Windows

        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)
        let getDownloadPolicy = async () => {
          getDownloadPolicyCount := getDownloadPolicyCount.contents + 1
          Config.Connection.DownloadPolicy.No // don't care, shouldn't be invoked
        }
        let alreadyDownloaded = async () => None // don't care
        let downloadLatestALS = async _ => Error(
          Connection__Download.Error.CannotFindCompatibleALSRelease,
        ) // don't care
        let result = await Connection.fromDownloads(
          attempts,
          Ok(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )
        Assert.deepStrictEqual(result, Error(Aggregated(NoDownloadALS(attempts))))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should not ask the user for download policy
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
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
        let alreadyDownloaded = async () => None // don't care
        let downloadLatestALS = async _ => Error(
          Connection__Download.Error.CannotFindCompatibleALSRelease,
        ) // don't care
        let result = await Connection.fromDownloads(
          attempts,
          Ok(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )
        Assert.deepStrictEqual(result, Error(Aggregated(NoDownloadALS(attempts))))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should ask the user for download policy exactly once
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)
      },
    )

    Async.it(
      "should check if the latest ALS is already downloaded when the download policy is `Yes`",
      async () => {
        let platform = Connection__Download__Platform.Windows

        // access the Agda mock
        let target = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }
        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
        let checkedCache = ref(false)
        let alreadyDownloaded = async () => {
          checkedCache := true
          Some(target)
        }
        let downloadLatestALS = async _ => Error(
          Connection__Download.Error.CannotFindCompatibleALSRelease,
        ) // don't care

        let result = await Connection.fromDownloads(
          attempts,
          Ok(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(result, Ok(target))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        let paths = Config.Connection.getAgdaPaths()->Array.map(Connection.URI.toString)
        Assert.ok(
          paths->Util.Array.includes(Connection.URI.toString(Connection.Target.toURI(target))),
        )
      },
    )

    Async.it(
      "should proceed to download the latest ALS when the download policy is `Yes` and the cached latest ALS is not found",
      async () => {
        let platform = Connection__Download__Platform.Windows

        // access the Agda mock
        let target = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
        let checkedCache = ref(false)
        let alreadyDownloaded = async () => {
          checkedCache := true
          None
        }
        let checkedDownload = ref(false)
        let downloadLatestALS = async _ => {
          checkedDownload := true
          Ok(target)
        }

        let result = await Connection.fromDownloads(
          attempts,
          Ok(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedDownload.contents, true)
        Assert.deepStrictEqual(result, Ok(target))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        let paths = Config.Connection.getAgdaPaths()->Array.map(Connection.URI.toString)
        Assert.ok(
          paths->Util.Array.includes(Connection.URI.toString(Connection.Target.toURI(target))),
        )
      },
    )

    Async.it(
      "should throw the `DownloadALS` error when the download policy is `Yes` but the download fails",
      async () => {
        let platform = Connection__Download__Platform.Windows

        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
        let checkedCache = ref(false)
        let alreadyDownloaded = async () => {
          checkedCache := true
          None
        }
        let checkedDownload = ref(false)
        let downloadLatestALS = async _ => {
          checkedDownload := true
          Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        }

        let result = await Connection.fromDownloads(
          attempts,
          Ok(platform),
          getDownloadPolicy,
          alreadyDownloaded,
          downloadLatestALS,
        )
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedDownload.contents, true)
        Assert.deepStrictEqual(
          result,
          Error(
            Aggregated(
              DownloadALS(attempts, Connection__Download.Error.CannotFindCompatibleALSRelease),
            ),
          ),
        )

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
      },
    )
  })

  describe_only("checkForPrebuiltDataDirectory", () => {
    Async.it(
      "should return asset path when data directory exists",
      async () => {
        // Create a temporary directory structure
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let execPath = NodeJs.Path.join([tempDir, "bin", "agda-language-server"])
        let dataDir = NodeJs.Path.join([tempDir, "bin", "data"])

        // Create the directory structure
        await NodeJs.Fs.mkdir(NodeJs.Path.join([tempDir, "bin"]), {recursive: true, mode: 0o777})
        await NodeJs.Fs.mkdir(dataDir, {recursive: true, mode: 0o777})

        // Test the function
        let result = await Connection__Target.checkForPrebuiltDataDirectory(execPath)

        // Should return Some with asset path
        let expectedAssetPath = NodeJs.Path.join([execPath, "..", "data"])
        Assert.deepStrictEqual(result, Some(expectedAssetPath))

        // Cleanup
        NodeJs.Fs.rmdirSync(dataDir)
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "bin"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return None when data directory does not exist",
      async () => {
        // Create a temporary directory structure without data directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let execPath = NodeJs.Path.join([tempDir, "bin", "agda-language-server"])

        // Create only the bin directory, no data directory
        await NodeJs.Fs.mkdir(NodeJs.Path.join([tempDir, "bin"]), {recursive: true, mode: 0o777})

        // Test the function
        let result = await Connection__Target.checkForPrebuiltDataDirectory(execPath)

        // Should return None
        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "bin"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })
})
