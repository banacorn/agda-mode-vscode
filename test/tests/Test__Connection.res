open Mocha
open Test__Util

let getAgdaTarget = async () => {
  let platformDeps = Desktop.make()
  switch await Connection.findCommands(platformDeps, ["agda"]) {
  | Ok(target) => target
  | Error(_) => failwith("expected to find `agda`")
  }
}

describe("Connection", () => {
  This.timeout(10000)

  describe("Target", () => {
    let agdaMockPath = ref("")
    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        // setup the Agda mock
        agdaMockPath := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock"))

        switch await Connection.Endpoint.fromRawPath(agdaMockPath.contents) {
        | Ok(target) => agdaMockEndpoint := Some(target)
        | Error(error) =>
          let errorMessage = Connection__Endpoint.Error.toString(error)
          failwith(
            "Got error when trying to construct target from mock Agda path:\n" ++ errorMessage,
          )
        }
      },
    )

    Async.it(
      "should return the previously picked connection",
      async () => {
        // access the Agda mock
        let agdaMockEndpoint = switch agdaMockEndpoint.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the memento
        let memento = Memento.make(None)
        await Connection.Endpoint.setPicked(memento, Some(agdaMockEndpoint))

        let paths = [agdaMockPath.contents, "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Endpoint.getPicked(memento, paths)
        let expected = Ok(agdaMockEndpoint)

        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when there's no previously picked connection",
      async () => {
        // setup the memento
        let memento = Memento.make(None)
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let expected = Error(
          paths->Array.map(
            uri =>
              switch uri {
              | FileURI(_, vscodeUri) =>
                Connection__Endpoint.Error.SomethingWentWrong(
                  uri,
                  NotFound(vscodeUri->VSCode.Uri.fsPath),
                )
              | LspURI(_) => failwith("Expected FileURI variant")
              },
          ),
        )

        let actual = await Connection__Endpoint.getPicked(memento, paths)

        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when the previously picked connection is not in the supplied paths",
      async () => {
        // access the Agda mock
        let agdaMockEndpoint = switch agdaMockEndpoint.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the memento
        let memento = Memento.make(None)
        await Connection.Endpoint.setPicked(memento, Some(agdaMockEndpoint))
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Endpoint.getPicked(memento, paths)
        if OS.onUnix {
          let expected = Error([
            Connection__Endpoint.Error.SomethingWentWrong(
              paths[0]->Option.getExn,
              NotFound(NodeJs.Path.resolve(["path/to/agda"])),
            ),
            Connection__Endpoint.Error.SomethingWentWrong(
              paths[1]->Option.getExn,
              NotFound(NodeJs.Path.resolve(["path/to/als"])),
            ),
          ])
          Assert.deepStrictEqual(actual, expected)
        } else {
          // let expected = Error([
          //   Connection__Endpoint.Error.SomethingWentWrong(
          //     Connection.URI.parse("path\\to\\agda"),
          //     NotFound("path\\to\\agda"),
          //   ),
          //   Connection__Endpoint.Error.SomethingWentWrong(
          //     Connection.URI.parse("path\\to\\als"),
          //     NotFound("path\\to\\als"),
          //   ),
          // ])
          switch actual {
          | Ok(_) => Assert.fail("expected an error, got Ok")
          | Error([
              Connection__Endpoint.Error.SomethingWentWrong(_, _),
              Connection__Endpoint.Error.SomethingWentWrong(_, _),
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
        let agdaMockEndpoint = switch agdaMockEndpoint.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the memento
        let memento = Memento.make(None)
        let paths =
          [
            "path/to/non-existent-agda",
            agdaMockPath.contents,
            "path/to/non-existent-als",
          ]->Array.map(Connection__URI.parse)

        let actual = await Connection__Endpoint.getPicked(memento, paths)
        let expected = Ok(agdaMockEndpoint)

        Assert.deepStrictEqual(actual, expected)
      },
    )

    Async.after(
      async () => {
        // cleanup the Agda mock
        switch agdaMockEndpoint.contents {
        | Some(target) =>
          await Endpoint.Agda.destroy(target)
          agdaMockEndpoint := None
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
        let platformDeps = Desktop.make()
        switch await Connection.findCommands(platformDeps, commands) {
        | Ok(_) => ()
        | Error(_) => failwith("expected to find `agda` or `als`")
        }
      },
    )

    Async.it(
      "should return an error when the command is not found",
      async () => {
        let commands = ["non-existent-command"]
        let platformDeps = Desktop.make()
        switch await Connection.findCommands(platformDeps, commands) {
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
        let memento = Memento.make(None)
        let paths = []
        let commands = ["agda", "als"]
        let platformDeps = Desktop.make()
        let result = await Connection.fromPathsAndCommands(platformDeps, memento, paths, commands)

        let expected = await getAgdaTarget()

        Assert.deepStrictEqual(result, Ok(expected))
      },
    )

    Async.it(
      "should find commands even if all paths given are wrong",
      async () => {
        let memento = Memento.make(None)
        let paths = [Connection__URI.parse("some/other/paths")]
        let commands = ["agda", "als"]
        let platformDeps = Desktop.make()
        let result = await Connection.fromPathsAndCommands(platformDeps, memento, paths, commands)

        let expected = await getAgdaTarget()

        Assert.deepStrictEqual(result, Ok(expected))
      },
    )

    Async.it(
      "should find the command with its path given",
      async () => {
        let agdaTarget = await getAgdaTarget()

        let memento = Memento.make(None)
        let paths = [
          Connection__Endpoint.toURI(agdaTarget),
          Connection__URI.parse("some/other/paths"),
        ]
        let commands = ["non-existent-command", "agda", "als"]
        let platformDeps = Desktop.make()
        let result = await Connection.fromPathsAndCommands(platformDeps, memento, paths, commands)

        Assert.deepStrictEqual(result, Ok(agdaTarget))
      },
    )

    Async.it(
      "should throw an error when the command is not found",
      async () => {
        let memento = Memento.make(None)
        let paths = [Connection__URI.parse("some/other/paths")]
        let commands = ["non-existent-command"]
        let platformDeps = Desktop.make()
        let result = await Connection.fromPathsAndCommands(platformDeps, memento, paths, commands)

        if OS.onUnix {
          let expected = {
            Connection__Error.Aggregated.Attempts.endpoints: [
              {
                SomethingWentWrong(
                  paths[0]->Option.getExn,
                  if OS.onUnix {
                    NotFound(NodeJs.Path.resolve(["some/other/paths"]))
                  } else {
                    NotFound(NodeJs.Path.resolve(["some\\other\\paths"]))
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
            switch error.endpoints {
            | [Connection__Endpoint.Error.SomethingWentWrong(_, _)] => Assert.ok(true)
            | _ => Assert.fail("expected an error with a single endpoint error")
            }
          }
        }
      },
    )
  })

  describe("`fromDownloads`", () => {
    let attempts = {
      Connection__Error.Aggregated.Attempts.endpoints: [],
      commands: [],
    }

    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        try {
          // setup the Agda mock
          let path = await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock")

          switch await Connection.Endpoint.fromRawPath(path) {
          | Ok(target) => agdaMockEndpoint := Some(target)
          | Error(error) =>
            let errorMessage = Connection__Endpoint.Error.toString(error)
            failwith(
              "Got error when trying to construct target from mock Agda path:\n" ++ errorMessage,
            )
          }
        } catch {
        | Failure(msg) => failwith(msg) // Preserve detailed error from Test__Util.res
        | _ => failwith("Got error when trying to construct target from mock Agda: unknown error")
        }
      },
    )

    Async.after(
      async () => {
        await Config.Connection.setAgdaPaths([])
        await Connection.Endpoint.setPicked(Memento.make(None), None)

        // cleanup the Agda mock
        switch agdaMockEndpoint.contents {
        | Some(target) =>
          await Endpoint.Agda.destroy(target)
          agdaMockEndpoint := None
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

        // Create a mock platform that returns an unsupported platform error
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Error(platform))
          let findCommands = _commands =>
            Promise.resolve(Error([Connection__Command.Error.NotFound("mock")]))
          let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.No)
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          attempts,
        )

        Assert.deepStrictEqual(result, Error(Aggregated(PlatformNotSupported(attempts, platform))))
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the initial download policy is `No`",
      async () => {
        let platform = Connection__Download__Platform.Windows

        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that simulates successful platform determination but No download policy
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Ok(platform))
          let findCommands = _commands =>
            Promise.resolve(Error([Connection__Command.Error.NotFound("mock")]))
          let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () => {
            getDownloadPolicyCount := getDownloadPolicyCount.contents + 1
            Promise.resolve(Config.Connection.DownloadPolicy.No)
          }
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          attempts,
        )
        Assert.deepStrictEqual(result, Error(Aggregated(NoDownloadALS(attempts))))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should not ask the user for download policy since it was already set to No
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the user clicked `cancel` on the download dialog",
      async () => {
        let platform = Connection__Download__Platform.Windows

        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that asks user and gets Undecided response
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Ok(platform))
          let findCommands = _commands =>
            Promise.resolve(Error([Connection__Command.Error.NotFound("mock")]))
          let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () => {
            getDownloadPolicyCount := getDownloadPolicyCount.contents + 1
            Promise.resolve(Config.Connection.DownloadPolicy.Undecided)
          }
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          attempts,
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
        let target = switch agdaMockEndpoint.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        // Create a mock platform that returns cached ALS
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Ok(platform))
          let findCommands = _commands =>
            Promise.resolve(Error([Connection__Command.Error.NotFound("mock")]))
          let alreadyDownloaded = _globalStorageUri => () => {
            checkedCache := true
            Promise.resolve(Some(target))
          }
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          attempts,
        )
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(result, Ok(target))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        let paths = Config.Connection.getAgdaPaths()->Array.map(Connection.URI.toString)
        Assert.ok(
          paths->Util.Array.includes(Connection.URI.toString(Connection.Endpoint.toURI(target))),
        )
      },
    )

    Async.it(
      "should proceed to download the latest ALS when the download policy is `Yes` and the cached latest ALS is not found",
      async () => {
        let platform = Connection__Download__Platform.Windows

        // access the Agda mock
        let target = switch agdaMockEndpoint.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that downloads ALS
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Ok(platform))
          let findCommands = _commands =>
            Promise.resolve(Error([Connection__Command.Error.NotFound("mock")]))
          let alreadyDownloaded = _globalStorageUri => () => {
            checkedCache := true
            Promise.resolve(None)
          }
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform => {
            checkedDownload := true
            Promise.resolve(Ok(target))
          }
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          attempts,
        )
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedDownload.contents, true)
        Assert.deepStrictEqual(result, Ok(target))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        let paths = Config.Connection.getAgdaPaths()->Array.map(Connection.URI.toString)
        Assert.ok(
          paths->Util.Array.includes(Connection.URI.toString(Connection.Endpoint.toURI(target))),
        )
      },
    )

    Async.it(
      "should throw the `DownloadALS` error when the download policy is `Yes` but the download fails",
      async () => {
        let platform = Connection__Download__Platform.Windows

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that fails to download ALS
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Ok(platform))
          let findCommands = _commands =>
            Promise.resolve(Error([Connection__Command.Error.NotFound("mock")]))
          let alreadyDownloaded = _globalStorageUri => () => {
            checkedCache := true
            Promise.resolve(None)
          }
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform => {
            checkedDownload := true
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          }
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          attempts,
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

  describe("checkForPrebuiltDataDirectory", () => {
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
        let result = await Connection__Endpoint.checkForPrebuiltDataDirectory(execPath)

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
        let result = await Connection__Endpoint.checkForPrebuiltDataDirectory(execPath)

        // Should return None
        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "bin"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })
})
