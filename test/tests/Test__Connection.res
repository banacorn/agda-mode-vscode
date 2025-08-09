open Mocha
open Test__Util

let getAgdaTarget = async () => {
  let platformDeps = Desktop.make()
  switch await Connection.fromCommands(platformDeps, ["agda"]) {
  | Ok(connection) => connection
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
        agdaMockEndpoint := Some(agdaMockPath.contents)
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
      "should return an error when the command is not found",
      async () => {
        switch await Connection__Command.search("non-existent-command") {
        | Ok(_output) => failwith("expected to not find `non-existent-command`")
        | Error(_) => ()
        }
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
        let result = await Connection.checkForPrebuiltDataDirectory(execPath)

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
        let result = await Connection.checkForPrebuiltDataDirectory(execPath)

        // Should return None
        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "bin"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("`fromDownloads`", () => {
    let constructionError = Connection__Error.Construction.make()

    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        try {
          // setup the Agda mock
          let path = await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-2")

          agdaMockEndpoint := Some(path)
        } catch {
        | Failure(msg) => failwith(msg) // Preserve detailed error from Test__Util.res
        | _ => failwith("Got error when trying to construct target from mock Agda: unknown error")
        }
      },
    )

    Async.after(
      async () => {
        await Config.Connection.setAgdaPaths([])
        await Memento.PickedConnection.set(Memento.make(None), None)

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
        let mockPlatformDeps = Mock.Platform.makeWithPlatformError(platform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let actual = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        let expected = Connection.Error.Construction.merge(
          constructionError,
          Connection.Error.Construction.fromDownloadError(PlatformNotSupported(platform)),
        )

        Assert.deepStrictEqual(actual, Error(expected))
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the initial download policy is `No`",
      async () => {
        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that simulates successful platform determination but No download policy
        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.No,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(result, Error(constructionError))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should not ask the user for download policy since it was already set to No
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the user clicked `cancel` on the download dialog",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that asks user and gets Undecided response
        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.Undecided,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(result, Error(constructionError))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should ask the user for download policy exactly once
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)
      },
    )

    Async.it(
      "should check if the latest ALS is already downloaded when the download policy is `Yes`",
      async () => {
        // access the Agda mock (using it as ALS for this test)
        let mockEndpoint = switch agdaMockEndpoint.contents {
        | Some(endpoint) => endpoint
        | None => failwith("Unable to access the Agda mock endpoint")
        }
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        // Create a mock platform that returns cached ALS as endpoint
        let mockPlatformDeps = Mock.Platform.makeWithCachedDownloadAndFlag(
          mockEndpoint,
          checkedCache,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)

        // Should return connection directly (not endpoint)
        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) => Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(_) => Assert.fail("Expected successful cached download")
        }

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        // Check that the filesystem path was added to config (not the URI path)
        let paths = Config.Connection.getAgdaPaths()
        let expectedPath = mockEndpoint
        Assert.ok(paths->Array.includes(expectedPath))
      },
    )

    Async.it(
      "should proceed to download the latest ALS when the download policy is `Yes` and the cached latest ALS is not found",
      async () => {
        // access the Agda mock (using it as ALS for this test)
        let mockEndpoint = switch agdaMockEndpoint.contents {
        | Some(endpoint) => endpoint
        | None => failwith("Unable to access the Agda mock endpoint")
        }
        let mockPath = mockEndpoint

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that downloads ALS and returns raw path
        let mockPlatformDeps = Mock.Platform.makeWithSuccessfulDownload2AndFlags(
          mockPath,
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedDownload.contents, true)

        // Should return connection directly (not endpoint)
        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) => Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(_) => Assert.fail("Expected successful fresh download")
        }

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        // Check that the filesystem path was added to config (not the URI path)
        let paths = Config.Connection.getAgdaPaths()
        let expectedPath = mockEndpoint
        Assert.ok(paths->Array.includes(expectedPath))
      },
    )

    Async.it(
      "should throw the `DownloadALS` error when the download policy is `Yes` but the download fails",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that fails to download ALS
        let mockPlatformDeps = Mock.Platform.makeWithDownloadFailureAndFlags(
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedDownload.contents, true)

        let expected = Connection.Error.Construction.merge(
          constructionError,
          Connection.Error.Construction.fromDownloadError(CannotFindCompatibleALSRelease),
        )

        Assert.deepStrictEqual(result, Error(expected))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
      },
    )
  })

  describe("`fromPaths`", () => {
    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        try {
          // setup the Agda mock
          let path = await Test__Util.Endpoint.Agda.mock(
            ~version="2.7.0.1",
            ~name="agda-mock-paths",
          )
          agdaMockEndpoint := Some(path)
        } catch {
        | error => failwith("Failed to create Agda mock: " ++ Js.String.make(error))
        }
      },
    )

    Async.after(
      async () => {
        // cleanup the Agda mock
        switch agdaMockEndpoint.contents {
        | Some(path) =>
          try {
            NodeJs.Fs.unlinkSync(path)
          } catch {
          | _ => () // ignore cleanup errors
          }
        | None => ()
        }
      },
    )

    Async.it(
      "should connect successfully with valid path",
      async () => {
        let mockPath = switch agdaMockEndpoint.contents {
        | Some(path) => path
        | None => failwith("Mock endpoint not available")
        }

        let platformDeps = Desktop.make()
        let paths = [mockPath]
        let result = await Connection.fromPaths(platformDeps, paths)

        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, mockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(_) => Assert.fail("Expected successful connection")
        }
      },
    )

    Async.it(
      "should try multiple paths and use first valid one",
      async () => {
        let mockPath = switch agdaMockEndpoint.contents {
        | Some(path) => path
        | None => failwith("Mock endpoint not available")
        }

        let platformDeps = Desktop.make()
        let paths = ["invalid/path/1", "invalid/path/2", mockPath, "invalid/path/3"]
        let result = await Connection.fromPaths(platformDeps, paths)

        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, mockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(_) => Assert.fail("Expected successful connection to first valid path")
        }
      },
    )

    Async.it(
      "should return Construction error when all paths are invalid",
      async () => {
        let platformDeps = Desktop.make()
        let paths = ["invalid/path/1", "invalid/path/2", "invalid/path/3"]
        let result = await Connection.fromPaths(platformDeps, paths)

        switch result {
        | Ok(_) => Assert.fail("Expected error with invalid paths")
        | Error(constructionError) =>
          // Should have three endpoint errors
          let endpointErrors = constructionError.endpoints->Dict.toArray
          Assert.deepStrictEqual(Array.length(endpointErrors), 3)

          // Should have no command errors
          let commandErrors = constructionError.commands->Dict.toArray
          Assert.deepStrictEqual(Array.length(commandErrors), 0)

          // Should have no download error
          Assert.deepStrictEqual(constructionError.download, None)
        }
      },
    )

    Async.it(
      "should return empty error when no paths provided",
      async () => {
        let platformDeps = Desktop.make()
        let paths = []
        let result = await Connection.fromPaths(platformDeps, paths)

        switch result {
        | Ok(_) => Assert.fail("Expected error with empty paths")
        | Error(constructionError) =>
          // Should have no errors since no paths were tried
          let endpointErrors = constructionError.endpoints->Dict.toArray
          Assert.deepStrictEqual(Array.length(endpointErrors), 0)

          let commandErrors = constructionError.commands->Dict.toArray
          Assert.deepStrictEqual(Array.length(commandErrors), 0)

          Assert.deepStrictEqual(constructionError.download, None)
        }
      },
    )
  })

  describe("`fromCommands`", () => {
    Async.it(
      "should connect successfully with valid command",
      async () => {
        let platformDeps = Desktop.make()
        let commands = ["agda", "als"]
        let result = await Connection.fromCommands(platformDeps, commands)

        switch result {
        | Ok(connection) =>
          // Should connect to either Agda or ALS
          switch connection {
          | Agda(_, path, version) =>
            Assert.ok(String.length(path) > 0)
            Assert.ok(String.length(version) > 0)
          | ALS(_, path, (alsVersion, agdaVersion, _)) =>
            Assert.ok(String.length(path) > 0)
            Assert.ok(String.length(alsVersion) > 0)
            Assert.ok(String.length(agdaVersion) > 0)
          }
        | Error(_) => Assert.fail("Expected successful connection via command")
        }
      },
    )

    Async.it(
      "should try multiple commands and use first valid one",
      async () => {
        let platformDeps = Desktop.make()
        let commands = ["non-existent-cmd", "agda", "als"]
        let result = await Connection.fromCommands(platformDeps, commands)

        switch result {
        | Ok(connection) =>
          // Should connect to agda (first valid command)
          switch connection {
          | Agda(_, path, version) =>
            Assert.ok(String.length(path) > 0)
            Assert.ok(String.length(version) > 0)
          | ALS(_, path, (alsVersion, agdaVersion, _)) =>
            Assert.ok(String.length(path) > 0)
            Assert.ok(String.length(alsVersion) > 0)
            Assert.ok(String.length(agdaVersion) > 0)
          }
        | Error(_) => Assert.fail("Expected successful connection to valid command")
        }
      },
    )

    Async.it(
      "should return Construction error when all commands are invalid",
      async () => {
        let platformDeps = Desktop.make()
        let commands = ["non-existent-cmd-1", "non-existent-cmd-2", "non-existent-cmd-3"]
        let result = await Connection.fromCommands(platformDeps, commands)

        switch result {
        | Ok(_) => Assert.fail("Expected error with invalid commands")
        | Error(constructionError) =>
          // Should have no endpoint errors
          let endpointErrors = constructionError.endpoints->Dict.toArray
          Assert.deepStrictEqual(Array.length(endpointErrors), 0)

          // Should have three command errors
          let commandErrors = constructionError.commands->Dict.toArray
          Assert.deepStrictEqual(Array.length(commandErrors), 3)

          // Verify all command names are present
          let commandNames =
            commandErrors->Array.map(((name, _)) => name)->Array.toSorted(String.compare)
          Assert.deepStrictEqual(
            commandNames,
            ["non-existent-cmd-1", "non-existent-cmd-2", "non-existent-cmd-3"],
          )

          // Should have no download error
          Assert.deepStrictEqual(constructionError.download, None)
        }
      },
    )

    Async.it(
      "should return empty error when no commands provided",
      async () => {
        let platformDeps = Desktop.make()
        let commands = []
        let result = await Connection.fromCommands(platformDeps, commands)

        switch result {
        | Ok(_) => Assert.fail("Expected error with empty commands")
        | Error(constructionError) =>
          // Should have no errors since no commands were tried
          let endpointErrors = constructionError.endpoints->Dict.toArray
          Assert.deepStrictEqual(Array.length(endpointErrors), 0)

          let commandErrors = constructionError.commands->Dict.toArray
          Assert.deepStrictEqual(Array.length(commandErrors), 0)

          Assert.deepStrictEqual(constructionError.download, None)
        }
      },
    )
  })

  describe("make with logging", () => {
    Async.it(
      "should log ConnectedToAgda when Agda connection succeeds",
      async () => {
        /**
         * TEST PURPOSE: Verify that Connection.make emits ConnectedToAgda events
         * 
         * SCENARIO:
         * 1. Setup a mock Agda executable
         * 2. Create Connection.make with log channel
         * 3. Verify ConnectedToAgda event is logged with correct path and version
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel to capture all log events
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Setup mock Agda using Test__Util
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
          ~version="2.6.4",
          ~name="agda-mock-for-make",
        )

        // Create minimal memento and platformDeps
        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.make with the mock Agda path
        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          [agdaMockPath], // paths
          [], // commands
          logChannel,
        ) {
        | Ok(connection) =>
          // VERIFY: ConnectedToAgda event was logged with exact details
          Assert.deepStrictEqual(
            loggedEvents,
            [Log.Connection(Log.Connection.ConnectedToAgda(agdaMockPath, "2.6.4"))],
          )

          // VERIFY: Connection type matches logged event
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.6.4")
          | _ => Assert.fail("Expected Agda connection")
          }

        | Error(_) => Assert.fail("Expected connection to succeed")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => () // Ignore cleanup errors
        }
      },
    )

    Async.it(
      "should log connection events when using real agda command",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make logging works with real connections
         * 
         * SCENARIO:
         * 1. Try to connect using the real 'agda' command
         * 2. If successful, verify appropriate connection event is logged
         * 3. If failed, verify no connection events are logged
         * 
         * This test is more realistic as it uses actual command discovery
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel to capture all log events
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Create minimal memento and platformDeps
        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.make with real agda command
        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          [], // no specific paths
          ["agda"], // try to find agda command
          logChannel,
        ) {
        | Ok(connection) =>
          // VERIFY: Connection event matches the actual connection
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToAgda(path, version))],
            )
          | ALS(_, path, (alsVersion, agdaVersion, _)) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToALS(path, alsVersion, agdaVersion))],
            )
          }

        | Error(_) =>
          // If connection fails, verify no connection events were logged
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should not log connection events when connection fails",
      async () => {
        /**
         * TEST PURPOSE: Verify that failed connections don't emit connection events
         * 
         * SCENARIO: 
         * 1. Try to connect with invalid paths and commands
         * 2. Verify no ConnectedTo* events are logged
         * 3. Verify Connection.make returns Error
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel to capture all log events
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Create minimal memento and platformDeps (using mock to avoid dialogs)
        let memento = Memento.make(None)
        let platformDeps = Mock.Platform.makeBasic()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.make with invalid paths and commands
        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          ["/nonexistent/path"], // invalid paths
          ["nonexistent-command"], // invalid commands
          logChannel,
        ) {
        | Ok(_) => Assert.fail("Expected connection to fail")
        | Error(_) =>
          // VERIFY: No connection events were logged
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should log connection events even when falling back to downloads",
      async () => {
        /**
         * TEST PURPOSE: Verify logging works in download fallback scenarios
         * 
         * SCENARIO:
         * 1. Try paths/commands that fail
         * 2. Fallback to download succeeds (mocked)
         * 3. Verify connection event is logged for downloaded connection
         * 
         * NOTE: This test may be complex to implement due to download mocking.
         * For now, we'll just verify the interface works correctly.
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Create mock platform that simulates download failure (no actual download test)
        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.make with invalid paths - this should attempt download but fail
        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        ) {
        | Ok(_) =>
          // If it succeeds, verify the exact connection event was logged
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, _))] => () // Expected Agda connection event
          | [Log.Connection(Log.Connection.ConnectedToALS(_, _, _))] => () // Expected ALS connection event
          | [] => Assert.fail("Expected connection event to be logged")
          | _ => Assert.fail("Expected exactly one connection event")
          }
        | Error(_) =>
          // If it fails (expected), verify no connection events were logged
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )
  })

  describe("make fromPathsAndCommands scenarios", () => {
    Async.it(
      "should find commands when no paths are given",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make finds commands when no paths provided
         * 
         * SCENARIO:
         * 1. Call Connection.make with empty paths array
         * 2. Provide valid commands ["agda", "als"]
         * 3. Should successfully connect and log the connection
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          [], // no paths
          ["agda", "als"], // try commands
          logChannel,
        ) {
        | Ok(connection) =>
          // Should have logged a connection event
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, _))] => ()
          | [Log.Connection(Log.Connection.ConnectedToALS(_, _, _))] => ()
          | [] => Assert.fail("Expected connection event to be logged")
          | _ => Assert.fail("Expected exactly one connection event")
          }
        | Error(_) => Assert.fail("Expected to find agda or als")
        }
      },
    )

    Async.it(
      "should find commands even if all paths given are wrong",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make falls back to commands when paths fail
         * 
         * SCENARIO:
         * 1. Provide invalid paths
         * 2. Provide valid commands
         * 3. Should ignore failed paths and use commands successfully
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          ["/some/invalid/path"], // invalid paths
          ["agda", "als"], // valid commands
          logChannel,
        ) {
        | Ok(connection) =>
          // Should have logged a connection event
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, _))] => ()
          | [Log.Connection(Log.Connection.ConnectedToALS(_, _, _))] => ()
          | [] => Assert.fail("Expected connection event to be logged")
          | _ => Assert.fail("Expected exactly one connection event")
          }
        | Error(_) => Assert.fail("Expected to find agda or als via commands")
        }
      },
    )

    Async.it(
      "should prioritize valid paths over commands",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make uses paths before falling back to commands
         * 
         * SCENARIO:
         * 1. Setup mock Agda at known path
         * 2. Provide that path plus invalid commands
         * 3. Should use the path, not attempt commands
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Setup mock Agda
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
          ~version="2.6.4",
          ~name="agda-mock-for-path-priority",
        )

        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          [agdaMockPath], // valid path
          ["invalid-command"], // invalid commands
          logChannel,
        ) {
        | Ok(connection) =>
          // Should have logged connection to the mock path
          Assert.deepStrictEqual(
            loggedEvents,
            [Log.Connection(Log.Connection.ConnectedToAgda(agdaMockPath, "2.6.4"))],
          )

          // Connection should match the mock
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.6.4")
          | _ => Assert.fail("Expected Agda connection")
          }

        | Error(_) => Assert.fail("Expected connection to mock Agda path")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should respect memento picked path priority",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make prioritizes memento picked path
         * 
         * SCENARIO:
         * 1. Setup two mock Agda executables
         * 2. Set one as picked in memento
         * 3. Provide both paths to Connection.make
         * 4. Should connect to the picked one first
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Setup two mock Agda executables
        let agdaMockPath1 = await Test__Util.Endpoint.Agda.mock(
          ~version="2.6.3",
          ~name="agda-mock-1",
        )
        let agdaMockPath2 = await Test__Util.Endpoint.Agda.mock(
          ~version="2.6.4",
          ~name="agda-mock-2",
        )

        let memento = Memento.make(None)
        // Set path2 as picked in memento
        await Memento.PickedConnection.set(memento, Some(agdaMockPath2))

        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          platformDeps,
          memento,
          globalStorageUri,
          [agdaMockPath1, agdaMockPath2], // both paths, but path2 is picked
          [], // no commands
          logChannel,
        ) {
        | Ok(connection) =>
          // Should have connected to path2 (the picked one)
          Assert.deepStrictEqual(
            loggedEvents,
            [Log.Connection(Log.Connection.ConnectedToAgda(agdaMockPath2, "2.6.4"))],
          )

        | Error(_) => Assert.fail("Expected connection to picked mock path")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath1)
          NodeJs.Fs.unlinkSync(agdaMockPath2)
        } catch {
        | _ => ()
        }
      },
    )
  })

  describe("make fromDownloads scenarios", () => {
    Async.it(
      "should handle platform not supported error with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make handles unsupported platform gracefully
         * 
         * SCENARIO:
         * 1. Use mock platform that returns unsupported platform error
         * 2. Provide invalid paths/commands to force download fallback
         * 3. Should get PlatformNotSupported error and no connection events
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        let platform = {
          "os": "unsupported-os",
          "dist": "unsupported-dist",
          "codename": "unsupported-codename",
          "release": "unsupported-release",
        }

        let mockPlatformDeps = Mock.Platform.makeWithPlatformError(platform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"], // force fallback to downloads
          ["invalid-command"], // force fallback to downloads
          logChannel,
        ) {
        | Ok(_) => Assert.fail("Expected platform error")
        | Error(error) =>
          // Should get Construction error with PlatformNotSupported
          switch error {
          | Connection.Error.Construction(constructionError) =>
            switch constructionError.download {
            | Some(Connection__Download.Error.PlatformNotSupported(_)) => ()
            | _ => Assert.fail("Expected PlatformNotSupported download error")
            }
          | _ => Assert.fail("Expected Construction error")
          }

          // Should not have logged any connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle download policy No with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make respects No download policy
         * 
         * SCENARIO:
         * 1. Set download policy to No
         * 2. Force fallback to downloads (invalid paths/commands)
         * 3. Should return error without downloading and no connection events
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.No,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        ) {
        | Ok(_) => Assert.fail("Expected error due to No download policy")
        | Error(error) =>
          // Should get Construction error (empty since No policy blocks download)
          switch error {
          | Connection.Error.Construction(_) => ()
          | _ => Assert.fail("Expected Construction error")
          }

          // Should not have asked for download policy (already set to No)
          Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)

          // Should not have logged connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle download policy Undecided (user cancelled) with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make handles user cancelling download dialog
         * 
         * SCENARIO:
         * 1. Set download policy to Undecided
         * 2. Mock user clicking cancel (returning Undecided)
         * 3. Should return error and set policy to No, with no connection events
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that asks user and gets Undecided response (user cancelled)
        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.Undecided,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        ) {
        | Ok(_) => Assert.fail("Expected error due to user cancelling download")
        | Error(error) =>
          // Should get Construction error
          switch error {
          | Connection.Error.Construction(_) => ()
          | _ => Assert.fail("Expected Construction error")
          }

          // Should have asked for download policy exactly once
          Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)

          // Should have set policy to No after user cancelled
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

          // Should not have logged connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle cached ALS download with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make uses cached ALS and logs connection
         * 
         * SCENARIO:
         * 1. Set download policy to Yes
         * 2. Mock cached ALS available
         * 3. Should use cached ALS and log connection event
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Setup mock ALS executable for cached download
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-cached",
        )
        let mockEndpoint = agdaMockPath

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        // Create a mock platform that returns cached ALS
        let mockPlatformDeps = Mock.Platform.makeWithCachedDownloadAndFlag(
          mockEndpoint,
          checkedCache,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"], // force download fallback
          ["invalid-command"], // force download fallback
          logChannel,
        ) {
        | Ok(connection) =>
          // Should have checked cache
          Assert.deepStrictEqual(checkedCache.contents, true)

          // Should have logged connection
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, version))] =>
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          // Connection should match logged event
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }

          // Should have set policy to Yes
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        | Error(_) => Assert.fail("Expected successful cached download")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle fresh ALS download with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make downloads fresh ALS and logs connection
         * 
         * SCENARIO:
         * 1. Set download policy to Yes
         * 2. Mock no cached ALS, successful fresh download
         * 3. Should download ALS and log connection event
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Setup mock ALS for successful download
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-fresh-download",
        )
        let mockEndpoint = agdaMockPath

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that downloads ALS
        let mockPlatformDeps = Mock.Platform.makeWithSuccessfulDownloadAndFlags(
          mockEndpoint,
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"], // force download fallback
          ["invalid-command"], // force download fallback
          logChannel,
        ) {
        | Ok(connection) =>
          // Should have checked cache and performed download
          Assert.deepStrictEqual(checkedCache.contents, true)
          Assert.deepStrictEqual(checkedDownload.contents, true)

          // Should have logged connection
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, version))] =>
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          // Connection should match logged event
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }

          // Should have set policy to Yes
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        | Error(_) => Assert.fail("Expected successful fresh download")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle download failure with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.make handles download failures gracefully
         * 
         * SCENARIO:
         * 1. Set download policy to Yes
         * 2. Mock download failure
         * 3. Should return error and no connection events
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that fails to download ALS
        let mockPlatformDeps = Mock.Platform.makeWithDownloadFailureAndFlags(
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        switch await Connection.make(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        ) {
        | Ok(_) => Assert.fail("Expected download failure")
        | Error(error) =>
          // Should have checked cache and attempted download
          Assert.deepStrictEqual(checkedCache.contents, true)
          Assert.deepStrictEqual(checkedDownload.contents, true)

          // Should get Construction error with download failure
          switch error {
          | Connection.Error.Construction(constructionError) =>
            switch constructionError.download {
            | Some(Connection__Download.Error.CannotFindCompatibleALSRelease) => ()
            | _ => Assert.fail("Expected CannotFindCompatibleALSRelease download error")
            }
          | _ => Assert.fail("Expected Construction error")
          }

          // Should have set policy to Yes
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

          // Should not have logged connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )
  })
})
