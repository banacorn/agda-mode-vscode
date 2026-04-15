open Mocha
open Test__Util

// Config.Connection paths
// ├── Working config paths
// │   ├── Single working path → should not modify config
// │   ├── Multiple working paths → should not modify config
// │   └── Mixed working/broken paths → should not modify config (working paths exist)
// ├── Broken config paths
// │   └── All broken, auto discovery fails → should fail without modifying config
// ├── Empty config paths
// │   └── Auto discovery fails → should fail without modifying config
// └── UI-triggered additions
//     └── Switch version UI selection → should update PreferredCandidate only

describe("Config.Connection paths", () => {
  // FIXME: UI-related tests are flaky on CI
  This.retries(2)

  let userAgda = ref("")
  let systemAgda = ref("")
  let brokenAgda = ref("/broken/agda")
  let alternativeAgda = ref("")
  let downloadedALS = ref("")
  let logChannel = Chan.make()

  // setup the Agda mocks
  Async.before(async () => {
    userAgda := (await Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-mock-user"))
    systemAgda := (await Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-mock-system"))
    alternativeAgda := (await Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-mock-alt"))
    downloadedALS :=
      (
        await Candidate.ALS.mock(
          ~alsVersion="1.3.1",
          ~agdaVersion="2.6.3",
          ~name="als-make-mock",
        )
      )
  })

  // cleanup the Agda mocks
  Async.after(async () => {
    await Candidate.Agda.destroy(userAgda.contents)
    await Candidate.Agda.destroy(systemAgda.contents)
    await Candidate.Agda.destroy(alternativeAgda.contents)
    await Candidate.ALS.destroy(downloadedALS.contents)
  })

  beforeEach(() => {
    Registry__Connection.status := Empty
  })

  // Mock platform that returns the system Agda path when `findCommand` is called
  let platformWithDiscovery = (
    module(
      {
        include Desktop.Desktop
        let findCommand = (_command, ~timeout as _timeout=1000) => {
          Promise.resolve(Ok(systemAgda.contents))
        }
      }
    ): Platform.t
  )

  // Mock platform that fails discovery
  let platformNoDiscovery = (
    module(
      {
        include Desktop.Desktop
        let findCommand = (_command, ~timeout as _timeout=1000) => {
          Promise.resolve(Error(Connection__Command.Error.NotFound))
        }
      }
    ): Platform.t
  )

  // Helper function to create a connection with given config paths
  // also returns a Log listener to capture config changes
  let makeConnection = async (configPaths: array<string>, platform: Platform.t) => {
    let memento = Memento.make(None)
    // Set the initial config paths
    await Config.Connection.setAgdaPaths(logChannel, configPaths)
    // Start collecting logs only AFTER setting config paths, but BEFORE making the connection!
    let listener = Log.collect(logChannel)

    let result = await Connection.makeWithFallback(
      platform,
      memento,
      VSCode.Uri.file("/tmp/test"),
      configPaths,
      ["agda"], // command to discover
      logChannel,
    )

    let logs = listener(~filter=Log.isConfig)
    (logs, result)
  }

  describe("Working config paths", () => {
    Async.it(
      "should not modify config with single working path",
      async () => {
        let configPaths = [userAgda.contents]
        let (logs, result) = await makeConnection(configPaths, platformWithDiscovery)

        // should not modify config
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )

    Async.it(
      "should not modify config with multiple working paths",
      async () => {
        let configPaths = [userAgda.contents, alternativeAgda.contents]
        let (logs, result) = await makeConnection(configPaths, platformWithDiscovery)

        // should not modify config
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

        switch result {
        | Ok(connection) =>
          // should use last working path (reverse order per spec)
          Assert.deepStrictEqual(connection->Connection.getPath, alternativeAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )

    Async.it(
      "should not modify config with mixed working/broken paths",
      async () => {
        let configPaths = [brokenAgda.contents, userAgda.contents, "/another/broken"]
        let (logs, result) = await makeConnection(configPaths, platformWithDiscovery)

        // should not modify config since working paths exist
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

        switch result {
        | Ok(connection) =>
          // should use first working path (userAgda)
          Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )
  })

  describe("Bare command in config paths (#277)", () => {
    Async.it(
      "should resolve bare command via PATH lookup",
      async () => {
        let result = await Connection.fromPathsOrCommands(
          platformWithDiscovery,
          [("agda", Connection__Error.Establish.FromConfig)],
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, systemAgda.contents)
        | Error(_) =>
          Assert.fail("fromPathsOrCommands should succeed with bare command 'agda'")
        }
      },
    )

    Async.it(
      "should fail when bare command is not found in PATH",
      async () => {
        let result = await Connection.fromPathsOrCommands(
          platformNoDiscovery,
          [("agda", Connection__Error.Establish.FromConfig)],
        )

        switch result {
        | Ok(_) => Assert.fail("should fail when bare command is not in PATH")
        | Error(_) => ()
        }
      },
    )

    Async.it(
      "should still resolve absolute paths directly",
      async () => {
        let result = await Connection.fromPathsOrCommands(
          platformNoDiscovery,
          [(userAgda.contents, Connection__Error.Establish.FromConfig)],
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
        | Error(_) =>
          Assert.fail("absolute paths should work without PATH lookup")
        }
      },
    )
  })

  describe("Broken config paths", () => {
    Async.it(
      "should fail without modifying config when all config paths are broken and auto discovery fails",
      async () => {
        // Set download policy to No to prevent dialog prompts in test environment
        await Config.Connection.DownloadPolicy.set(No)

        let configPaths = [brokenAgda.contents, "/another/broken"]
        let (logs, result) = await makeConnection(configPaths, platformNoDiscovery)

        // should not modify config when all attempts fail
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

        switch result {
        | Ok(_) => Assert.fail("Connection should fail when all paths are broken and no discovery")
        | Error(_) => () // expected to fail
        }
      },
    )
  })

  describe("Empty config paths", () => {
    Async.it(
      "should fail without modifying config when config is empty and auto discovery fails",
      async () => {
        // Set download policy to No to prevent dialog prompts in test environment
        await Config.Connection.DownloadPolicy.set(No)

        let configPaths = []
        let (logs, result) = await makeConnection(configPaths, platformNoDiscovery)

        // should not modify config when discovery fails
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

        switch result {
        | Ok(_) => Assert.fail("Connection should fail when config is empty and no discovery")
        | Error(_) => () // expected to fail
        }
      },
    )
  })

  describe("Default value", () => {
    Async.it(
      "connection.paths default value should be [\"agda\", \"als\"] in package.json",
      async () => {
        let packageJsonPath = NodeJs.Path.join([
          NodeJs.Process.cwd(NodeJs.Process.process),
          "package.json",
        ])
        let content = NodeJs.Fs.readFileSync(packageJsonPath)->NodeJs.Buffer.toString
        let json = JSON.parseExn(content)

        // Navigate to contributes.configuration.properties["agdaMode.connection.paths"].default
        let default = switch json {
        | Object(root) =>
          root
          ->Dict.get("contributes")
          ->Option.flatMap(v =>
            switch v {
            | Object(contributes) => contributes->Dict.get("configuration")
            | _ => None
            }
          )
          ->Option.flatMap(v =>
            switch v {
            | Object(config) => config->Dict.get("properties")
            | _ => None
            }
          )
          ->Option.flatMap(v =>
            switch v {
            | Object(properties) => properties->Dict.get("agdaMode.connection.paths")
            | _ => None
            }
          )
          ->Option.flatMap(v =>
            switch v {
            | Object(pathConfig) => pathConfig->Dict.get("default")
            | _ => None
            }
          )
        | _ => None
        }

        Assert.deepStrictEqual(default, Some(JSON.Array([JSON.String("agda"), JSON.String("als")])))
      },
    )
  })

  describe("Duplicate candidates", () => {
    Async.it(
      "addAgdaPath should not add duplicate path",
      async () => {
        let initialPaths = ["/usr/bin/agda", "/opt/homebrew/bin/agda"]
        await Config.Connection.setAgdaPaths(logChannel, initialPaths)

        // Try to add a path that already exists
        await Config.Connection.addAgdaPath(logChannel, "/usr/bin/agda")

        Assert.deepStrictEqual(
          Config.Connection.getAgdaPaths(),
          ["/usr/bin/agda", "/opt/homebrew/bin/agda"],
        )
      },
    )

    Async.it(
      "addAgdaPath should not add semantically duplicate candidate",
      async () => {
        let initialPaths = ["/usr/bin/agda"]
        await Config.Connection.setAgdaPaths(logChannel, initialPaths)

        await Config.Connection.addAgdaPath(logChannel, "file:///usr/bin/agda")

        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["/usr/bin/agda"])
      },
    )

    Async.it(
      "setAgdaPaths should deduplicate paths",
      async () => {
        let pathsWithDuplicates = ["/usr/bin/agda", "/opt/homebrew/bin/agda", "/usr/bin/agda"]
        await Config.Connection.setAgdaPaths(logChannel, pathsWithDuplicates)

        // connection.paths MUST be an ordered list of unique Candidates
        Assert.deepStrictEqual(
          Config.Connection.getAgdaPaths(),
          ["/usr/bin/agda", "/opt/homebrew/bin/agda"],
        )
      },
    )

    Async.it(
      "setAgdaPaths should deduplicate semantically equal candidates while preserving first raw spelling",
      async () => {
        let pathsWithDuplicates = ["/usr/bin/agda", "file:///usr/bin/agda", "agda", "agda"]
        await Config.Connection.setAgdaPaths(logChannel, pathsWithDuplicates)

        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["/usr/bin/agda", "agda"])
      },
    )

    Async.it(
      "setAgdaPaths then getAgdaPaths should round-trip without reordering (production path)",
      async () => {
        // Temporarily disable testing mode to exercise the real config persistence path
        // (exercises the real config read/write cycle through VSCode settings)
        Config.inTestingMode := false

        let paths = ["/first", "/second", "/third"]
        await Config.Connection.setAgdaPaths(logChannel, paths)
        let retrieved = Config.Connection.getAgdaPaths()

        // Restore testing mode before assertions (in case assertion throws)
        Config.inTestingMode := true

        // Round-trip: set → get MUST return the same order
        Assert.deepStrictEqual(retrieved, paths)
      },
    )

    Async.it(
      "addAgdaPath should preserve existing order (production path)",
      async () => {
        // Temporarily disable testing mode to exercise the real config persistence path
        Config.inTestingMode := false

        let initialPaths = ["/usr/bin/agda", "/opt/homebrew/bin/agda"]
        await Config.Connection.setAgdaPaths(logChannel, initialPaths)

        // Add a new path via the real addAgdaPath
        await Config.Connection.addAgdaPath(logChannel, "/new/path/als")
        let retrieved = Config.Connection.getAgdaPaths()

        // Restore testing mode before assertions
        Config.inTestingMode := true

        // Existing paths MUST be in their original order, with new path appended
        Assert.deepStrictEqual(retrieved, ["/usr/bin/agda", "/opt/homebrew/bin/agda", "/new/path/als"])
      },
    )
  })

  describe("UI-triggered additions", () => {
    // Helper functions for UI-triggered tests
    let createTestState = () => {
      let channels = {
        State.inputMethod: Chan.make(),
        responseHandled: Chan.make(),
        commandHandled: Chan.make(),
        log: logChannel,
      }

      let mockEditor = %raw(`{
        document: { fileName: "test.agda" }
      }`)

      let mockStorageUri = VSCode.Uri.file(NodeJs.Os.tmpdir())
      let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))

      State.make(
        "test-id",
        platformWithDiscovery,
        channels,
        mockStorageUri,
        mockExtensionUri,
        Memento.make(None),
        mockEditor,
        None,
      )
    }

    // UI Test Builders - Simplified and reusable UI simulation functions
    module UITestBuilders = {
      // Common setup shared by all UI test builders
      let setupUITest = async (initialConfig: array<string>) => {
        await Config.Connection.setAgdaPaths(logChannel, initialConfig)
        let listener = Log.collect(logChannel)
        let mockState = createTestState()
        (listener, mockState)
      }

      // Common execution pattern shared by all UI test builders
      let executeUITest = async (mockState, platform, mockSelectedItem) => {
        let view = State__SwitchVersion.View.make(logChannel)
        let manager = State__SwitchVersion.SwitchVersionManager.make(mockState)

        // Promise that resolves when onSelection handler has completed all operations
        let onOperationComplete = Log.on(
          logChannel,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        // Execute the selection
        State__SwitchVersion.Handler.onSelection(
          mockState,
          platform,
          manager,
          ref([Connection__Download.Channel.DevALS]),
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [mockSelectedItem],
        )

        await onOperationComplete

        // Clean up
        view->State__SwitchVersion.View.destroy
      }

      // Simplified path selection simulation
      let simulatePathSelection = async (
        initialConfig: array<string>,
        selectedPath: string,
      ) => {
        let (listener, mockState) = await setupUITest(initialConfig)

        let mockSelectedItem = State__SwitchVersion.Item.fromItemData(
          State__SwitchVersion.ItemData.Candidate(
            selectedPath,
            selectedPath,
            {
              kind: Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
              timestamp: Date.make(),
              error: None,
            },
            false,
          ),
          mockState.extensionUri,
        )

        await executeUITest(mockState, platformWithDiscovery, mockSelectedItem)
        let logs = listener(~filter=Log.isConfig)
        let finalConfig = Config.Connection.getAgdaPaths()
        let preferredCandidate = Memento.PreferredCandidate.get(mockState.memento)
        (logs, finalConfig, preferredCandidate)
      }

      // Simplified download action simulation
      let simulateDownloadAction = async (
        initialConfig: array<string>,
        ~isAlreadyDownloaded: bool=false,
      ) => {
        let (listener, mockState) = await setupUITest(initialConfig)
        let releasesDir =
          VSCode.Uri.joinPath(mockState.globalStorageUri, ["releases"])
        let expectedDownloadedPath = if isAlreadyDownloaded {
          // Create a release-managed native artifact at the expected location
          // so that findReleaseManagedDownloadedForDesktopPlatform finds it.
          // The mock platform returns MacOS_Arm, so we use that artifact name.
          let artifactDir = NodeJs.Path.join([
            VSCode.Uri.fsPath(mockState.globalStorageUri),
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          let alsPath = NodeJs.Path.join([artifactDir, "als"])
          NodeJs.Fs.writeFileSync(alsPath, NodeJs.Buffer.fromString("mock-native"))
          alsPath
        } else {
          downloadedALS.contents
        }

        let mockSelectedItem = State__SwitchVersion.Item.fromItemData(
          State__SwitchVersion.ItemData.DownloadAction(
            isAlreadyDownloaded,
            "Agda v2.8.0 Language Server (dev build)",
            "native",
          ),
          mockState.extensionUri,
        )

        await executeUITest(
          mockState,
          Mock.Platform.makeWithSuccessfulDownload(downloadedALS.contents),
          mockSelectedItem,
        )
        let logs = listener(~filter=Log.isConfig)
        let finalConfig = Config.Connection.getAgdaPaths()
        let preferredCandidate = Memento.PreferredCandidate.get(mockState.memento)
        // Clean up any release-managed files created in the shared tmpdir
        let _ = await FS.deleteRecursive(releasesDir)
        (logs, finalConfig, preferredCandidate, expectedDownloadedPath)
      }

      // Simulate empty selection (user cancels)
      let simulateEmptySelection = async (initialConfig: array<string>) => {
        let (listener, mockState) = await setupUITest(initialConfig)

        let view = State__SwitchVersion.View.make(logChannel)
        let manager = State__SwitchVersion.SwitchVersionManager.make(mockState)

        let onOperationComplete = Log.on(
          logChannel,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        // Simulate empty selection (no items selected)
        State__SwitchVersion.Handler.onSelection(
          mockState,
          platformWithDiscovery,
          manager,
          ref([Connection__Download.Channel.DevALS]),
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [], // Empty selection
        )

        await onOperationComplete
        view->State__SwitchVersion.View.destroy

        let logs = listener(~filter=Log.isConfig)
        let finalConfig = Config.Connection.getAgdaPaths()
        let preferredCandidate = Memento.PreferredCandidate.get(mockState.memento)
        (logs, finalConfig, preferredCandidate)
      }
    }

    describe(
      "Switch version UI selection",
      () => {
        Async.it(
          "should update PreferredCandidate without modifying config when user selects existing path not in config",
          async () => {
            This.retries(2)

            let initialConfig = [userAgda.contents]
            let selectedPath = alternativeAgda.contents // not in initial config

            let (logs, finalConfig, preferredCandidate) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
            )

            // Should not modify config
            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
            // Should update PreferredCandidate
            Assert.deepStrictEqual(preferredCandidate, Some(selectedPath))
          },
        )

        Async.it(
          "should update PreferredCandidate without modifying config when user selects path already in config",
          async () => {
            let initialConfig = [userAgda.contents, alternativeAgda.contents]
            let selectedPath = userAgda.contents // already in config

            let (logs, finalConfig, preferredCandidate) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
            )

            // Should not modify config since path already exists
            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
            // Should update PreferredCandidate
            Assert.deepStrictEqual(preferredCandidate, Some(selectedPath))
          },
        )

        Async.it(
          "should update PreferredCandidate when user selects different path",
          async () => {
            let initialConfig = [userAgda.contents]
            let selectedPath = alternativeAgda.contents

            let (logs, finalConfig, preferredCandidate) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
            )

            // Should not modify config
            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
            // Should update PreferredCandidate
            Assert.deepStrictEqual(preferredCandidate, Some(selectedPath))
          },
        )

        Async.it(
          "should preserve bare agda command in PreferredCandidate without modifying config",
          async () => {
            let initialConfig = ["agda"]
            let selectedPath = "agda"

            let (logs, finalConfig, preferredCandidate) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
            )

            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
            Assert.deepStrictEqual(preferredCandidate, Some("agda"))
          },
        )

        Async.it(
          "should preserve bare als command in PreferredCandidate without modifying config",
          async () => {
            let initialConfig = ["als"]
            let selectedPath = "als"

            let (logs, finalConfig, preferredCandidate) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
            )

            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
            Assert.deepStrictEqual(preferredCandidate, Some("als"))
          },
        )
      },
    )

    describe(
      "Download action selection",
      () => {
        Async.it(
          "should add downloaded path to config when user downloads new ALS",
          async () => {
            let initialConfig = [userAgda.contents]

            let (logs, finalConfig, preferredCandidate, expectedDownloadedPath) = await UITestBuilders.simulateDownloadAction(
              initialConfig,
              ~isAlreadyDownloaded=false,
            )

            // Should add the downloaded ALS path to config
            let expectedConfig = Array.concat(initialConfig, [expectedDownloadedPath])
            Assert.deepStrictEqual(logs, [Log.Config(Changed(initialConfig, expectedConfig))])
            Assert.deepStrictEqual(finalConfig, expectedConfig)
            // Manual UI download must not modify PreferredCandidate
            Assert.deepStrictEqual(preferredCandidate, None)
          },
        )

        Async.it(
          "should add already downloaded path to config when user selects already downloaded ALS",
          async () => {
            let initialConfig = [userAgda.contents]

            let (logs, finalConfig, preferredCandidate, expectedDownloadedPath) = await UITestBuilders.simulateDownloadAction(
              initialConfig,
              ~isAlreadyDownloaded=true,
            )

            // Should add the already downloaded ALS path to config
            let expectedConfig = Array.concat(initialConfig, [expectedDownloadedPath])
            Assert.deepStrictEqual(logs, [Log.Config(Changed(initialConfig, expectedConfig))])
            Assert.deepStrictEqual(finalConfig, expectedConfig)
            // Manual UI download must not modify PreferredCandidate
            Assert.deepStrictEqual(preferredCandidate, None)
          },
        )
      },
    )

    describe(
      "Non-candidate selections",
      () => {
        Async.it(
          "should handle empty selection correctly",
          async () => {
            let initialConfig = [userAgda.contents]

            let (logs, finalConfig, preferredCandidate) = await UITestBuilders.simulateEmptySelection(
              initialConfig,
            )

            // Config should remain unchanged
            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
            Assert.deepStrictEqual(preferredCandidate, None)
          },
        )
      },
    )
  })
})
