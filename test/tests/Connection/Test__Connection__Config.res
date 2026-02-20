open Mocha
open Test__Util

// Config.Connection paths
// ├── Working config paths
// │   ├── Single working path → should not modify config
// │   ├── Multiple working paths → should not modify config
// │   └── Mixed working/broken paths → should not modify config (working paths exist)
// ├── Broken config paths
// │   ├── All broken, auto discovery succeeds → should not add discovered path to config
// │   └── All broken, auto discovery fails → should fail without modifying config
// ├── Empty config paths
// │   ├── Auto discovery succeeds → should not add discovered path to config
// │   └── Auto discovery fails → should fail without modifying config
// └── UI-triggered additions
//     └── Switch version UI selection → should add selected path to config

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
    userAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-user"))
    systemAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-system"))
    alternativeAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-alt"))
    downloadedALS :=
      (
        await Endpoint.ALS.mock(
          ~alsVersion="1.3.1",
          ~agdaVersion="2.6.3",
          ~name="als-make-mock",
        )
      )
  })

  // cleanup the Agda mocks
  Async.after(async () => {
    await Endpoint.Agda.destroy(userAgda.contents)
    await Endpoint.Agda.destroy(systemAgda.contents)
    await Endpoint.Agda.destroy(alternativeAgda.contents)
    await Endpoint.ALS.destroy(downloadedALS.contents)
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
          // should use first working path
          Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
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
      "should not add discovered path when all config paths are broken and auto discovery succeeds",
      async () => {
        let configPaths = [brokenAgda.contents, "/another/broken"]
        let (logs, result) = await makeConnection(configPaths, platformWithDiscovery)

        // should add discovered path to config
        let expectedConfig = configPaths
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), expectedConfig)

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, systemAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )

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
      "should not add discovered path when config is empty and auto discovery succeeds",
      async () => {
        let configPaths = []
        let (logs, result) = await makeConnection(configPaths, platformWithDiscovery)

        // should add discovered path to config
        let expectedConfig = []
        Assert.deepStrictEqual(logs, [])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), expectedConfig)

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, systemAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )

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
        ~description: string="",
      ) => {
        let (listener, mockState) = await setupUITest(initialConfig)

        // Set up the path in memento endpoints so it can be selected
        let discoveredEndpoints = Dict.make()
        discoveredEndpoints->Dict.set(selectedPath, Memento.Endpoints.Agda(Some("2.7.0.1")))
        await Memento.Endpoints.syncWithPaths(mockState.memento, discoveredEndpoints)

        // Create mock QuickPickItem representing the path selection
        let mockSelectedItem: VSCode.QuickPickItem.t = {
          label: "Agda v2.7.0.1",
          description,
          detail: selectedPath,
        }

        await executeUITest(mockState, platformWithDiscovery, mockSelectedItem)
        let logs = listener(~filter=Log.isConfig)
        let finalConfig = Config.Connection.getAgdaPaths()
        (logs, finalConfig)
      }

      // Simplified download action simulation
      let simulateDownloadAction = async (
        initialConfig: array<string>,
        ~isAlreadyDownloaded: bool=false,
      ) => {
        let (listener, mockState) = await setupUITest(initialConfig)

        // Create mock QuickPickItem representing download action
        let mockSelectedItem: VSCode.QuickPickItem.t = {
          label: "$(cloud-download)  Download the latest Agda Language Server",
          description: isAlreadyDownloaded ? "Downloaded and installed" : "",
          detail: "ALS v0.2.10, Agda v2.7.0.1",
        }

        await executeUITest(
          mockState,
          Mock.Platform.makeWithSuccessfulDownload(downloadedALS.contents),
          mockSelectedItem,
        )
        let logs = listener(~filter=Log.isConfig)
        let finalConfig = Config.Connection.getAgdaPaths()
        (logs, finalConfig)
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
          _downloadInfo => Promise.resolve(),
          view,
          [], // Empty selection
        )

        await onOperationComplete
        view->State__SwitchVersion.View.destroy

        let logs = listener(~filter=Log.isConfig)
        let finalConfig = Config.Connection.getAgdaPaths()
        (logs, finalConfig)
      }
    }

    describe(
      "Switch version UI selection",
      () => {
        Async.it(
          "should add path to config when user selects existing path not in config",
          async () => {
            This.retries(2)

            let initialConfig = [userAgda.contents]
            let selectedPath = alternativeAgda.contents // not in initial config

            let (logs, finalConfig) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
              ~description="",
            )

            // Should add the selected path to config
            let expectedConfig = Array.concat(initialConfig, [selectedPath])
            Assert.deepStrictEqual(logs, [Log.Config(Changed(initialConfig, expectedConfig))])
            // FIXME: this test is flaky
            Assert.deepStrictEqual(finalConfig, expectedConfig)
          },
        )

        Async.it(
          "should not modify config when user selects path already in config",
          async () => {
            let initialConfig = [userAgda.contents, alternativeAgda.contents]
            let selectedPath = userAgda.contents // already in config

            let (logs, finalConfig) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
              ~description="Selected",
            )

            // Should not modify config since path already exists
            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
          },
        )

        Async.it(
          "should add new path to config when user selects different path",
          async () => {
            let initialConfig = [userAgda.contents]
            let selectedPath = alternativeAgda.contents

            let (logs, finalConfig) = await UITestBuilders.simulatePathSelection(
              initialConfig,
              selectedPath,
              ~description="",
            )

            // Should add the new path to existing config
            let expectedConfig = Array.concat(initialConfig, [selectedPath])
            Assert.deepStrictEqual(logs, [Log.Config(Changed(initialConfig, expectedConfig))])
            Assert.deepStrictEqual(finalConfig, expectedConfig)
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

            let (logs, finalConfig) = await UITestBuilders.simulateDownloadAction(
              initialConfig,
              ~isAlreadyDownloaded=false,
            )

            // Should add the downloaded ALS path to config
            let expectedConfig = Array.concat(initialConfig, [downloadedALS.contents])
            Assert.deepStrictEqual(logs, [Log.Config(Changed(initialConfig, expectedConfig))])
            Assert.deepStrictEqual(finalConfig, expectedConfig)
          },
        )

        Async.it(
          "should add already downloaded path to config when user selects already downloaded ALS",
          async () => {
            let initialConfig = [userAgda.contents]

            let (logs, finalConfig) = await UITestBuilders.simulateDownloadAction(
              initialConfig,
              ~isAlreadyDownloaded=true,
            )

            // Should add the already downloaded ALS path to config
            let expectedConfig = Array.concat(initialConfig, [downloadedALS.contents])
            Assert.deepStrictEqual(logs, [Log.Config(Changed(initialConfig, expectedConfig))])
            Assert.deepStrictEqual(finalConfig, expectedConfig)
          },
        )
      },
    )

    describe(
      "Non-endpoint selections",
      () => {
        Async.it(
          "should handle empty selection correctly",
          async () => {
            let initialConfig = [userAgda.contents]

            let (logs, finalConfig) = await UITestBuilders.simulateEmptySelection(initialConfig)

            // Config should remain unchanged
            Assert.deepStrictEqual(logs, [])
            Assert.deepStrictEqual(finalConfig, initialConfig)
          },
        )
      },
    )
  })
})
