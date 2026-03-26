open Mocha
open State__SwitchVersion.ItemData

module TestData = {
  // Mock endpoint entries for testing
  let createMockEntry = (
    endpoint: Memento.Endpoints.endpoint,
    ~error: option<string>=?,
    (),
  ): Memento.Endpoints.entry => {
    endpoint,
    timestamp: Date.make(),
    error,
  }

  let agdaEntry = createMockEntry(Agda(Some("2.6.4")), ())
  let agdaUnknownEntry = createMockEntry(Agda(None), ())
  let alsEntry = createMockEntry(ALS(Some(("4.0.0", "2.6.4", None))), ())
  let unknownEntry = createMockEntry(Unknown, ~error="Permission denied", ())

  // Simple mock functions for testing
  let createMockMemento = () => Memento.make(None)
  let createMockExtensionUri = () => VSCode.Uri.file("/test/extension")

  let makeMockConnection = (_path, _version): Connection.t => {
    %raw(`{
      TAG: "Agda",
      _0: {
        chan: { removeAllListeners: () => {} },
        process: { status: "Destroyed" },
        encountedFirstPrompt: false,
        version: _version,
        path: _path
      },
      _1: _path,
      _2: _version
    }`)
  }
}

describe("State__SwitchVersion", () => {
  describe("Core", () => {
    describe(
      "getEndpointDisplayInfo",
      () => {
        it(
          "should format Agda endpoint with version",
          () => {
            let entry = TestData.agdaEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getEndpointDisplayInfo(
              "agda",
              entry,
            )

            Assert.deepStrictEqual(label, "Agda v2.6.4")
            Assert.deepStrictEqual(errorDescription, None)
          },
        )

        it(
          "should format Agda endpoint without version",
          () => {
            let entry = TestData.agdaUnknownEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getEndpointDisplayInfo(
              "agda",
              entry,
            )

            Assert.deepStrictEqual(label, "Agda (version unknown)")
            Assert.deepStrictEqual(errorDescription, None)
          },
        )

        it(
          "should format ALS endpoint with versions",
          () => {
            let entry = TestData.alsEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getEndpointDisplayInfo(
              "als",
              entry,
            )

            Assert.deepStrictEqual(label, "$(squirrel)  ALS v4.0.0, Agda v2.6.4")
            Assert.deepStrictEqual(errorDescription, None)
          },
        )

        it(
          "should format error endpoint",
          () => {
            let entry = TestData.unknownEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getEndpointDisplayInfo(
              "broken-agda",
              entry,
            )

            Assert.deepStrictEqual(label, "$(error) broken-agda")
            Assert.deepStrictEqual(errorDescription, Some("Error: Permission denied"))
          },
        )

        it(
          "should format unknown endpoint without error",
          () => {
            let entry = TestData.createMockEntry(Unknown, ())
            let (label, errorDescription) = State__SwitchVersion.ItemData.getEndpointDisplayInfo(
              "mystery",
              entry,
            )

            Assert.deepStrictEqual(label, "$(question) mystery")
            Assert.deepStrictEqual(errorDescription, Some("Unknown executable"))
          },
        )
      },
    )

    describe(
      "shouldEndpointHaveIcon",
      () => {
        it(
          "should return true for Agda endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(Agda(Some("2.6.4"))),
              true,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(Agda(None)),
              true,
            )
          },
        )

        it(
          "should return false for ALS endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(
                ALS(Some(("4.0.0", "2.6.4", None))),
              ),
              false,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(ALS(None)),
              false,
            )
          },
        )

        it(
          "should return false for unknown endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(Unknown),
              false,
            )
          },
        )
      },
    )

    describe(
      "inferEndpointType",
      () => {
        it(
          "should recognize als.wasm as ALS endpoint",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("als.wasm"),
              Memento.Endpoints.ALS(None),
            )
          },
        )

        it(
          "should recognize als as ALS endpoint",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("als"),
              Memento.Endpoints.ALS(None),
            )
          },
        )

        it(
          "should recognize als.exe as ALS endpoint",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("als.exe"),
              Memento.Endpoints.ALS(None),
            )
          },
        )

        it(
          "should recognize agda as Agda endpoint",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("agda"),
              Memento.Endpoints.Agda(None),
            )
          },
        )

        it(
          "should recognize agda-2.6.4 as Agda endpoint",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("agda-2.6.4"),
              Memento.Endpoints.Agda(None),
            )
          },
        )

        it(
          "should recognize unknown executables",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("unknown"),
              Memento.Endpoints.Unknown,
            )
          },
        )

        it(
          "should extract basename from full paths",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType(
                "/path/to/dev-als/als.wasm",
              ),
              Memento.Endpoints.ALS(None),
            )
          },
        )

        it(
          "should handle uppercase extensions",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("ALS.WASM"),
              Memento.Endpoints.ALS(None),
            )
          },
        )

        it(
          "should recognize als- prefixed executables",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferEndpointType("als-server"),
              Memento.Endpoints.ALS(None),
            )
          },
        )
      },
    )
  })

  describe("QuickPick", () => {
    let extensionUri = TestData.createMockExtensionUri()

    describe(
      "fromItemData",
      () => {
        it(
          "should create quickpick item from endpoint data with correct properties",
          () => {
            let entry = TestData.agdaEntry
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/agda", entry, false)
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "Agda v2.6.4")
            Assert.deepStrictEqual(item.description, Some(""))
            Assert.deepStrictEqual(item.detail, Some("/usr/bin/agda"))
          },
        )

        it(
          "should include icon for Agda endpoints",
          () => {
            let entry = TestData.agdaEntry
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/agda", entry, false)
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            // Check that iconPath is present for Agda
            switch item.iconPath {
            | Some(_) => () // Expected
            | None => Assert.fail("Expected iconPath for Agda endpoint")
            }
          },
        )

        it(
          "should not include icon for ALS endpoints",
          () => {
            let entry = TestData.alsEntry
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/als", entry, false)
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            // Check that iconPath is absent for ALS
            switch item.iconPath {
            | None => () // Expected
            | Some(_) => Assert.fail("Did not expect iconPath for ALS endpoint")
            }
          },
        )
      },
    )

    describe(
      "separator items",
      () => {
        it(
          "should create separator with correct kind",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = Separator("Test Section")
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "Test Section")
            Assert.deepStrictEqual(item.kind, Some(VSCode.QuickPickItemKind.Separator))
          },
        )
      },
    )

    describe(
      "no installations item",
      () => {
        it(
          "should create placeholder item",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = NoInstallations
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "$(info) No installations found")
            Assert.deepStrictEqual(item.description, Some("Try installing Agda or ALS first"))
            Assert.deepStrictEqual(item.detail, Some("No executable paths detected"))
          },
        )
      },
    )

    describe(
      "download items",
      () => {
        it(
          "should create download item when not downloaded",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = DownloadAction(false, "ALS v1.0.0", "native")
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(
              item.label,
              "$(cloud-download)  Download Agda Language Server (native)",
            )
            Assert.deepStrictEqual(item.description, Some(""))
            Assert.deepStrictEqual(item.detail, Some("ALS v1.0.0"))
          },
        )

        it(
          "should create download item when already downloaded",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = DownloadAction(true, "ALS v1.0.0", "native")
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(
              item.label,
              "$(cloud-download)  Download Agda Language Server (native)",
            )
            Assert.deepStrictEqual(item.description, Some("Downloaded and installed"))
            Assert.deepStrictEqual(item.detail, Some("ALS v1.0.0"))
          },
        )
      },
    )
  })

  describe("View", () => {
    it(
      "should create quickpick with correct initial state",
      () => {
        let qp = State__SwitchVersion.View.make(Chan.make())

        Assert.deepStrictEqual(Array.length(qp.items), 0)
        Assert.deepStrictEqual(Array.length(qp.subscriptions), 0)
      },
    )

    it(
      "should update items correctly",
      () => {
        let qp = State__SwitchVersion.View.make(Chan.make())
        let itemData: State__SwitchVersion.ItemData.t = NoInstallations
        let items = [
          State__SwitchVersion.Item.fromItemData(itemData, TestData.createMockExtensionUri()),
        ]

        qp->State__SwitchVersion.View.updateItems(items)

        Assert.deepStrictEqual(Array.length(qp.items), 1)
        Assert.deepStrictEqual(
          qp.items[0]->Option.map(item => item.label),
          Some("$(info) No installations found"),
        )
      },
    )
  })

  describe("Core Integration Tests", () => {
    describe(
      "entriesToItemData",
      () => {
        it(
          "should hide Installed section when no endpoints are found",
          () => {
            let entries = Dict.make()
            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [],
            )

            let installedSeparator = itemData->Array.find(
              data =>
                switch data {
                | Separator("Installed") => true
                | _ => false
                },
            )

            let noInstallationsPlaceholder = itemData->Array.find(
              data =>
                switch data {
                | NoInstallations => true
                | _ => false
                },
            )

            Assert.ok(installedSeparator->Option.isNone)
            Assert.ok(noInstallationsPlaceholder->Option.isNone)
            Assert.deepStrictEqual(Array.length(itemData), 2) // Misc separator + Delete downloads

            switch itemData[0] {
            | Some(Separator("Misc")) => () // Expected
            | _ => Assert.fail("Expected Misc separator")
            }

            switch itemData[1] {
            | Some(DeleteDownloads) => () // Expected
            | _ => Assert.fail("Expected DeleteDownloads item")
            }
          },
        )

        it(
          "should create items with separator when entries exist",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
            entries->Dict.set("/usr/bin/als", TestData.alsEntry)

            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [],
            )

            Assert.deepStrictEqual(Array.length(itemData), 5) // Installed separator + 2 endpoints + Misc separator + Delete downloads

            switch itemData[0] {
            | Some(Separator("Installed")) => () // Expected
            | _ => Assert.fail("Expected Installed separator")
            }

            switch itemData[3] {
            | Some(Separator("Misc")) => () // Expected
            | _ => Assert.fail("Expected Misc separator")
            }

            switch itemData[4] {
            | Some(DeleteDownloads) => () // Expected
            | _ => Assert.fail("Expected DeleteDownloads item")
            }
          },
        )

        it(
          "should include download section when download info is provided",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)

            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [(false, "ALS v1.0.0", "native")],
            )

            Assert.deepStrictEqual(Array.length(itemData), 6) // Installed separator + 1 item + Download separator + download item + Misc separator + Delete downloads

            let downloadSeparator = itemData->Array.find(
              data =>
                switch data {
                | Separator("Download (channel: Hardcoded)") => true
                | _ => false
                },
            )

            let downloadAction = itemData->Array.find(
              data =>
                switch data {
                | DownloadAction(false, "ALS v1.0.0", "native") => true
                | _ => false
                },
            )

            Assert.ok(downloadSeparator->Option.isSome)
            Assert.ok(downloadAction->Option.isSome)
          },
        )

        it(
          "should include active channel in download section header",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)

            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [(false, "ALS v1.0.0", "native")],
            )

            let channelAwareDownloadSeparator = itemData->Array.find(
              data =>
                switch data {
                | Separator("Download (channel: Hardcoded)") => true
                | _ => false
                },
            )

            Assert.ok(channelAwareDownloadSeparator->Option.isSome)
          },
        )

      },
    )
  })

  describe("Events", () => {
    let mockAgda = ref("")

    Async.before(async () => {
      mockAgda := await Test__Util.Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-sv-raw-key")
    })

    Async.after(async () => {
      await Test__Util.Endpoint.Agda.destroy(mockAgda.contents)
    })

    // Simple mock platform for testing
    let makeMockPlatform = (): Platform.t => Mock.Platform.makeWithAgda()
    let makeMockPlatformWithBareCommands = (): Platform.t => {
      module MockPlatform = {
        include Desktop.Desktop
        let findCommand = (command, ~timeout as _timeout=1000) =>
          switch command {
          | "agda" => Promise.resolve(Ok(mockAgda.contents))
          // Reuse the stable Agda mock path to keep this test focused on keying behavior.
          | "als" => Promise.resolve(Ok(mockAgda.contents))
          | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
      }
      module(MockPlatform)
    }

    // Create a test state with proper channels
    let createTestStateWithPlatformAndStorage = (
      platform: Platform.t,
      storageUri: VSCode.Uri.t,
    ) => {
      let channels = {
        State.inputMethod: Chan.make(),
        responseHandled: Chan.make(),
        commandHandled: Chan.make(),
        log: Chan.make(),
      }

      let mockEditor = %raw(`{
        document: { fileName: "test.agda" }
      }`)

      let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))

      State.make(
        "test-id",
        platform,
        channels,
        storageUri,
        mockExtensionUri,
        Memento.make(None),
        mockEditor,
        None,
      )
    }
    let createTestStateWithPlatform = (platform: Platform.t) =>
      createTestStateWithPlatformAndStorage(platform, VSCode.Uri.file(NodeJs.Os.tmpdir()))
    let createTestState = () => createTestStateWithPlatform(makeMockPlatform())

    Async.it(
      "should not show native download option on web platform",
      async () => {
        module MockWebPlatform = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Web))

          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))

          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)

          let resolveDownloadChannel = (channel, _useCache) =>
            async (_memento, _globalStorageUri, _platform) =>
              switch channel {
              | Connection__Download.Channel.LatestALS =>
                Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              | Connection__Download.Channel.DevALS =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.DevALS,
                    "https://example.invalid/dev-als.wasm",
                    "dev-als",
                  ),
                )
              | Connection__Download.Channel.Hardcoded =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.Hardcoded,
                    "https://example.invalid/hardcoded-als.wasm",
                    "hardcoded-als",
                  ),
                )
              }

          let download = (_globalStorageUri, _downloadDescriptor) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

          let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let webPlatform: Platform.t = module(MockWebPlatform)
        let state = createTestStateWithPlatform(webPlatform)
        let placeholderItems = await State__SwitchVersion.Download.getPlaceholderDownloadItems(
          webPlatform,
        )
        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
          state,
          webPlatform,
        )

        let placeholderHasNativeLatest =
          placeholderItems->Array.some(((/* downloaded */ _, /* version */ _, downloadType)) =>
            downloadType == "native"
          )
        let hasNativeLatest =
          downloadItems->Array.some(((/* downloaded */ _, /* version */ _, downloadType)) =>
            downloadType == "native"
          )

        Assert.deepStrictEqual(placeholderHasNativeLatest, false)
        Assert.deepStrictEqual(hasNativeLatest, false)
      },
    )

    Async.it(
      "should expose Hardcoded and DevALS channels on Desktop, only Hardcoded on Web",
      async () => {
        let desktopChannels = await State__SwitchVersion.Download.getAvailableChannels(
          makeMockPlatform(),
        )
        Assert.deepStrictEqual(desktopChannels, [Connection__Download.Channel.Hardcoded, Connection__Download.Channel.DevALS])

        module MockWebPlatform = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Web))

          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))

          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)

          let resolveDownloadChannel = (_channel, _useCache) =>
            async (_memento, _globalStorageUri, _platform) =>
              Error(Connection__Download.Error.CannotFindCompatibleALSRelease)

          let download = (_globalStorageUri, _downloadDescriptor) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

          let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let webChannels = await State__SwitchVersion.Download.getAvailableChannels(
          module(MockWebPlatform),
        )
        Assert.deepStrictEqual(webChannels, [Connection__Download.Channel.Hardcoded])
      },
    )

    Async.it(
      "should render channel switch item when multiple channels are available on Desktop",
      async () => {
        let availableChannels = await State__SwitchVersion.Download.getAvailableChannels(
          makeMockPlatform(),
        )
        let shouldShowChannelSelector = Array.length(availableChannels) >= 2
        Assert.deepStrictEqual(shouldShowChannelSelector, true)

        let itemData: array<State__SwitchVersion.ItemData.t> =
          State__SwitchVersion.ItemData.entriesToItemData(
            Dict.make(),
            None,
            [(false, "ALS v1.0.0", "native")],
            ~showChannelSelector=shouldShowChannelSelector,
          )

        let hasSelectOtherChannels =
          itemData->Array.some(item =>
            switch item {
            | SelectOtherChannels => true
            | _ => false
            }
          )

        Assert.deepStrictEqual(hasSelectOtherChannels, true)
      },
    )

    Async.it(
      "should expose download variants instead of channel-tagged latest/dev items",
      async () => {
        module MockDesktopHardcodedOnly = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))

          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))

          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)

          let resolveDownloadChannel = (channel, _useCache) =>
            async (_memento, _globalStorageUri, _platform) =>
              switch channel {
              | Connection__Download.Channel.Hardcoded =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.Hardcoded,
                    "https://example.invalid/hardcoded-native",
                    "hardcoded-als",
                  ),
                )
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }

          let download = (_globalStorageUri, _downloadDescriptor) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

          let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let platform: Platform.t = module(MockDesktopHardcodedOnly)
        let state = createTestStateWithPlatform(platform)
        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)

        let hasChannelTaggedItems =
          downloadItems->Array.some(((_, _, downloadType)) =>
            downloadType == "latest" || downloadType == "dev"
          )

        Assert.deepStrictEqual(hasChannelTaggedItems, false)
      },
    )

    Async.it(
      "should hide native download variant when its managed path is already in connection.paths",
      async () => {
        let platform = makeMockPlatform()
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        let nativeManagedPath = State__SwitchVersion.Download.expectedPathForVariant(
          state.globalStorageUri,
          State__SwitchVersion.Download.Native,
        )
        await Config.Connection.setAgdaPaths(state.channels.log, [nativeManagedPath])

        let _ = await State__SwitchVersion.SwitchVersionManager.syncWithFilesystem(manager, platform)
        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, downloadItems)

        let hasNativeDownloadAction =
          itemData->Array.some(item =>
            switch item {
            | DownloadAction(_, _, "native") => true
            | _ => false
            }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        Assert.deepStrictEqual(hasNativeDownloadAction, false)
      },
    )

    Async.it(
      "should not treat checking-availability placeholder as endpoint selection",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let sawSelectionCompleted = ref(false)
        let sawSelectedDownloadAction = ref(false)

        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(SelectionCompleted) => sawSelectionCompleted := true
          | Log.SwitchVersionUI(SelectedDownloadAction(_, _)) =>
            sawSelectedDownloadAction := true
          | _ => ()
          }
        )

        let selectedItem: VSCode.QuickPickItem.t = {
          label: State__SwitchVersion.Constants.checkingAvailability,
        }

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedDownloadAction.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, false)
      },
    )

    Async.it(
      "should not treat unknown quickpick item labels as endpoint selection",
      async () => {
        let state = createTestState()
        let selectedPath = mockAgda.contents
        let discoveredEndpoints = Dict.make()
        discoveredEndpoints->Dict.set(selectedPath, Memento.Endpoints.Agda(None))
        await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let sawSelectedEndpoint = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(SelectedEndpoint(_, _, _)) => sawSelectedEndpoint := true
          | _ => ()
          }
        )

        let selectedItem: VSCode.QuickPickItem.t = {
          label: "__unexpected_item__",
          description: "",
          detail: selectedPath,
        }

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedEndpoint.contents, false)
      },
    )

    Async.it(
      "should keep main quickpick open when selecting channel-switch button",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let sawDestroyed = ref(false)
        let sawSelectionCompleted = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
          | Log.SwitchVersionUI(SelectionCompleted) => sawSelectionCompleted := true
          | _ => ()
          }
        )

        let selectedItem: VSCode.QuickPickItem.t = %raw(`({ label: "$(tag)  Select other channels" })`)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawDestroyed.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, false)
      },
    )

    Async.it(
      "should keep existing shared connection when switch target cannot be established",
      async () => {
        // Keep this test isolated from prior registry state.
        Registry__Connection.status := Empty

        let state = createTestState()
        let existingPath = "/usr/bin/agda"
        let existingConnection = TestData.makeMockConnection(existingPath, "2.6.4")

        Registry__Connection.status :=
          Active({
            connection: existingConnection,
            users: Belt.Set.String.fromArray(["owner-before-switch"]),
            currentOwnerId: None,
            queue: Promise.resolve(),
          })

        let missingPath = "/__agda_mode_vscode_nonexistent__/binary_should_not_exist_280"

        let completion =
          State__SwitchVersion.switchAgdaVersion(state, missingPath)->Promise.thenResolve(_ => "done")
        let timeout = Util.Promise_.setTimeout(1000)->Promise.thenResolve(_ => "timeout")
        let winner = await Promise.race([completion, timeout])

        switch winner {
        | "done" =>
          switch Registry__Connection.status.contents {
          | Active(resource) =>
            Assert.deepStrictEqual(Connection.getPath(resource.connection), existingPath)
          | _ =>
            Assert.fail(
              "Expected existing shared connection to remain active when switch target fails to establish",
            )
          }
        | "timeout" =>
          Registry__Connection.status := Empty
          Assert.fail("switchAgdaVersion hung while switching to an invalid target")
        | _ => Assert.fail("Unexpected race result")
        }

        Registry__Connection.status := Empty
      },
    )

    Async.it(
      "should have an endpoint marked as selected onActivation",
      async () => {
        /**
         * TEST PURPOSE: Expose fresh install UX problem where no endpoint appears "Selected"
         * 
         * PROBLEM DESCRIPTION:
         * On fresh installs or after clearing extension data, users experience confusing UI behavior:
         * 1. User runs Command.Load → Agda connection establishes successfully (auto-detected)
         * 2. User opens switch version UI → Sees multiple endpoints but NONE marked as "Selected"
         * 3. User is confused: "Which connection am I currently using?"
         * 4. Only after manually switching connection does one show as "Selected"
         * 
         * ROOT CAUSE:
         * - Command.Load establishes connection without setting Memento.PickedConnection
         * - Memento.PickedConnection.get() returns None on fresh installs
         * - UI selection logic requires explicit memento entry to mark endpoint as "Selected"
         * - No mechanism exists to infer selection from active connection state
         * 
         * REPRODUCTION:
         * This test simulates fresh install by ensuring Memento.PickedConnection = None,
         * then invokes onActivate and observes the logged UpdateEndpoints events.
         */
        let state = createTestState()
        let loggedEvents = []

        // Subscribe to log channel to capture UpdateEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedEndpoints(endpoints)) =>
              loggedEvents->Array.push(endpoints)
            | _ => ()
            }
          },
        )

        // SIMULATE: Fresh install - ensure no picked connection in memento
        await Memento.PickedConnection.set(state.memento, None)

        // SIMULATE: Discovered endpoints (as if filesystem sync already found them)
        let discoveredEndpoints = Dict.make()
        discoveredEndpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
        await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

        // SIMULATE: Active connection (Command.Load established connection)
        // Create a mock connection that matches one of the discovered endpoints
        let mockConnection = TestData.makeMockConnection("/usr/bin/agda", "2.6.4")
        Registry__Connection.status :=
          Active({
            connection: mockConnection,
            users: Belt.Set.String.empty,
            currentOwnerId: None,
            queue: Promise.resolve(),
          })

        // INVOKE: onActivate to trigger the actual UI logic
        await State__SwitchVersion.Handler.onActivate(state, makeMockPlatform())

        // ANALYZE: Check logged UpdateEndpoints events for selection marking
        let allEndpointsFromLogs = loggedEvents->Array.flat
        let anyEndpointSelected =
          allEndpointsFromLogs->Array.some(((_, _, _, isSelected)) => isSelected)

        // VERIFY: Assert that the fix works (endpoint marked as selected)
        Assert.ok(anyEndpointSelected) // Expected: Active connection endpoint should be marked as selected
      },
    )

    Async.it(
      "should prefer memento selection over active connection inference",
      async () => {
        /**
         * TEST PURPOSE: Ensure explicit user selection takes precedence over connection inference
         * 
         * SCENARIO:
         * 1. User has multiple endpoints discovered
         * 2. User has explicitly selected one endpoint (stored in memento)
         * 3. But a different endpoint is currently active (connection established)
         * 4. UI should show the explicitly selected endpoint as "Selected", not the active one
         * 
         * This tests the precedence logic: explicit selection > active connection inference
         */
        let state = createTestState()
        let loggedEvents = []

        // Subscribe to log channel to capture UpdateEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedEndpoints(endpoints)) =>
              loggedEvents->Array.push(endpoints)
            | _ => ()
            }
          },
        )

        // SIMULATE: Multiple discovered endpoints
        let discoveredEndpoints = Dict.make()
        discoveredEndpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
        discoveredEndpoints->Dict.set(
          "/opt/homebrew/bin/agda",
          Memento.Endpoints.Agda(Some("2.6.3")),
        )
        await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

        // SIMULATE: User explicitly selected one endpoint (stored in memento)
        await Memento.PickedConnection.set(state.memento, Some("/usr/bin/agda"))

        // SIMULATE: But different endpoint is currently active
        let mockConnection = TestData.makeMockConnection("/opt/homebrew/bin/agda", "2.6.3")
        Registry__Connection.status :=
          Active({
            connection: mockConnection,
            users: Belt.Set.String.empty,
            currentOwnerId: None,
            queue: Promise.resolve(),
          })

        // INVOKE: onActivate to trigger the actual UI logic
        await State__SwitchVersion.Handler.onActivate(state, makeMockPlatform())

        // ANALYZE: Check which endpoint is marked as selected
        let allEndpointsFromLogs = loggedEvents->Array.flat

        // Find the selected endpoint
        let selectedEndpoint =
          allEndpointsFromLogs->Array.find(((_, _, _, isSelected)) => isSelected)

        // VERIFY: The explicitly selected endpoint (from memento) should be marked as selected
        // NOT the active connection endpoint
        switch selectedEndpoint {
        | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/usr/bin/agda") // Memento selection should win
        | None => Assert.fail("Expected one endpoint to be marked as selected")
        }

        // VERIFY: Only one endpoint should be selected
        let selectedCount =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)->Array.length
        Assert.deepStrictEqual(selectedCount, 1)
      },
    )

    Async.it(
      "should keep endpoint version keys as raw bare commands after endpoint selection",
      async () => {
        let platform = makeMockPlatformWithBareCommands()
        let runSelectionAndAssert = async (selectedPath: string) => {
          let state = createTestStateWithPlatform(platform)

          let view = State__SwitchVersion.View.make(state.channels.log)
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)
          let onOperationComplete = Log.on(
            state.channels.log,
            log =>
              switch log {
              | Log.SwitchVersionUI(SelectionCompleted) => true
              | _ => false
              },
          )

          let discoveredEndpoints = Dict.make()
          discoveredEndpoints->Dict.set("agda", Memento.Endpoints.Agda(None))
          discoveredEndpoints->Dict.set("als", Memento.Endpoints.ALS(None))
          await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

          let selectedItemLabel = switch Memento.Endpoints.get(state.memento, selectedPath) {
          | Some(entry) =>
            let (label, _description) = State__SwitchVersion.ItemData.getEndpointDisplayInfo(
              NodeJs.Path.basename(selectedPath),
              entry,
            )
            label
          | None =>
            Assert.fail("Expected discovered endpoint to exist in memento")
            ""
          }

          let selectedItem: VSCode.QuickPickItem.t = {
            label: selectedItemLabel,
            description: "",
            detail: selectedPath,
          }

          State__SwitchVersion.Handler.onSelection(
            state,
            platform,
            manager,
            ref([Connection__Download.Channel.Hardcoded]),
            ref(Connection__Download.Channel.Hardcoded),
            _downloadInfo => Promise.resolve(),
            view,
            [selectedItem],
          )
          await onOperationComplete
          view->State__SwitchVersion.View.destroy

          Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(selectedPath))
          Assert.ok(Memento.Endpoints.get(state.memento, selectedPath)->Option.isSome)
          Assert.ok(Memento.Endpoints.get(state.memento, mockAgda.contents)->Option.isNone)
        }

        await runSelectionAndAssert("agda")
        await runSelectionAndAssert("als")
      },
    )

    Async.it(
      "should remove download paths from connection.paths and preserve PreferredCandidate on Delete Downloads",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-switch-version-delete-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let hardcodedDirUri = VSCode.Uri.joinPath(storageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(hardcodedDirUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let keepBareCommand = "agda"
        let downloadedLatest =
          VSCode.Uri.joinPath(storageUri, ["latest-als", "als"])->VSCode.Uri.fsPath
        let downloadedDevWasm =
          VSCode.Uri.joinPath(storageUri, ["dev-als", "als.wasm"])->VSCode.Uri.toString
        let downloadedHardcoded =
          VSCode.Uri.joinPath(storageUri, ["hardcoded-als", "als"])->VSCode.Uri.fsPath

        await Config.Connection.setAgdaPaths(
          state.channels.log,
          [keepPath, downloadedLatest, downloadedDevWasm, downloadedHardcoded, keepBareCommand],
        )
        await Memento.PickedConnection.set(state.memento, Some(downloadedLatest))

        let selectedItem: VSCode.QuickPickItem.t = {
          label: State__SwitchVersion.Constants.deleteDownloads,
          description: "",
          detail: "",
        }

        let onSelectionCompleted = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )
        await onSelectionCompleted

        // Delete Downloads MUST remove download paths from connection.paths, MUST NOT modify PreferredCandidate
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath, keepBareCommand])
        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(downloadedLatest))
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(VSCode.Uri.fsPath(hardcodedDirUri)), false)

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should remove download paths with unescaped space URIs from connection.paths on Delete Downloads",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda switch uri space " ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let hardcodedDirUri = VSCode.Uri.joinPath(storageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(hardcodedDirUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let encodedWasmUri =
          VSCode.Uri.joinPath(storageUri, ["hardcoded-als", "als.wasm"])->VSCode.Uri.toString
        let unescapedWasmUri =
          "file://" ++ VSCode.Uri.joinPath(storageUri, ["hardcoded-als", "als.wasm"])->VSCode.Uri.fsPath

        // Sanity guard: this test only makes sense when URI encoding differs.
        Assert.notDeepStrictEqual(unescapedWasmUri, encodedWasmUri)

        await Config.Connection.setAgdaPaths(state.channels.log, [keepPath, unescapedWasmUri])

        let selectedItem: VSCode.QuickPickItem.t = {
          label: State__SwitchVersion.Constants.deleteDownloads,
          description: "",
          detail: "",
        }

        let onSelectionCompleted = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )
        await onSelectionCompleted

        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath])

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should remove download paths from connection.paths and preserve PickedConnection on Delete Downloads",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-switch-version-delete-keep-picked-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let downloadedLatest =
          VSCode.Uri.joinPath(storageUri, ["latest-als", "als"])->VSCode.Uri.fsPath

        await Config.Connection.setAgdaPaths(state.channels.log, [keepPath, downloadedLatest])
        await Memento.PickedConnection.set(state.memento, Some(keepPath))

        let selectedItem: VSCode.QuickPickItem.t = {
          label: State__SwitchVersion.Constants.deleteDownloads,
          description: "",
          detail: "",
        }

        let onSelectionCompleted = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )
        await onSelectionCompleted

        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath])
        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(keepPath))

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should handle download workflow correctly",
      async () => {
        /**
         * TEST PURPOSE: Test the entire download workflow through real UI interactions
         * 
         * SCENARIO:
         * 1. User activates switch version UI
         * 2. Download item appears (not downloaded yet)
         * 3. User would click download → download completes 
         * 4. UI updates to show "Downloaded and installed"
         * 5. Download item behavior changes appropriately
         * 
         * This tests the complete download integration through actual onActivate flow
         */
        let state = createTestState()
        let loggedEvents = []

        // Subscribe to log channel to capture UpdateEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedEndpoints(endpoints)) =>
              loggedEvents->Array.push(endpoints)
            | _ => ()
            }
          },
        )

        // SIMULATE: Basic endpoint setup
        let discoveredEndpoints = Dict.make()
        discoveredEndpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
        await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

        // SIMULATE: No picked connection (fresh state)
        await Memento.PickedConnection.set(state.memento, None)

        // SIMULATE: Active connection
        let mockConnection = TestData.makeMockConnection("/usr/bin/agda", "2.6.4")
        Registry__Connection.status :=
          Active({
            connection: mockConnection,
            users: Belt.Set.String.empty,
            currentOwnerId: None,
            queue: Promise.resolve(),
          })

        // PHASE 1: Test initial state (download available but not downloaded)
        // Mock platform to return download available
        let makeMockPlatformWithDownload = (): Platform.t => Mock.Platform.makeWithAgda()

        // INVOKE: onActivate to trigger the actual UI logic with download available
        await State__SwitchVersion.Handler.onActivate(state, makeMockPlatformWithDownload())

        // ANALYZE: Check logged UpdateEndpoints events
        let allEndpointsFromLogs = loggedEvents->Array.flat

        // VERIFY: Endpoint selection still works correctly even with download items present
        let selectedEndpoints =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)
        Assert.deepStrictEqual(Array.length(selectedEndpoints), 1) // One endpoint should be selected

        // Find the selected endpoint
        switch selectedEndpoints[0] {
        | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/usr/bin/agda") // Should be the active connection
        | None => Assert.fail("Expected one endpoint to be selected")
        }

        // VERIFY: All endpoints are properly logged (this tests the download integration doesn't break endpoint logging)
        Assert.ok(Array.length(allEndpointsFromLogs) > 0) // Should have endpoints logged

        // VERIFY: No errors in endpoint entries
        let hasErrors = allEndpointsFromLogs->Array.some(
          ((_, _, error, _)) =>
            switch error {
            | Some(_) => true
            | None => false
            },
        )
        Assert.ok(!hasErrors) // Should not have errors in normal download workflow
      },
    )

    Async.it(
      "should set picked connection to downloaded path when downloading via handler",
      async () => {
        let testCases = [
          (Some("/usr/bin/agda"), State__SwitchVersion.Download.Native, false),
          (Some("/usr/bin/agda"), State__SwitchVersion.Download.Native, true),
          (Some("/usr/bin/agda"), State__SwitchVersion.Download.WASM, false),
          (Some("/usr/bin/agda"), State__SwitchVersion.Download.WASM, true),
          (None, State__SwitchVersion.Download.Native, false),
          (None, State__SwitchVersion.Download.Native, true),
          (None, State__SwitchVersion.Download.WASM, false),
          (None, State__SwitchVersion.Download.WASM, true),
        ]

        let runCase = async (
          initialPicked: option<string>,
          variant: State__SwitchVersion.Download.variant,
          downloaded: bool,
        ) => {
          let state = createTestState()
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)

          let discoveredEndpoints = Dict.make()
          discoveredEndpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
          discoveredEndpoints->Dict.set("/opt/homebrew/bin/agda", Memento.Endpoints.Agda(Some("2.6.3")))
          await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

          await Memento.PickedConnection.set(state.memento, initialPicked)

          let activePath = "/opt/homebrew/bin/agda"
          let activeConnection = TestData.makeMockConnection(activePath, "2.6.3")
          Registry__Connection.status :=
            Active({
              connection: activeConnection,
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          let expectedDownloadPath =
            State__SwitchVersion.Download.expectedPathForVariant(state.globalStorageUri, variant)
          let platform =
            Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath)
          let versionString = "ALS v" ++ (downloaded ? "downloaded" : "to-download")

          await State__SwitchVersion.Handler.handleDownload(
            state,
            platform,
            variant,
            downloaded,
            versionString,
            ~refreshUI=None,
          )

          let _ = manager->State__SwitchVersion.SwitchVersionManager.refreshFromMemento

          let pickedAfter = Memento.PickedConnection.get(state.memento)
          // Manual UI download should set PreferredCandidate
          Assert.deepStrictEqual(pickedAfter, Some(expectedDownloadPath))

          let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
            manager,
            await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform),
          )

          let selectedEndpoints =
            itemData
            ->Array.filterMap(item =>
              switch item {
              | Endpoint(path, _, true) => Some(path)
              | _ => None
              }
            )
          // After manual UI download, picked is the downloaded path
          Assert.deepStrictEqual(selectedEndpoints, [expectedDownloadPath])
          let hasDownloadedPath = Config.Connection.getAgdaPaths()->Array.some(path => path == expectedDownloadPath)
          Assert.deepStrictEqual(hasDownloadedPath, true)

          Registry__Connection.status := Empty
        }

        // Run cases sequentially to avoid race conditions on shared Registry__Connection.status
        for i in 0 to Array.length(testCases) - 1 {
          let (initialPicked, variant, downloaded) = testCases[i]->Option.getExn
          await runCase(initialPicked, variant, downloaded)
        }
      },
    )

    Async.it(
      "should set picked connection to downloaded path when user clicks download action",
      async () => {
        let cases = [
          State__SwitchVersion.Constants.downloadNativeALS,
          State__SwitchVersion.Constants.downloadWasmALS,
        ]

        let runCase = async (label: string) => {
          let state = createTestState()
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)
          let view = State__SwitchVersion.View.make(state.channels.log)

          // Reset config paths to avoid leaking state from previous tests
          await Config.Connection.setAgdaPaths(state.channels.log, [])

          let selectedVariant =
            label == State__SwitchVersion.Constants.downloadWasmALS
              ? State__SwitchVersion.Download.WASM
              : State__SwitchVersion.Download.Native

          let expectedDownloadPath =
            State__SwitchVersion.Download.expectedPathForVariant(state.globalStorageUri, selectedVariant)

          let discoveredEndpoints = Dict.make()
          discoveredEndpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
          discoveredEndpoints->Dict.set("/opt/homebrew/bin/agda", Memento.Endpoints.Agda(Some("2.6.3")))
          await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

          let previouslyPicked = Some("/usr/bin/agda")
          await Memento.PickedConnection.set(state.memento, previouslyPicked)
          let activePath = "/opt/homebrew/bin/agda"
          Registry__Connection.status :=
            Active({
              connection: TestData.makeMockConnection(activePath, "2.6.3"),
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          let selectedItem: VSCode.QuickPickItem.t = {
            label,
            description: "",
            detail: "",
          }

          let sawSelectedEndpoint = ref(false)
          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(SelectedEndpoint(_, _, _)) => sawSelectedEndpoint := true
            | _ => ()
            },
          )

          let onOperationComplete = Log.on(
            state.channels.log,
            log =>
              switch log {
              | Log.SwitchVersionUI(SelectionCompleted) => true
              | _ => false
              },
          )

          State__SwitchVersion.Handler.onSelection(
            state,
            Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath),
            manager,
            ref([Connection__Download.Channel.Hardcoded]),
            ref(Connection__Download.Channel.Hardcoded),
            _downloadItems => Promise.resolve(),
            view,
            [selectedItem],
          )

          await onOperationComplete

          let _ = manager->State__SwitchVersion.SwitchVersionManager.refreshFromMemento

          // Manual UI download should set PreferredCandidate
          Assert.deepStrictEqual(
            Memento.PickedConnection.get(state.memento),
            Some(expectedDownloadPath),
          )

          let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
            manager,
            await State__SwitchVersion.Download.getAllAvailableDownloads(
              state,
              Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath),
            ),
          )
          let selectedEndpoints =
            itemData
            ->Array.filterMap(item =>
              switch item {
              | Endpoint(path, _, true) => Some(path)
              | _ => None
              }
            )
          Assert.deepStrictEqual(selectedEndpoints, [expectedDownloadPath])
          Assert.deepStrictEqual(sawSelectedEndpoint.contents, false)
        view->State__SwitchVersion.View.destroy
          Registry__Connection.status := Empty
        }

        // Run cases sequentially to avoid race conditions on shared Registry__Connection.status
        for i in 0 to Array.length(cases) - 1 {
          await runCase(cases[i]->Option.getExn)
        }
      },
    )

    Async.it(
      "should clear error without dropping version metadata when re-downloading",
      async () => {
        let state = createTestState()
        let path = "/usr/local/bin/als"

        // Set up an endpoint with known version AND an error
        await Memento.Endpoints.setVersion(
          state.memento,
          path,
          Memento.Endpoints.ALS(Some(("0.2.10", "2.7.0.1", None))),
        )
        await Memento.Endpoints.setError(state.memento, path, "connection timed out")

        // Verify precondition: entry has both version and error
        let before = Memento.Endpoints.get(state.memento, path)
        Assert.deepStrictEqual(
          before->Option.map(e => e.error),
          Some(Some("connection timed out")),
        )
        Assert.deepStrictEqual(
          before->Option.map(e => e.endpoint),
          Some(Memento.Endpoints.ALS(Some(("0.2.10", "2.7.0.1", None)))),
        )

        // ensureEndpointRegistered should clear error but preserve version
        await State__SwitchVersion.Handler.ensureEndpointRegistered(state, path)

        let after = Memento.Endpoints.get(state.memento, path)
        // Error MUST be cleared
        Assert.deepStrictEqual(
          after->Option.map(e => e.error),
          Some(None),
        )
        // Version metadata MUST be preserved
        Assert.deepStrictEqual(
          after->Option.map(e => e.endpoint),
          Some(Memento.Endpoints.ALS(Some(("0.2.10", "2.7.0.1", None)))),
        )
      },
    )

    Async.it(
      "should mark at most one endpoint selected when URI and fsPath aliases coexist",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let fsPath = "/tmp/hardcoded-als/als.wasm"
        let uriPath = "file:///tmp/hardcoded-als/als.wasm"

        // Register both URI and fsPath as separate endpoint entries
        await Memento.Endpoints.setVersion(
          state.memento,
          fsPath,
          Memento.Endpoints.ALS(Some(("0.2.10", "2.7.0.1", None))),
        )
        await Memento.Endpoints.setVersion(
          state.memento,
          uriPath,
          Memento.Endpoints.ALS(None),
        )

        // Set PickedConnection to the URI form
        await Memento.PickedConnection.set(state.memento, Some(uriPath))

        let _ = manager->State__SwitchVersion.SwitchVersionManager.refreshFromMemento

        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
        )

        let selectedEndpoints =
          itemData->Array.filterMap(item =>
            switch item {
            | Endpoint(_, _, true) => Some(true)
            | _ => None
            }
          )

        // Exactly one endpoint MUST be marked selected, not both
        Assert.deepStrictEqual(selectedEndpoints, [true])
      },
    )

    Async.it(
      "should mark only one endpoint when multiple exist",
      async () => {
        /**
         * TEST PURPOSE: Ensure only one endpoint is marked as selected when multiple endpoints exist
         * 
         * SCENARIO:
         * Multiple endpoints are discovered (different Agda versions, different paths)
         * Only the correctly matched endpoint should be marked as "Selected"
         * All other endpoints should remain unselected
         * 
         * This tests:
         * - Path matching logic works correctly across multiple endpoints
         * - No logic errors cause multiple endpoints to be selected
         * - Selection marking is precise and doesn't have false positives
         */
        let state = createTestState()
        let loggedEvents = []

        // Subscribe to log channel to capture UpdatedEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedEndpoints(endpoints)) =>
              loggedEvents->Array.push(endpoints)
            | _ => ()
            }
          },
        )

        // SIMULATE: Multiple discovered endpoints at different paths
        let discoveredEndpoints = Dict.make()
        discoveredEndpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
        discoveredEndpoints->Dict.set(
          "/opt/homebrew/bin/agda",
          Memento.Endpoints.Agda(Some("2.6.3")),
        )
        discoveredEndpoints->Dict.set("/usr/local/bin/agda", Memento.Endpoints.Agda(Some("2.6.2")))
        discoveredEndpoints->Dict.set(
          "/usr/bin/als",
          Memento.Endpoints.ALS(Some(("4.0.0", "2.6.4", None))),
        )
        await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)

        // SIMULATE: User has explicitly selected one specific endpoint
        await Memento.PickedConnection.set(state.memento, Some("/opt/homebrew/bin/agda"))

        // SIMULATE: But different endpoint is currently active (should be overridden by memento)
        let mockConnection = TestData.makeMockConnection("/usr/bin/agda", "2.6.4")
        Registry__Connection.status :=
          Active({
            connection: mockConnection,
            users: Belt.Set.String.empty,
            currentOwnerId: None,
            queue: Promise.resolve(),
          })

        // INVOKE: onActivate to trigger the actual UI logic
        await State__SwitchVersion.Handler.onActivate(state, makeMockPlatform())

        // ANALYZE: Check selection marking across all endpoints
        let allEndpointsFromLogs = loggedEvents->Array.flat

        // VERIFY: Exactly one endpoint should be selected
        let selectedEndpoints =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)
        Assert.deepStrictEqual(Array.length(selectedEndpoints), 1)

        // VERIFY: The correct endpoint (from memento) is selected
        switch selectedEndpoints[0] {
        | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/opt/homebrew/bin/agda") // Should be the memento selection
        | None => Assert.fail("Expected exactly one endpoint to be selected")
        }

        // VERIFY: All other endpoints are not selected
        let unselectedEndpoints =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => !isSelected)
        Assert.deepStrictEqual(Array.length(unselectedEndpoints), 3) // Should be 3 unselected endpoints

        // VERIFY: The unselected endpoints are the expected ones
        let unselectedPaths =
          unselectedEndpoints->Array.map(((path, _, _, _)) => path)->Array.toSorted(String.compare)
        let expectedUnselectedPaths =
          ["/usr/bin/agda", "/usr/bin/als", "/usr/local/bin/agda"]->Array.toSorted(String.compare)
        Assert.deepStrictEqual(unselectedPaths, expectedUnselectedPaths)

        // VERIFY: Total endpoint count is correct
        Assert.deepStrictEqual(Array.length(allEndpointsFromLogs), 4) // Should have all 4 endpoints logged
      },
    )

    Async.it(
      "should persist channel selection in memento across UI activations",
      async () => {
        // Channel selection MUST be stored in memento
        //
        // Test: call handleChannelSwitch (the real code path that runs after
        // user picks a channel in showQuickPick), then verify memento persistence.

        let state = createTestState()
        let platform = makeMockPlatform()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)

        // Precondition: memento has no channel entry
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        // Execute the real channel-switch code path
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state,
          platform,
          manager,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
        )

        // Verify the switch happened
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)

        // After channel switch, memento MUST store the exact channel label
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), Some("DevALS"))
      },
    )

    Async.it(
      "should preserve connection.paths when switching channels",
      async () => {
        // Switching channels MUST NOT remove existing downloaded Candidates
        //
        // Test: set up config paths, call handleChannelSwitch (the real code path),
        // then verify paths are unchanged.

        let state = createTestState()
        let platform = makeMockPlatform()
        let logChannel = state.channels.log
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)
        let view = State__SwitchVersion.View.make(logChannel)

        // Set up config with existing paths including downloaded ALS from different channels
        let existingPaths = ["/usr/bin/agda", "/downloaded/als-hardcoded", "/downloaded/als-dev"]
        await Config.Connection.setAgdaPaths(logChannel, existingPaths)

        // Build a real updateUI closure (same as onActivate creates)
        let updateUI = async (downloadItems: array<(bool, string, string)>): unit => {
          let downloadHeader =
            "Download (channel: " ++
              State__SwitchVersion.Download.channelToLabel(selectedChannel.contents) ++ ")"
          let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
            manager,
            downloadItems,
            ~downloadHeader,
            ~showChannelSelector=true,
          )
          let items = State__SwitchVersion.Item.fromItemDataArray(itemData, state.extensionUri)
          view->State__SwitchVersion.View.updateItems(items)
        }

        // Execute the real channel-switch code path
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state,
          platform,
          manager,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          updateUI,
        )

        // Verify the switch happened
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)

        // Paths MUST be unchanged after channel switch
        Assert.deepStrictEqual(
          Config.Connection.getAgdaPaths(),
          existingPaths,
        )

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should default to Hardcoded channel on fresh activation",
      async () => {
        // Hardcoded MUST be the default channel
        //
        // On fresh activation (no memento data), the restored channel should be Hardcoded.
        // This tests the logic in onActivate lines 1020-1029.

        let state = createTestState()

        // Precondition: memento has no selectedChannel entry (fresh install)
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        // Simulate the channel restoration logic from onActivate
        let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
        | Some(label) =>
          switch State__SwitchVersion.Download.channelFromLabel(label) {
          | Some(channel) => channel
          | None => Connection__Download.Channel.Hardcoded
          }
        | None => Connection__Download.Channel.Hardcoded
        }

        Assert.deepStrictEqual(restoredChannel, Connection__Download.Channel.Hardcoded)
      },
    )

    Async.it(
      "should default to Hardcoded channel when memento contains invalid channel label",
      async () => {
        let state = createTestState()

        // Set an invalid channel label in memento
        await Memento.SelectedChannel.set(state.memento, "InvalidChannel")
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), Some("InvalidChannel"))

        // Simulate the channel restoration logic from onActivate
        let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
        | Some(label) =>
          switch State__SwitchVersion.Download.channelFromLabel(label) {
          | Some(channel) => channel
          | None => Connection__Download.Channel.Hardcoded
          }
        | None => Connection__Download.Channel.Hardcoded
        }

        Assert.deepStrictEqual(restoredChannel, Connection__Download.Channel.Hardcoded)
      },
    )

    Async.it(
      "should restore a valid persisted channel on activation",
      async () => {
        // Round-trip: set channel in memento -> restore -> verify selected channel
        let state = createTestState()

        // Persist DevALS channel in memento (simulating a previous session)
        await Memento.SelectedChannel.set(
          state.memento,
          State__SwitchVersion.Download.channelToLabel(Connection__Download.Channel.DevALS),
        )

        // Simulate the channel restoration logic from onActivate (lines 1021-1028)
        let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
        | Some(label) =>
          switch State__SwitchVersion.Download.channelFromLabel(label) {
          | Some(channel) => channel
          | None => Connection__Download.Channel.Hardcoded
          }
        | None => Connection__Download.Channel.Hardcoded
        }

        Assert.deepStrictEqual(restoredChannel, Connection__Download.Channel.DevALS)
      },
    )

    Async.it(
      "should NOT clear PreferredCandidate when Delete Downloads is invoked and picked path is under download directory",
      async () => {
        // Delete Downloads MUST remove download paths from connection.paths, MUST NOT modify PreferredCandidate

        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-t12-clear-picked-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let hardcodedDirUri = VSCode.Uri.joinPath(storageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(hardcodedDirUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        // Set PickedConnection to a path under download directory
        let downloadedPath =
          VSCode.Uri.joinPath(storageUri, ["hardcoded-als", "als"])->VSCode.Uri.fsPath
        await Memento.PickedConnection.set(state.memento, Some(downloadedPath))

        // Precondition: PickedConnection is set to a download path
        Assert.deepStrictEqual(
          Memento.PickedConnection.get(state.memento),
          Some(downloadedPath),
        )

        let selectedItem: VSCode.QuickPickItem.t = {
          label: State__SwitchVersion.Constants.deleteDownloads,
          description: "",
          detail: "",
        }

        await Config.Connection.setAgdaPaths(
          state.channels.log,
          ["/usr/bin/agda", downloadedPath],
        )

        let onSelectionCompleted = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await onSelectionCompleted

        // Delete Downloads MUST remove download paths from connection.paths, MUST NOT modify PreferredCandidate
        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(downloadedPath))
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["/usr/bin/agda"])

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should NOT clear PreferredCandidate when Delete Downloads is invoked and picked path is NOT under download directory",
      async () => {
        // Delete Downloads MUST remove download paths from connection.paths, MUST NOT modify PreferredCandidate

        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-t12-keep-picked-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let hardcodedDirUri = VSCode.Uri.joinPath(storageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(hardcodedDirUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        // Set PickedConnection to a user-managed path (NOT under download directory)
        let userPath = "/usr/local/bin/agda"
        await Memento.PickedConnection.set(state.memento, Some(userPath))

        // Precondition: PickedConnection is set to a user path
        Assert.deepStrictEqual(
          Memento.PickedConnection.get(state.memento),
          Some(userPath),
        )

        let downloadedPath =
          VSCode.Uri.joinPath(storageUri, ["hardcoded-als", "als"])->VSCode.Uri.fsPath
        await Config.Connection.setAgdaPaths(
          state.channels.log,
          [userPath, downloadedPath],
        )

        let selectedItem: VSCode.QuickPickItem.t = {
          label: State__SwitchVersion.Constants.deleteDownloads,
          description: "",
          detail: "",
        }

        let onSelectionCompleted = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await onSelectionCompleted

        // Delete Downloads MUST remove download paths from connection.paths, MUST NOT modify PreferredCandidate
        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(userPath))
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [userPath])

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should use selected channel for manual UI-triggered downloads (sourceForVariant)",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let view = State__SwitchVersion.View.make(state.channels.log)

        // Select DevALS channel via the real code path
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state,
          makeMockPlatform(),
          manager,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
        )

        // Verify channel was switched
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)

        // Now trigger a manual download — the source channel should be DevALS, not Hardcoded
        let wasmSource = State__SwitchVersion.Download.sourceForVariant(
          Connection__Download__Platform.MacOS_Arm,
          State__SwitchVersion.Download.WASM,
          ~channel=selectedChannel.contents,
        )

        let sourceChannel = switch wasmSource {
        | Some(Connection__Download.Source.FromURL(channel, _, _)) => Some(channel)
        | Some(Connection__Download.Source.FromGitHub(channel, _)) => Some(channel)
        | None => None
        }

        // Selected channel MUST apply to manual UI-triggered downloads
        Assert.deepStrictEqual(sourceChannel, Some(Connection__Download.Channel.DevALS))

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should use selected channel for manual UI-triggered downloads (handleDownload end-to-end)",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        // Track which channel the download descriptor uses
        let downloadedChannel = ref(None)
        let downloadedPath = State__SwitchVersion.Download.expectedPathForVariant(
          state.globalStorageUri,
          State__SwitchVersion.Download.WASM,
        )

        let platform: Platform.t = {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.DevALS =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.DevALS,
                    "https://example.invalid/dev-als.wasm",
                    "dev-als",
                  ),
                )
              | Connection__Download.Channel.Hardcoded =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.Hardcoded,
                    "https://example.invalid/hardcoded-als.wasm",
                    "hardcoded-als",
                  ),
                )
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, source) => {
              let channel = switch source {
              | Connection__Download.Source.FromURL(ch, _, _) => Some(ch)
              | Connection__Download.Source.FromGitHub(ch, _) => Some(ch)
              }
              downloadedChannel := channel
              Promise.resolve(Ok(downloadedPath))
            }
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          module(MockPlatform)
        }

        // Select DevALS channel
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state,
          platform,
          manager,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
        )

        // Trigger manual download via handleDownload with selected channel
        await State__SwitchVersion.Handler.handleDownload(
          state,
          platform,
          State__SwitchVersion.Download.WASM,
          false,
          "ALS vTest",
          ~channel=Connection__Download.Channel.DevALS,
        )

        // The download descriptor MUST use the selected channel (DevALS), not Hardcoded
        Assert.deepStrictEqual(downloadedChannel.contents, Some(Connection__Download.Channel.DevALS))
      },
    )

    Async.it(
      "should clamp restored channel to available channels when activating UI",
      async () => {
        // Set memento to LatestALS (not available on Desktop, which only has Hardcoded + DevALS)
        let state = createTestState()
        await Memento.SelectedChannel.set(
          state.memento,
          State__SwitchVersion.Download.channelToLabel(Connection__Download.Channel.LatestALS),
        )

        // Capture download headers logged by onActivate's updateUI
        let loggedHeaders = []

        // Register listener AND wait-promise BEFORE calling onActivate to avoid race
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Others(msg)) if String.startsWith(msg, "Download (channel:") =>
            loggedHeaders->Array.push(msg)
          | _ => ()
          }
        )
        let onShown = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(Others("QuickPick shown")) => true
            | _ => false
            },
        )

        // Call the real onActivate
        let platform = makeMockPlatform()
        await State__SwitchVersion.Handler.onActivate(state, platform)

        // Wait for the QuickPick to be shown (ensures initial updateUI has completed)
        await onShown

        // Get available channel labels
        let availableChannels = await State__SwitchVersion.Download.getAvailableChannels(platform)
        let _availableLabels = availableChannels->Array.map(
          State__SwitchVersion.Download.channelToLabel,
        )

        // The download header emitted by onActivate MUST reference an available channel
        // LatestALS is not available, so it MUST be clamped to Hardcoded
        Assert.ok(Array.length(loggedHeaders) > 0)
        let header = loggedHeaders[0]->Option.getExn
        Assert.deepStrictEqual(header, "Download (channel: Hardcoded)")
      },
    )

    Async.it(
      "should expose Dev/nightly channel in available channels on Desktop",
      async () => {
        let platform = makeMockPlatform()
        let channels = await State__SwitchVersion.Download.getAvailableChannels(platform)

        // Spec: Hardcoded + Dev/nightly MUST be supported
        Assert.ok(channels->Array.includes(Connection__Download.Channel.Hardcoded))

        // Spec requires Dev/nightly channel to be available
        Assert.ok(channels->Array.includes(Connection__Download.Channel.DevALS))
      },
    )
  })

  describe("ItemCreation", () => {
    describe(
      "fromItemData - Separator",
      () => {
        it(
          "should create separator item correctly",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = Separator("Installed")
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Installed")
            // Check if kind is set to Separator
            switch actual.kind {
            | Some(kind) => Assert.deepStrictEqual(kind, VSCode.QuickPickItemKind.Separator)
            | None => Assert.fail("Expected kind to be set to Separator")
            }
          },
        )
      },
    )

    describe(
      "fromItemData - DownloadAction",
      () => {
        it(
          "should create download item for not downloaded version",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = DownloadAction(
              false,
              "Agda v2.6.4 Language Server v1.2.3",
              "native",
            )
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(
              actual.label,
              "$(cloud-download)  Download Agda Language Server (native)",
            )
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "Agda v2.6.4 Language Server v1.2.3")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )

        it(
          "should create download item for already downloaded version",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = DownloadAction(
              true,
              "Agda v2.6.4 Language Server v1.2.3",
              "native",
            )
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(
              actual.label,
              "$(cloud-download)  Download Agda Language Server (native)",
            )
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "Downloaded and installed")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "Agda v2.6.4 Language Server v1.2.3")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )
      },
    )

    describe(
      "fromItemData - NoInstallations",
      () => {
        it(
          "should create no installations item correctly",
          () => {
            let itemData: State__SwitchVersion.ItemData.t = NoInstallations
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(info) No installations found")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "Try installing Agda or ALS first")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "No executable paths detected")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )
      },
    )

    describe(
      "fromItemData - Endpoint (Agda)",
      () => {
        it(
          "should create Agda item for non-selected version",
          () => {
            let entry = TestData.createMockEntry(Agda(Some("2.6.4")), ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/agda", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Agda v2.6.4")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "/usr/bin/agda")
            | None => Assert.fail("Expected detail to be set")
            }
            // iconPath is a complex object, just check it exists for Agda
            switch actual.iconPath {
            | Some(_) => Assert.ok(true)
            | None => Assert.fail("Expected iconPath to be set for Agda endpoint")
            }
          },
        )

        it(
          "should create Agda item for selected version",
          () => {
            let entry = TestData.createMockEntry(Agda(Some("2.6.4")), ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/agda", entry, true)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Agda v2.6.4")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "Selected")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "/usr/bin/agda")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )

        it(
          "should create Agda item with unknown version",
          () => {
            let entry = TestData.createMockEntry(Agda(None), ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/agda", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Agda (version unknown)")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "")
            | None => Assert.fail("Expected description to be set")
            }
          },
        )
      },
    )

    describe(
      "fromItemData - Endpoint (ALS)",
      () => {
        it(
          "should create ALS item for non-selected version",
          () => {
            let entry = TestData.createMockEntry(ALS(Some(("1.2.3", "2.6.4", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/als", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(squirrel)  ALS v1.2.3, Agda v2.6.4")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "/usr/bin/als")
            | None => Assert.fail("Expected detail to be set")
            }
            // ALS should not have iconPath
            switch actual.iconPath {
            | None => Assert.ok(true)
            | Some(_) => Assert.fail("Did not expect iconPath for ALS endpoint")
            }
          },
        )

        it(
          "should create ALS item for selected version",
          () => {
            let entry = TestData.createMockEntry(ALS(Some(("1.2.3", "2.6.4", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/als", entry, true)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(squirrel)  ALS v1.2.3, Agda v2.6.4")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "Selected")
            | None => Assert.fail("Expected description to be set")
            }
          },
        )

        it(
          "should create ALS item with unknown version",
          () => {
            let entry = TestData.createMockEntry(ALS(None), ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/usr/bin/als", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(squirrel)  ALS v(version unknown)")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "")
            | None => Assert.fail("Expected description to be set")
            }
          },
        )
      },
    )

    describe(
      "fromItemData - Endpoint (Error)",
      () => {
        it(
          "should create error item correctly",
          () => {
            let entry = TestData.createMockEntry(Unknown, ~error="Permission denied", ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/broken/path", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(error) path")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "Error: Permission denied")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "/broken/path")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )

        it(
          "should create unknown item correctly",
          () => {
            let entry = TestData.createMockEntry(Unknown, ())
            let itemData: State__SwitchVersion.ItemData.t = Endpoint("/mystery/path", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(question) path")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "Unknown executable")
            | None => Assert.fail("Expected description to be set")
            }
          },
        )
      },
    )

    describe(
      "shouldEndpointHaveIcon",
      () => {
        it(
          "should return true for Agda endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(Agda(Some("2.6.4"))),
              true,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(Agda(None)),
              true,
            )
          },
        )

        it(
          "should return false for ALS endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(
                ALS(Some(("4.0.0", "2.6.4", None))),
              ),
              false,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(ALS(None)),
              false,
            )
          },
        )

        it(
          "should return false for unknown endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldEndpointHaveIcon(Unknown),
              false,
            )
          },
        )
      },
    )
  })

  describe("Selection Logic", () => {
    describe(
      "Selection Parsing (inline logic)",
      () => {
        it(
          "should identify download ALS action by label",
          () => {
            // This tests the inline logic in Handler.onSelection
            // We verify the constants used for comparison
            Assert.strictEqual(
              State__SwitchVersion.Constants.downloadNativeALS,
              "$(cloud-download)  Download Agda Language Server (native)",
            )
          },
        )

        it(
          "should verify other UI constants",
          () => {
            Assert.strictEqual(State__SwitchVersion.Constants.agdaVersionPrefix, "Agda v")
            Assert.strictEqual(State__SwitchVersion.Constants.alsWithSquirrel, "$(squirrel)  ALS v")
            Assert.strictEqual(
              State__SwitchVersion.Constants.downloadedAndInstalled,
              "Downloaded and installed",
            )
          },
        )
      },
    )
  })

  // I20: Channel coexistence semantics are not modeled in download paths
  // Download path helpers are hardcoded to "hardcoded-als" and not channel-parameterized
  describe("Channel-parameterized download paths", () => {
    Async.it(
      "isDownloaded should not report Hardcoded download as downloaded for DevALS channel",
      async () => {
        // Create a storage directory with a Hardcoded native binary
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-i20-coexist-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let globalStorageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(globalStorageUri)
        let hardcodedDirUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(hardcodedDirUri)
        let alsPath = NodeJs.Path.join([VSCode.Uri.fsPath(hardcodedDirUri), "als"])
        NodeJs.Fs.writeFileSync(alsPath, NodeJs.Buffer.fromString("mock-native"))

        // Hardcoded native IS downloaded — isDownloaded should return true
        let hardcodedResult = await State__SwitchVersion.Download.isDownloaded(
          globalStorageUri,
          State__SwitchVersion.Download.Native,
        )
        Assert.deepStrictEqual(hardcodedResult, true)

        // Channel-aware check: DevALS native is NOT downloaded (no "dev-als/als" exists)
        // Connection__Download.alreadyDownloaded is channel-parameterized and correctly returns None
        let devALSResult = await Connection__Download.alreadyDownloaded(
          globalStorageUri,
          Connection__Download.Channel.DevALS,
        )
        Assert.deepStrictEqual(devALSResult, None)

        // isDownloaded with DevALS channel checks "dev-als" directory, not "hardcoded-als"
        let isDownloadedForDevALS = await State__SwitchVersion.Download.isDownloaded(
          globalStorageUri,
          State__SwitchVersion.Download.Native,
          ~channel=Connection__Download.Channel.DevALS,
        )
        // DevALS native is NOT downloaded (no "dev-als/als" exists)
        Assert.deepStrictEqual(isDownloadedForDevALS, false)

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )

    Async.it(
      "switching channel from Hardcoded to DevALS should change download item availability",
      async () => {
        // Create storage with only a Hardcoded native binary on disk
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-i20-ui-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let hardcodedDirUri = VSCode.Uri.joinPath(storageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(hardcodedDirUri)
        let alsPath = NodeJs.Path.join([VSCode.Uri.fsPath(hardcodedDirUri), "als"])
        NodeJs.Fs.writeFileSync(alsPath, NodeJs.Buffer.fromString("mock-native"))

        let channels = {
          State.inputMethod: Chan.make(),
          responseHandled: Chan.make(),
          commandHandled: Chan.make(),
          log: Chan.make(),
        }
        let mockEditor = %raw(`{ document: { fileName: "test.agda" } }`)
        let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))
        let state = State.make(
          "test-id",
          Mock.Platform.makeWithAgda(),
          channels,
          storageUri,
          mockExtensionUri,
          Memento.make(None),
          mockEditor,
          None,
        )

        let platform = Mock.Platform.makeWithAgda()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)

        // Capture download items passed to updateUI after each channel switch
        let capturedDownloadItems = ref([])
        let updateUI = async (downloadItems: array<(bool, string, string)>) => {
          capturedDownloadItems := downloadItems
        }

        // Step 1: Switch to Hardcoded — native should correctly show downloaded=true
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state,
          platform,
          manager,
          selectedChannel,
          Connection__Download.Channel.Hardcoded,
          updateUI,
        )
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.Hardcoded)
        let hardcodedFlags =
          capturedDownloadItems.contents->Array.map(((downloaded, _, _)) => downloaded)
        // Hardcoded native IS downloaded — this is correct
        Assert.deepStrictEqual(hardcodedFlags, [true, false])

        // Step 2: Switch to DevALS — native should now show downloaded=false
        // because no DevALS binary exists (only Hardcoded at "hardcoded-als/als")
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state,
          platform,
          manager,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          updateUI,
        )
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        let devALSFlags =
          capturedDownloadItems.contents->Array.map(((downloaded, _, _)) => downloaded)

        // Channel selection MUST affect downloaded-state rendering.
        // DevALS native is NOT downloaded, so flags should differ from Hardcoded.
        // Currently: both channels return [true, false] because isDownloaded always
        // checks "hardcoded-als" — the channel switch has no effect on the result.
        Assert.notDeepStrictEqual(devALSFlags, hardcodedFlags)

        let _ = await FS.deleteRecursive(storageUri)
      },
    )
  })
})
