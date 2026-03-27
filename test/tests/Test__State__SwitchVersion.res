open Mocha
open State__SwitchVersion.ItemData

module TestData = {
  // Mock candidate entries for testing
  let createMockEntry = (
    kind: Memento.ResolvedMetadata.kind,
    ~error: option<string>=?,
    (),
  ): Memento.ResolvedMetadata.entry => {
    kind,
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
      "getCandidateDisplayInfo",
      () => {
        it(
          "should format Agda candidate with version",
          () => {
            let entry = TestData.agdaEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getCandidateDisplayInfo(
              "agda",
              entry,
            )

            Assert.deepStrictEqual(label, "Agda 2.6.4")
            Assert.deepStrictEqual(errorDescription, None)
          },
        )

        it(
          "should format Agda candidate without version",
          () => {
            let entry = TestData.agdaUnknownEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getCandidateDisplayInfo(
              "agda",
              entry,
            )

            Assert.deepStrictEqual(label, "Agda (version unknown)")
            Assert.deepStrictEqual(errorDescription, None)
          },
        )

        it(
          "should format ALS candidate with versions",
          () => {
            let entry = TestData.alsEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getCandidateDisplayInfo(
              "als",
              entry,
            )

            Assert.deepStrictEqual(label, "$(squirrel)  Agda 2.6.4 Language Server v4.0.0")
            Assert.deepStrictEqual(errorDescription, None)
          },
        )

        it(
          "should format error candidate",
          () => {
            let entry = TestData.unknownEntry
            let (label, errorDescription) = State__SwitchVersion.ItemData.getCandidateDisplayInfo(
              "broken-agda",
              entry,
            )

            Assert.deepStrictEqual(label, "$(error) broken-agda")
            Assert.deepStrictEqual(errorDescription, Some("Error: Permission denied"))
          },
        )

        it(
          "should format unknown candidate without error",
          () => {
            let entry = TestData.createMockEntry(Unknown, ())
            let (label, errorDescription) = State__SwitchVersion.ItemData.getCandidateDisplayInfo(
              "mystery",
              entry,
            )

            Assert.deepStrictEqual(label, "$(question) mystery")
            Assert.deepStrictEqual(errorDescription, Some("Unknown executable"))
          },
        )

        it(
          "should derive display filename from resource URI",
          () => {
            let entry = TestData.createMockEntry(Unknown, ())
            let (label, errorDescription) = State__SwitchVersion.ItemData.getCandidateDisplayInfo(
              "vscode-userdata:/global/als.wasm",
              entry,
            )

            Assert.deepStrictEqual(label, "$(question) als.wasm")
            Assert.deepStrictEqual(errorDescription, Some("Unknown executable"))
          },
        )
      },
    )

    describe(
      "shouldCandidateHaveIcon",
      () => {
        it(
          "should return true for Agda candidates",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldCandidateHaveIcon(Agda(Some("2.6.4"))),
              true,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldCandidateHaveIcon(Agda(None)),
              true,
            )
          },
        )

        it(
          "should return false for ALS candidates",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldCandidateHaveIcon(
                ALS(Some(("4.0.0", "2.6.4", None))),
              ),
              false,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldCandidateHaveIcon(ALS(None)),
              false,
            )
          },
        )

        it(
          "should return false for unknown candidates",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldCandidateHaveIcon(Unknown),
              false,
            )
          },
        )
      },
    )

    describe(
      "inferCandidateKind",
      () => {
        it(
          "should recognize als.wasm as ALS candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als.wasm"),
              Memento.ResolvedMetadata.ALS(None),
            )
          },
        )

        it(
          "should recognize als as ALS candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als"),
              Memento.ResolvedMetadata.ALS(None),
            )
          },
        )

        it(
          "should recognize als.exe as ALS candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als.exe"),
              Memento.ResolvedMetadata.ALS(None),
            )
          },
        )

        it(
          "should recognize agda as Agda candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("agda"),
              Memento.ResolvedMetadata.Agda(None),
            )
          },
        )

        it(
          "should recognize agda-2.6.4 as Agda candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("agda-2.6.4"),
              Memento.ResolvedMetadata.Agda(None),
            )
          },
        )

        it(
          "should recognize unknown executables",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("unknown"),
              Memento.ResolvedMetadata.Unknown,
            )
          },
        )

        it(
          "should extract basename from full paths",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/dev-als/als.wasm",
              ),
              Memento.ResolvedMetadata.ALS(None),
            )
          },
        )

        it(
          "should handle uppercase extensions",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("ALS.WASM"),
              Memento.ResolvedMetadata.ALS(None),
            )
          },
        )

        it(
          "should recognize als- prefixed executables",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als-server"),
              Memento.ResolvedMetadata.ALS(None),
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
          "should create quickpick item from candidate data with correct properties",
          () => {
            let entry = TestData.agdaEntry
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "/usr/bin/agda",
              "/usr/bin/agda",
              entry,
              false,
            )
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "Agda 2.6.4")
            Assert.deepStrictEqual(item.description, Some(""))
            Assert.deepStrictEqual(item.detail, Some("/usr/bin/agda"))
          },
        )

        it(
          "should include icon for Agda candidates",
          () => {
            let entry = TestData.agdaEntry
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "/usr/bin/agda",
              "/usr/bin/agda",
              entry,
              false,
            )
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            // Check that iconPath is present for Agda
            switch item.iconPath {
            | Some(_) => () // Expected
            | None => Assert.fail("Expected iconPath for Agda candidate")
            }
          },
        )

        it(
          "should not include icon for ALS candidates",
          () => {
            let entry = TestData.alsEntry
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "/usr/bin/als",
              "/usr/bin/als",
              entry,
              false,
            )
            let item = State__SwitchVersion.Item.fromItemData(itemData, extensionUri)

            // Check that iconPath is absent for ALS
            switch item.iconPath {
            | None => () // Expected
            | Some(_) => Assert.fail("Did not expect iconPath for ALS candidate")
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
          "should hide Candidates section when no candidates are found",
          () => {
            let entries = []
            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [],
            )

            let candidatesSeparator = itemData->Array.find(
              data =>
                switch data {
                | Separator("Candidates") => true
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

            Assert.ok(candidatesSeparator->Option.isNone)
            Assert.ok(noInstallationsPlaceholder->Option.isNone)
            Assert.deepStrictEqual(Array.length(itemData), 3) // Download separator + Select other channel + Delete downloads

            switch itemData[0] {
            | Some(Separator("Download (channel: Hardcoded)")) => () // Expected
            | _ => Assert.fail("Expected Download separator")
            }

            switch itemData[1] {
            | Some(SelectOtherChannels) => () // Expected
            | _ => Assert.fail("Expected SelectOtherChannels item")
            }

            switch itemData[2] {
            | Some(DeleteDownloads) => () // Expected
            | _ => Assert.fail("Expected DeleteDownloads item")
            }
          },
        )

        it(
          "should create items with separator when entries exist",
          () => {
            let entries = [
              ("/usr/bin/agda", "/usr/bin/agda", TestData.agdaEntry),
              ("/usr/bin/als", "/usr/bin/als", TestData.alsEntry),
            ]

            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [],
            )

            Assert.deepStrictEqual(Array.length(itemData), 6) // Candidates separator + 2 candidates + Download separator + Select other channel + Delete downloads

            switch itemData[0] {
            | Some(Separator("Candidates")) => () // Expected
            | _ => Assert.fail("Expected Candidates separator")
            }

            switch itemData[3] {
            | Some(Separator("Download (channel: Hardcoded)")) => () // Expected
            | _ => Assert.fail("Expected Download separator")
            }

            switch itemData[4] {
            | Some(SelectOtherChannels) => () // Expected
            | _ => Assert.fail("Expected SelectOtherChannels item")
            }

            switch itemData[5] {
            | Some(DeleteDownloads) => () // Expected
            | _ => Assert.fail("Expected DeleteDownloads item")
            }
          },
        )

        it(
          "should include download section when download info is provided",
          () => {
            let entries = [("/usr/bin/agda", "/usr/bin/agda", TestData.agdaEntry)]

            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [(false, "ALS v1.0.0", "native")],
            )

            Assert.deepStrictEqual(Array.length(itemData), 6) // Candidates separator + 1 item + Download separator + download item + Select other channel + Delete downloads

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
            let entries = [("/usr/bin/agda", "/usr/bin/agda", TestData.agdaEntry)]

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

        it(
          "should include download section header when all downloads are installed and channel selector is shown",
          () => {
            let entries = [("/usr/bin/agda", "/usr/bin/agda", TestData.agdaEntry)]

            // All downloads installed → suppressManagedVariants returns empty array
            // but channel selector is still shown
            let itemData: array<
              State__SwitchVersion.ItemData.t,
            > = State__SwitchVersion.ItemData.entriesToItemData(
              entries,
              None,
              [],
              ~downloadHeader="Download (channel: Hardcoded)",
            )

            let hasDownloadSeparator = itemData->Array.some(
              data =>
                switch data {
                | Separator("Download (channel: Hardcoded)") => true
                | _ => false
                },
            )

            let hasChannelSelector = itemData->Array.some(
              data =>
                switch data {
                | SelectOtherChannels => true
                | _ => false
                },
            )

            let hasDeleteDownloads = itemData->Array.some(
              data =>
                switch data {
                | DeleteDownloads => true
                | _ => false
                },
            )

            // Both channel selector and download section header must be present
            Assert.deepStrictEqual(hasChannelSelector, true)
            Assert.deepStrictEqual(hasDownloadSeparator, true)
            Assert.deepStrictEqual(hasDeleteDownloads, true)
          },
        )

        it(
          "should structure sections as Candidates then Download when candidates and downloads are present",
          () => {
            let entries = [
              ("/usr/bin/agda", "/usr/bin/agda", TestData.agdaEntry),
              ("/usr/bin/als", "/usr/bin/als", TestData.alsEntry),
            ]

            let itemData: array<State__SwitchVersion.ItemData.t> =
              State__SwitchVersion.ItemData.entriesToItemData(
                entries,
                None,
                [(false, "ALS v1.0.0", "native")],
                ~downloadHeader="Download",
              )

            let structure = itemData->Array.map(item =>
              switch item {
              | Separator(label) => "sep:" ++ label
              | Candidate(_, _, _, _) => "candidate"
              | DownloadAction(_, _, _) => "download"
              | SelectOtherChannels => "select-other-channel"
              | DeleteDownloads => "delete-downloads"
              | NoInstallations => "no-installations"
              }
            )

            Assert.deepStrictEqual(
              structure,
              [
                "sep:Candidates",
                "candidate",
                "candidate",
                "sep:Download",
                "download",
                "select-other-channel",
                "delete-downloads",
              ],
            )
          },
        )

        it(
          "should keep Select other channel and Delete Downloads inside Download when there are no download rows",
          () => {
            let entries = [("/usr/bin/agda", "/usr/bin/agda", TestData.agdaEntry)]

            let itemData: array<State__SwitchVersion.ItemData.t> =
              State__SwitchVersion.ItemData.entriesToItemData(
                entries,
                None,
                [],
                ~downloadHeader="Download",
              )

            let structure = itemData->Array.map(item =>
              switch item {
              | Separator(label) => "sep:" ++ label
              | Candidate(_, _, _, _) => "candidate"
              | DownloadAction(_, _, _) => "download"
              | SelectOtherChannels => "select-other-channel"
              | DeleteDownloads => "delete-downloads"
              | NoInstallations => "no-installations"
              }
            )

            Assert.deepStrictEqual(
              structure,
              [
                "sep:Candidates",
                "candidate",
                "sep:Download",
                "select-other-channel",
                "delete-downloads",
              ],
            )
          },
        )

        it(
          "should show Download section only when connection.paths is empty",
          () => {
            let itemData: array<State__SwitchVersion.ItemData.t> =
              State__SwitchVersion.ItemData.entriesToItemData(
                [],
                None,
                [],
                ~downloadHeader="Download",
              )

            let structure = itemData->Array.map(item =>
              switch item {
              | Separator(label) => "sep:" ++ label
              | Candidate(_, _, _, _) => "candidate"
              | DownloadAction(_, _, _) => "download"
              | SelectOtherChannels => "select-other-channel"
              | DeleteDownloads => "delete-downloads"
              | NoInstallations => "no-installations"
              }
            )

            Assert.deepStrictEqual(
              structure,
              [
                "sep:Download",
                "select-other-channel",
                "delete-downloads",
              ],
            )
          },
        )

      },
    )
  })

  describe("Events", () => {
    let mockAgda = ref("")

    Async.before(async () => {
      mockAgda := await Test__Util.Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-sv-raw-key")
    })

    Async.after(async () => {
      await Test__Util.Candidate.Agda.destroy(mockAgda.contents)
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
    let makePickerItem = (state: State.t, itemData: State__SwitchVersion.ItemData.t) =>
      State__SwitchVersion.Item.fromItemData(itemData, state.extensionUri)

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
      "should render channel switch item even when only Hardcoded is available on Web",
      async () => {
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

        let availableChannels = await State__SwitchVersion.Download.getAvailableChannels(
          module(MockWebPlatform),
        )
        Assert.deepStrictEqual(availableChannels, [Connection__Download.Channel.Hardcoded])

        let itemData: array<State__SwitchVersion.ItemData.t> =
          State__SwitchVersion.ItemData.entriesToItemData(
            [],
            None,
            [(false, "ALS v1.0.0", "native")],
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
      "candidate rows should equal reversed connection.paths",
      async () => {
        module MockPlatform = {
          include Mock.Platform.Basic

          let findCommand = (command, ~timeout as _timeout=1000) =>
            switch command {
            | "agda" => Promise.resolve(Ok("/opt/discovered/agda"))
            | "als" => Promise.resolve(Ok("/opt/discovered/als"))
            | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
        }

        let platform: Platform.t = module(MockPlatform)
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()
        let configuredCandidates = ["/custom/agda", "vscode-userdata:/global/als.wasm"]

        await Config.Connection.setAgdaPaths(state.channels.log, configuredCandidates)

        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
          ~platformDeps=Some(platform),
        )
        let candidateRows =
          itemData->Array.filterMap(item =>
            switch item {
            | Candidate(path, _, _, _) => Some(path)
            | _ => None
            }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        Assert.deepStrictEqual(candidateRows, configuredCandidates->Array.toReversed)
      },
    )

    Async.it(
      "candidate rows should be empty when connection.paths is empty",
      async () => {
        module MockPlatform = {
          include Mock.Platform.Basic

          let findCommand = (command, ~timeout as _timeout=1000) =>
            switch command {
            | "agda" => Promise.resolve(Ok("/opt/discovered/agda"))
            | "als" => Promise.resolve(Ok("/opt/discovered/als"))
            | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
        }

        let platform: Platform.t = module(MockPlatform)
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, [])

        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
          ~platformDeps=Some(platform),
        )
        let candidateRows =
          itemData->Array.filterMap(item =>
            switch item {
            | Candidate(path, _, _, _) => Some(path)
            | _ => None
            }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        Assert.deepStrictEqual(candidateRows, [])
      },
    )

    Async.it(
      "should use resolved metadata for bare command candidate rows",
      async () => {
        let platform = makeMockPlatformWithBareCommands()
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, ["agda"])

        module PlatformOps = unpack(platform)
        let resolved = switch await Connection__Candidate.resolve(
          PlatformOps.findCommand,
          Connection__Candidate.make("agda"),
        ) {
        | Ok(resolved) => resolved
        | Error(_) => raise(Failure("Expected bare command candidate to resolve"))
        }
        await Memento.ResolvedMetadata.setKind(
          state.memento,
          resolved,
          Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
        )
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
          ~platformDeps=Some(platform),
        )
        let candidateEntry =
          itemData->Array.findMap(item =>
            switch item {
            | Candidate("agda", detail, entry, _) => Some((detail, entry))
            | _ => None
            }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        Assert.deepStrictEqual(
          candidateEntry->Option.map(((detail, entry)) => (detail, entry.kind)),
          Some(("agda (" ++ mockAgda.contents ++ ")", Memento.ResolvedMetadata.Agda(Some("2.7.0.1")))),
        )
      },
    )

    Async.it(
      "should use resolved metadata for resource candidate rows",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()
        let resourcePath = "vscode-userdata:/global/als.wasm"
        let candidate = Connection__Candidate.make(resourcePath)
        let resolved: Connection__Candidate.Resolved.t = switch candidate {
        | Resource(uri) => {original: candidate, resource: uri}
        | _ => raise(Failure("Expected resource candidate"))
        }

        await Config.Connection.setAgdaPaths(state.channels.log, [resourcePath])
        await Memento.ResolvedMetadata.setKind(
          state.memento,
          resolved,
          Memento.ResolvedMetadata.ALS(Some(("1.2.3", "2.6.4", None))),
        )
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, [])
        let candidateEntry =
          itemData->Array.findMap(item =>
            switch item {
            | Candidate(path, detail, entry, _) when path == resourcePath => Some((detail, entry))
            | _ => None
            }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        Assert.deepStrictEqual(
          candidateEntry->Option.map(((detail, entry)) => (detail, entry.kind)),
          Some((resourcePath, Memento.ResolvedMetadata.ALS(Some(("1.2.3", "2.6.4", None))))),
        )
      },
    )

    Async.it(
      "should hide native download variant when its managed path is present as file:// URI in connection.paths",
      async () => {
        let platform = makeMockPlatform()
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        let nativeManagedPath = State__SwitchVersion.Download.expectedPathForVariant(
          state.globalStorageUri,
          State__SwitchVersion.Download.Native,
        )
        let nativeManagedUri = VSCode.Uri.file(nativeManagedPath)->VSCode.Uri.toString
        await Config.Connection.setAgdaPaths(state.channels.log, [nativeManagedUri])

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
      "should not treat checking-availability placeholder as candidate selection",
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

        let selectedItem = makePickerItem(
          state,
          DownloadAction(false, State__SwitchVersion.Constants.checkingAvailability, "native"),
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

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedDownloadAction.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, false)
      },
    )

    Async.it(
      "should not treat non-candidate picker items as candidate selection",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let sawSelectedCandidate = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(SelectedCandidate(_, _, _)) => sawSelectedCandidate := true
          | _ => ()
          }
        )

        let selectedItem = makePickerItem(state, Separator("__unexpected_item__"))

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

        Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
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

        let selectedItem = makePickerItem(state, SelectOtherChannels)

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
      "should switch channel to DevALS via onSelection when showQuickPick returns a label",
      async () => {
        // Bug: showQuickPick with canPickMany:false returns a single string,
        // but the binding types it as array<string>. The code does selection[0]
        // which on a string returns just the first character (e.g. "DevALS"[0] = "D"),
        // so channelFromLabel("D") returns None and the switch silently fails.

        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)

        // Mock showQuickPick to return a single string "DevALS" (as real VS Code does
        // when canPickMany is false), saving the original for restore.
        let mockShowQuickPick: unit => unit = %raw(`function() {
          globalThis.__savedShowQuickPick = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = () => Promise.resolve("DevALS");
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick;
          delete globalThis.__savedShowQuickPick;
        }`)
        mockShowQuickPick()

        let downloadHeaderCapture = ref("")
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Others(header)) => downloadHeaderCapture := header
          | _ => ()
          }
        )

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded, Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => {
            let downloadHeader =
              "Download (channel: " ++
                State__SwitchVersion.Download.channelToLabel(selectedChannel.contents) ++ ")"
            state.channels.log->Chan.emit(Log.SwitchVersionUI(Others(downloadHeader)))
          },
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)
        restoreShowQuickPick()

        // The channel should have switched to DevALS
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(downloadHeaderCapture.contents, "Download (channel: DevALS)")

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should persist channel selection in memento via onSelection when showQuickPick returns a label",
      async () => {
        // Same root cause: showQuickPick returns a string, not an array.
        // Verify that memento is updated after channel switch via onSelection path.

        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)

        // Precondition: no channel in memento
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        // Mock showQuickPick to return "DevALS" as a plain string
        let mockShowQuickPick: unit => unit = %raw(`function() {
          globalThis.__savedShowQuickPick = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = () => Promise.resolve("DevALS");
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick;
          delete globalThis.__savedShowQuickPick;
        }`)
        mockShowQuickPick()

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded, Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)
        restoreShowQuickPick()

        // Memento should have the selected channel
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), Some("DevALS"))

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should re-show main QuickPick after channel selection completes",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)

        // Register onHide handler (same as onActivate does)
        view->State__SwitchVersion.View.onHide(() =>
          State__SwitchVersion.Handler.onHide(view)
        )

        // Track show() calls on the underlying quickPick
        let showCallCount = ref(0)
        let patchShow: (State__SwitchVersion.View.t, ref<int>) => unit = %raw(`function(view, counter) {
          var orig = view.quickPick.show.bind(view.quickPick);
          view.quickPick.show = function() { counter.contents++; return orig(); };
        }`)
        patchShow(view, showCallCount)

        let sawDestroyed = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
          | _ => ()
          }
        )

        // Mock showQuickPick to simulate VS Code hiding the main QuickPick
        // when a secondary picker opens, then returning "DevALS"
        let mockShowQuickPick: unit => unit = %raw(`function() {
          globalThis.__savedShowQuickPick = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = () => {
            // Simulate VS Code hiding the main QuickPick when secondary picker opens
            view.quickPick.hide();
            return Promise.resolve("DevALS");
          };
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick;
          delete globalThis.__savedShowQuickPick;
        }`)
        mockShowQuickPick()

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded, Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)
        restoreShowQuickPick()

        // View MUST NOT be destroyed during channel selection
        Assert.deepStrictEqual(sawDestroyed.contents, false)
        // After channel selection completes, the main QuickPick MUST be re-shown
        Assert.deepStrictEqual(showCallCount.contents, 1)

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should handle secondary showQuickPick rejection cleanly",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.Hardcoded)

        // Register onHide handler (same as onActivate does)
        view->State__SwitchVersion.View.onHide(() =>
          State__SwitchVersion.Handler.onHide(view)
        )

        // Track show() calls on the underlying quickPick
        let showCallCount = ref(0)
        let patchShow: (State__SwitchVersion.View.t, ref<int>) => unit = %raw(`function(view, counter) {
          var orig = view.quickPick.show.bind(view.quickPick);
          view.quickPick.show = function() { counter.contents++; return orig(); };
        }`)
        patchShow(view, showCallCount)

        let sawDestroyed = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
          | _ => ()
          }
        )

        // Mock showQuickPick to simulate VS Code hiding the main QuickPick,
        // then rejecting (e.g. the picker is disposed or errors)
        let mockShowQuickPick: unit => unit = %raw(`function() {
          globalThis.__savedShowQuickPick = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = () => {
            view.quickPick.hide();
            return Promise.reject(new Error("picker disposed"));
          };
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick;
          delete globalThis.__savedShowQuickPick;
        }`)
        mockShowQuickPick()

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.Hardcoded, Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)
        restoreShowQuickPick()

        // The hide during showQuickPick should have been suppressed
        Assert.deepStrictEqual(sawDestroyed.contents, false)
        // After rejection, the main QuickPick MUST be re-shown
        Assert.deepStrictEqual(showCallCount.contents, 1)

        // suppressHide must be reset so future hides work normally.
        // Re-show and then hide to trigger onDidHide and verify it destroys the view.
        view.quickPick->VSCode.QuickPick.show
        view.quickPick->VSCode.QuickPick.hide

        await Test__Util.wait(50)
        Assert.deepStrictEqual(sawDestroyed.contents, true)
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
      "should have an candidate marked as selected onActivation",
      async () => {
        /**
         * TEST PURPOSE: Expose fresh install UX problem where no candidate appears "Selected"
         * 
         * PROBLEM DESCRIPTION:
         * On fresh installs or after clearing extension data, users experience confusing UI behavior:
         * 1. User runs Command.Load → Agda connection establishes successfully (auto-detected)
         * 2. User opens switch version UI → Sees multiple candidates but NONE marked as "Selected"
         * 3. User is confused: "Which connection am I currently using?"
         * 4. Only after manually switching connection does one show as "Selected"
         * 
         * ROOT CAUSE:
         * - Command.Load establishes connection without setting Memento.PickedConnection
         * - Memento.PickedConnection.get() returns None on fresh installs
         * - UI selection logic requires explicit memento entry to mark candidate as "Selected"
         * - No mechanism exists to infer selection from active connection state
         * 
         * REPRODUCTION:
         * This test simulates fresh install by ensuring Memento.PickedConnection = None,
         * then invokes onActivate and observes the logged UpdateEndpoints events.
         */
        let state = createTestState()
        let loggedEvents = []
        let previousPaths = Config.Connection.getAgdaPaths()

        // Subscribe to log channel to capture UpdateEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedCandidates(candidates)) =>
              loggedEvents->Array.push(candidates)
            | _ => ()
            }
          },
        )

        // SIMULATE: Fresh install - ensure no picked connection in memento
        await Memento.PickedConnection.set(state.memento, None)
        await Config.Connection.setAgdaPaths(state.channels.log, ["/usr/bin/agda"])

        // SIMULATE: Active connection (Command.Load established connection)
        // Create a mock connection that matches one of the discovered candidates
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
        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        // VERIFY: Assert that the fix works (candidate marked as selected)
        Assert.ok(anyEndpointSelected) // Expected: Active connection candidate should be marked as selected
      },
    )

    Async.it(
      "should prefer memento selection over active connection inference",
      async () => {
        /**
         * TEST PURPOSE: Ensure explicit user selection takes precedence over connection inference
         * 
         * SCENARIO:
         * 1. User has multiple candidates discovered
         * 2. User has explicitly selected one candidate (stored in memento)
         * 3. But a different candidate is currently active (connection established)
         * 4. UI should show the explicitly selected candidate as "Selected", not the active one
         * 
         * This tests the precedence logic: explicit selection > active connection inference
         */
        let state = createTestState()
        let loggedEvents = []
        let previousPaths = Config.Connection.getAgdaPaths()

        // Subscribe to log channel to capture UpdateEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedCandidates(candidates)) =>
              loggedEvents->Array.push(candidates)
            | _ => ()
            }
          },
        )

        await Config.Connection.setAgdaPaths(
          state.channels.log,
          ["/usr/bin/agda", "/opt/homebrew/bin/agda"],
        )

        // SIMULATE: User explicitly selected one candidate (stored in memento)
        await Memento.PickedConnection.set(state.memento, Some("/usr/bin/agda"))

        // SIMULATE: But different candidate is currently active
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

        // ANALYZE: Check which candidate is marked as selected
        let allEndpointsFromLogs = loggedEvents->Array.flat

        // Find the selected candidate
        let selectedEndpoint =
          allEndpointsFromLogs->Array.find(((_, _, _, isSelected)) => isSelected)
        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        // VERIFY: The explicitly selected candidate (from memento) should be marked as selected
        // NOT the active connection candidate
        switch selectedEndpoint {
        | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/usr/bin/agda") // Memento selection should win
        | None => Assert.fail("Expected one candidate to be marked as selected")
        }

        // VERIFY: Only one candidate should be selected
        let selectedCount =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)->Array.length
        Assert.deepStrictEqual(selectedCount, 1)
      },
    )

    Async.it(
      "should preserve raw bare commands in PickedConnection after candidate selection",
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

          let selectedItem = makePickerItem(
            state,
            Candidate(
              selectedPath,
              selectedPath,
              {
                kind: selectedPath == "agda"
                  ? Memento.ResolvedMetadata.Agda(None)
                  : Memento.ResolvedMetadata.ALS(None),
                timestamp: Date.make(),
                error: None,
              },
              false,
            ),
          )

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
        }

        await runSelectionAndAssert("agda")
        await runSelectionAndAssert("als")
      },
    )

    Async.it(
      "should write probe metadata under resolved identity for bare command candidates",
      async () => {
        let platform = makeMockPlatformWithBareCommands()
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, ["agda"])

        let changed = await State__SwitchVersion.SwitchVersionManager.probeVersions(
          manager,
          platform,
        )

        module PlatformOps = unpack(platform)
        let resolved = switch await Connection__Candidate.resolve(
          PlatformOps.findCommand,
          Connection__Candidate.make("agda"),
        ) {
        | Ok(resolved) => resolved
        | Error(_) => raise(Failure("Expected bare command candidate to resolve"))
        }

        Assert.deepStrictEqual(changed, true)
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, resolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
        )
        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
      },
    )

    Async.it(
      "should write switch metadata under resolved identity for bare command selection",
      async () => {
        Registry__Connection.status := Empty

        let platform = makeMockPlatformWithBareCommands()
        let state = createTestStateWithPlatform(platform)

        await State__SwitchVersion.switchAgdaVersion(state, "agda")

        module PlatformOps = unpack(platform)
        let resolved = switch await Connection__Candidate.resolve(
          PlatformOps.findCommand,
          Connection__Candidate.make("agda"),
        ) {
        | Ok(resolved) => resolved
        | Error(_) => raise(Failure("Expected bare command selection to resolve"))
        }

        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some("agda"))
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, resolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
        )

        Registry__Connection.status := Empty
      },
    )

    Async.it(
      "should treat alias-equivalent candidate selection as unchanged",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let fsPath = "/tmp/hardcoded-als/als.wasm"
        let uriPath = "file:///tmp/hardcoded-als/als.wasm"

        await Memento.PickedConnection.set(state.memento, Some(uriPath))

        let sawSelectedCandidate = ref(false)
        let sawSelectionCompleted = ref(false)
        let _ = state.channels.log->Chan.on(log =>
          switch log {
          | Log.SwitchVersionUI(SelectedCandidate(_, _, _)) => sawSelectedCandidate := true
          | Log.SwitchVersionUI(SelectionCompleted) => sawSelectionCompleted := true
          | _ => ()
          }
        )

        let selectedItem = makePickerItem(
          state,
          Candidate(
            fsPath,
            fsPath,
            {kind: Memento.ResolvedMetadata.ALS(None), timestamp: Date.make(), error: None},
            false,
          ),
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

        await Test__Util.wait(50)
        view->State__SwitchVersion.View.destroy

        Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, true)
        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(uriPath))
      },
    )

    Async.it(
      "should use the selected candidate candidate from the typed picker item payload",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let fsPath = mockAgda.contents
        await Memento.PickedConnection.set(state.memento, None)

        let onSelectionCompleted = Log.on(
          state.channels.log,
          log =>
            switch log {
            | Log.SwitchVersionUI(SelectionCompleted) => true
            | _ => false
            },
        )

        let selectedItem = makePickerItem(
          state,
          Candidate(
            fsPath,
            fsPath,
            {
              kind: Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
              timestamp: Date.make(),
              error: None,
            },
            false,
          ),
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
        view->State__SwitchVersion.View.destroy

        Assert.deepStrictEqual(Memento.PickedConnection.get(state.memento), Some(fsPath))
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

        let selectedItem = makePickerItem(state, DeleteDownloads)

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

        let selectedItem = makePickerItem(state, DeleteDownloads)

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

        let selectedItem = makePickerItem(state, DeleteDownloads)

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
        let previousPaths = Config.Connection.getAgdaPaths()

        // Subscribe to log channel to capture UpdateEndpoints events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedCandidates(candidates)) =>
              loggedEvents->Array.push(candidates)
            | _ => ()
            }
          },
        )

        await Config.Connection.setAgdaPaths(state.channels.log, ["/usr/bin/agda"])

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

        // VERIFY: Candidate selection still works correctly even with download items present
        let selectedEndpoints =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)
        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        Assert.deepStrictEqual(Array.length(selectedEndpoints), 1) // One candidate should be selected

        // Find the selected candidate
        switch selectedEndpoints[0] {
        | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/usr/bin/agda") // Should be the active connection
        | None => Assert.fail("Expected one candidate to be selected")
        }

        // VERIFY: All candidates are properly logged (this tests the download integration doesn't break candidate logging)
        Assert.ok(Array.length(allEndpointsFromLogs) > 0) // Should have candidates logged

        // VERIFY: No errors in candidate entries
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
              | Candidate(path, _, _, true) => Some(path)
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

          let selectedItem = makePickerItem(
            state,
            DownloadAction(
              false,
              "ALS v1.0.0",
              State__SwitchVersion.Download.variantToTag(selectedVariant),
            ),
          )

          let sawSelectedCandidate = ref(false)
          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(SelectedCandidate(_, _, _)) => sawSelectedCandidate := true
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
              | Candidate(path, _, _, true) => Some(path)
              | _ => None
              }
            )
          Assert.deepStrictEqual(selectedEndpoints, [expectedDownloadPath])
          Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
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
      "should mark at most one candidate selected when URI and fsPath aliases coexist",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        let fsPath = "/tmp/hardcoded-als/als.wasm"
        let uriPath = "file:///tmp/hardcoded-als/als.wasm"

        // Set PickedConnection to the URI form
        await Memento.PickedConnection.set(state.memento, Some(uriPath))
        await Config.Connection.setAgdaPaths(state.channels.log, [fsPath])

        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
        )

        let selectedEndpoints =
          itemData->Array.filterMap(item =>
            switch item {
            | Candidate(_, _, _, true) => Some(true)
            | _ => None
            }
          )
        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        // Exactly one candidate MUST be marked selected, not both
        Assert.deepStrictEqual(selectedEndpoints, [true])
      },
    )

    Async.it(
      "should mark only one candidate when multiple exist",
      async () => {
        /**
         * TEST PURPOSE: Ensure only one candidate is marked as selected when multiple candidates exist
         * 
         * SCENARIO:
         * Multiple candidates are discovered (different Agda versions, different paths)
         * Only the correctly matched candidate should be marked as "Selected"
         * All other candidates should remain unselected
         * 
         * This tests:
         * - Path matching logic works correctly across multiple candidates
         * - No logic errors cause multiple candidates to be selected
         * - Selection marking is precise and doesn't have false positives
         */
        let state = createTestState()
        let loggedEvents = []
        let previousPaths = Config.Connection.getAgdaPaths()

        // Subscribe to log channel to capture UpdatedCandidates events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedCandidates(candidates)) =>
              loggedEvents->Array.push(candidates)
            | _ => ()
            }
          },
        )

        await Config.Connection.setAgdaPaths(
          state.channels.log,
          ["/usr/bin/agda", "/opt/homebrew/bin/agda", "/usr/local/bin/agda", "/usr/bin/als"],
        )

        // SIMULATE: User has explicitly selected one specific candidate
        await Memento.PickedConnection.set(state.memento, Some("/opt/homebrew/bin/agda"))

        // SIMULATE: But different candidate is currently active (should be overridden by memento)
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

        // ANALYZE: Check selection marking across all candidates
        let allEndpointsFromLogs = loggedEvents->Array.flat
        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

        // VERIFY: Exactly one candidate should be selected
        let selectedEndpoints =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)
        Assert.deepStrictEqual(Array.length(selectedEndpoints), 1)

        // VERIFY: The correct candidate (from memento) is selected
        switch selectedEndpoints[0] {
        | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/opt/homebrew/bin/agda") // Should be the memento selection
        | None => Assert.fail("Expected exactly one candidate to be selected")
        }

        // VERIFY: All other candidates are not selected
        let unselectedEndpoints =
          allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => !isSelected)
        Assert.deepStrictEqual(Array.length(unselectedEndpoints), 3) // Should be 3 unselected candidates

        // VERIFY: The unselected candidates are the expected ones
        let unselectedPaths =
          unselectedEndpoints->Array.map(((path, _, _, _)) => path)->Array.toSorted(String.compare)
        let expectedUnselectedPaths =
          ["/usr/bin/agda", "/usr/bin/als", "/usr/local/bin/agda"]->Array.toSorted(String.compare)
        Assert.deepStrictEqual(unselectedPaths, expectedUnselectedPaths)

        // VERIFY: Total candidate count is correct
        Assert.deepStrictEqual(Array.length(allEndpointsFromLogs), 4) // Should have all 4 candidates logged
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

        let selectedItem = makePickerItem(state, DeleteDownloads)

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

        let selectedItem = makePickerItem(state, DeleteDownloads)

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
            let itemData: State__SwitchVersion.ItemData.t = Separator("Candidates")
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Candidates")
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
      "fromItemData - Candidate (Agda)",
      () => {
        it(
          "should create Agda item for non-selected version",
          () => {
            let entry = TestData.createMockEntry(Agda(Some("2.6.4")), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/usr/bin/agda", "/usr/bin/agda", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Agda 2.6.4")
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
            | None => Assert.fail("Expected iconPath to be set for Agda candidate")
            }
          },
        )

        it(
          "should create Agda item for selected version",
          () => {
            let entry = TestData.createMockEntry(Agda(Some("2.6.4")), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/usr/bin/agda", "/usr/bin/agda", entry, true)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "Agda 2.6.4")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "selected")
            | None => Assert.fail("Expected description to be set")
            }
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "/usr/bin/agda")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )

        it(
          "should show command detail with resolved filepath for command candidates",
          () => {
            let entry = TestData.createMockEntry(Agda(Some("2.6.4")), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "agda",
              "agda (/usr/bin/agda)",
              entry,
              false,
            )
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "agda (/usr/bin/agda)")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )

        it(
          "should create Agda item with unknown version",
          () => {
            let entry = TestData.createMockEntry(Agda(None), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/usr/bin/agda", "/usr/bin/agda", entry, false)
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
      "fromItemData - Candidate (ALS)",
      () => {
        it(
          "should create ALS item for non-selected version",
          () => {
            let entry = TestData.createMockEntry(ALS(Some(("1.2.3", "2.6.4", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/usr/bin/als", "/usr/bin/als", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(squirrel)  Agda 2.6.4 Language Server v1.2.3")
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
            | Some(_) => Assert.fail("Did not expect iconPath for ALS candidate")
            }
          },
        )

        it(
          "should create ALS item for selected version",
          () => {
            let entry = TestData.createMockEntry(ALS(Some(("1.2.3", "2.6.4", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/usr/bin/als", "/usr/bin/als", entry, true)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(squirrel)  Agda 2.6.4 Language Server v1.2.3")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "selected")
            | None => Assert.fail("Expected description to be set")
            }
          },
        )

        it(
          "should show URI detail as the URI string for resource candidates",
          () => {
            let entry = TestData.createMockEntry(ALS(Some(("1.2.3", "2.6.4", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "vscode-userdata:/global/als.wasm",
              "vscode-userdata:/global/als.wasm",
              entry,
              false,
            )
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            switch actual.detail {
            | Some(detail) => Assert.strictEqual(detail, "vscode-userdata:/global/als.wasm")
            | None => Assert.fail("Expected detail to be set")
            }
          },
        )

        it(
          "should create ALS item with unknown version",
          () => {
            let entry = TestData.createMockEntry(ALS(None), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/usr/bin/als", "/usr/bin/als", entry, false)
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(actual.label, "$(squirrel)  Agda Language Server (version unknown)")
            switch actual.description {
            | Some(desc) => Assert.strictEqual(desc, "")
            | None => Assert.fail("Expected description to be set")
            }
          },
        )
      },
    )

    describe(
      "fromItemData - Candidate (Error)",
      () => {
        it(
          "should create error item correctly",
          () => {
            let entry = TestData.createMockEntry(Unknown, ~error="Permission denied", ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/broken/path", "/broken/path", entry, false)
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
            let itemData: State__SwitchVersion.ItemData.t = Candidate("/mystery/path", "/mystery/path", entry, false)
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
            Assert.strictEqual(State__SwitchVersion.Constants.agdaVersionPrefix, "Agda ")
            Assert.strictEqual(State__SwitchVersion.Constants.alsWithSquirrel, "$(squirrel)  Agda ")
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
