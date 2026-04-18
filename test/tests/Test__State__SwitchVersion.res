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
  let alsEntry = createMockEntry(ALS(Native, Some(("4.0.0", "2.6.4", None))), ())
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
                ALS(Native, Some(("4.0.0", "2.6.4", None))),
              ),
              false,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.ItemData.shouldCandidateHaveIcon(ALS(Native, None)),
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
              Memento.ResolvedMetadata.ALS(WASM, None),
            )
          },
        )

        it(
          "should recognize als as ALS candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als"),
              Memento.ResolvedMetadata.ALS(Native, None),
            )
          },
        )

        it(
          "should recognize als.exe as ALS candidate",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als.exe"),
              Memento.ResolvedMetadata.ALS(Native, None),
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
              Memento.ResolvedMetadata.ALS(WASM, None),
            )
          },
        )

        it(
          "should infer DevALS native metadata from source-specific cached path",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/dev-als/als-dev-Agda-2.8.0-macos-arm64/als",
              ),
              Memento.ResolvedMetadata.ALS(Native, Some(("dev", "2.8.0", None))),
            )
          },
        )

        it(
          "should infer DevALS WASM metadata from source-specific cached path",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/dev-als/als-dev-Agda-2.8.0-wasm/als.wasm",
              ),
              Memento.ResolvedMetadata.ALS(WASM, Some(("dev", "2.8.0", None))),
            )
          },
        )

        it(
          "should infer stable release native metadata from source-specific cached path",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/stable-als/als-v6-Agda-2.8.0-macos-arm64/als",
              ),
              Memento.ResolvedMetadata.ALS(Native, Some(("v6", "2.8.0", None))),
            )
          },
        )

        it(
          "should infer stable release WASM metadata from source-specific cached path",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/stable-als/als-v6-Agda-2.8.0-wasm/als.wasm",
              ),
              Memento.ResolvedMetadata.ALS(WASM, Some(("v6", "2.8.0", None))),
            )
          },
        )

        it(
          "should infer stable release metadata from release-based managed storage",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/globalStorage/releases/v6/als-v6-Agda-2.8.0-wasm/als.wasm",
              ),
              Memento.ResolvedMetadata.ALS(WASM, Some(("v6", "2.8.0", None))),
            )
          },
        )

        it(
          "should ignore malformed managed release artifact paths",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/globalStorage/releases/v6/als-v6-agda-2.8.0-wasm/als.wasm",
              ),
              Memento.ResolvedMetadata.Unknown,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind(
                "/path/to/globalStorage/releases/v6/als-v6-Agda-2.8.0-freebsd/als",
              ),
              Memento.ResolvedMetadata.Unknown,
            )
          },
        )

        it(
          "should handle uppercase extensions",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("ALS.WASM"),
              Memento.ResolvedMetadata.ALS(WASM, None),
            )
          },
        )

        it(
          "should recognize als- prefixed executables",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion.SwitchVersionManager.inferCandidateKind("als-server"),
              Memento.ResolvedMetadata.ALS(Native, None),
            )
          },
        )
      },
    )
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
            | Some(Separator("Download (Development)")) => () // Expected
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
            | Some(Separator("Download (Development)")) => () // Expected
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
                | Separator("Download (Development)") => true
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
                | Separator("Download (Development)") => true
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
              ~downloadHeader="Download (Development)",
            )

            let hasDownloadSeparator = itemData->Array.some(
              data =>
                switch data {
                | Separator("Download (Development)") => true
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
          Memento.ResolvedMetadata.ALS(Native, Some(("1.2.3", "2.6.4", None))),
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
          Some((resourcePath, Memento.ResolvedMetadata.ALS(Native, Some(("1.2.3", "2.6.4", None))))),
        )
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
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
      },
    )

    Async.it(
      "should handle secondary showQuickPick rejection cleanly",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)

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
         * - Command.Load establishes connection without setting Memento.PreferredCandidate
         * - Memento.PreferredCandidate.get() returns None on fresh installs
         * - UI selection logic requires explicit memento entry to mark candidate as "Selected"
         * - No mechanism exists to infer selection from active connection state
         * 
         * REPRODUCTION:
         * This test simulates fresh install by ensuring Memento.PreferredCandidate = None,
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

        // SIMULATE: Fresh install - ensure no preferred candidate in memento
        await Memento.PreferredCandidate.set(state.memento, None)
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
        await Memento.PreferredCandidate.set(state.memento, Some("/usr/bin/agda"))

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
      "should preserve raw bare commands in PreferredCandidate after candidate selection",
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
                  : Memento.ResolvedMetadata.ALS(Native, None),
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
            ref(Connection__Download.Channel.DevALS),
            _downloadInfo => Promise.resolve(),
            view,
            [selectedItem],
          )
          await onOperationComplete
          view->State__SwitchVersion.View.destroy

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(selectedPath))
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
      "should refine DevALS path-derived metadata from dev placeholder to probed ALS version",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-devals-refine-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devALSPath = NodeJs.Path.join([
          storagePath,
          "dev-als",
          "als-dev-Agda-2.8.0-macos-arm64",
          "als",
        ])
        let parentDir = NodeJs.Path.dirname(devALSPath)
        let content =
          if OS.onUnix {
            "#!/bin/sh\necho 'Agda v2.8.0 Language Server v1.2.3'\nexit 0"
          } else {
            "@echo Agda v2.8.0 Language Server v1.2.3"
          }

        let _ = await FS.createDirectory(VSCode.Uri.file(parentDir))
        NodeJs.Fs.writeFileSync(devALSPath, NodeJs.Buffer.fromString(content))
        if OS.onUnix {
          let _ = await NodeJs.Fs.chmod(devALSPath, ~mode=0o755)
        }

        let platform = makeMockPlatform()
        let storageUri = VSCode.Uri.file(storagePath)
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, [devALSPath])

        let preProbeItemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
          ~platformDeps=Some(platform),
        )
        let preProbeKind =
          preProbeItemData->Array.findMap(item =>
            switch item {
            | Candidate(path, _, entry, _) when path == devALSPath => Some(entry.kind)
            | _ => None
            }
          )

        let changed = await State__SwitchVersion.SwitchVersionManager.probeVersions(
          manager,
          platform,
        )

        let resolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(devALSPath),
          resource: VSCode.Uri.file(devALSPath),
        }

        Assert.deepStrictEqual(
          preProbeKind,
          Some(Memento.ResolvedMetadata.ALS(Native, Some(("dev", "2.8.0", None)))),
        )
        Assert.deepStrictEqual(changed, true)
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, resolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.ALS(Native, Some(("1.2.3", "2.8.0", None)))),
        )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(VSCode.Uri.file(storagePath))
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

        Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some("agda"))
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, resolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
        )

        Registry__Connection.status := Empty
      },
    )

    Async.it(
      "should show DevALS WASM downloaded candidate as dev build before probing",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-devals-wasm-label-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devALSWasmPath = NodeJs.Path.join([
          storagePath,
          "dev-als",
          "als-dev-Agda-2.8.0-wasm",
          "als.wasm",
        ])
        let parentDir = NodeJs.Path.dirname(devALSWasmPath)
        let platform = makeMockPlatform()
        let storageUri = VSCode.Uri.file(storagePath)
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        let _ = await FS.createDirectory(VSCode.Uri.file(parentDir))
        NodeJs.Fs.writeFileSync(devALSWasmPath, NodeJs.Buffer.fromString("wasm"))

        await Config.Connection.setAgdaPaths(state.channels.log, [devALSWasmPath])

        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          [],
          ~platformDeps=Some(platform),
        )
        let label =
          itemData->Array.findMap(item =>
            switch item {
            | Candidate(path, _, entry, _) when path == devALSWasmPath =>
              Some(
                State__SwitchVersion.Item.fromItemData(
                  Candidate(path, path, entry, false),
                  TestData.createMockExtensionUri(),
                ).label,
              )
            | _ => None
            }
          )

        Assert.deepStrictEqual(
          label,
          Some("$(squirrel)  Agda 2.8.0 Language Server (dev build) WASM"),
        )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(VSCode.Uri.file(storagePath))
      },
    )

    Async.it(
      "should preserve DevALS WASM path-derived metadata during background probing",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-devals-wasm-probe-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devALSWasmPath = NodeJs.Path.join([
          storagePath,
          "dev-als",
          "als-dev-Agda-2.8.0-wasm",
          "als.wasm",
        ])
        let parentDir = NodeJs.Path.dirname(devALSWasmPath)
        let platform = makeMockPlatform()
        let storageUri = VSCode.Uri.file(storagePath)
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()
        let resolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(devALSWasmPath),
          resource: VSCode.Uri.file(devALSWasmPath),
        }

        let _ = await FS.createDirectory(VSCode.Uri.file(parentDir))
        NodeJs.Fs.writeFileSync(devALSWasmPath, NodeJs.Buffer.fromString("wasm"))

        await Config.Connection.setAgdaPaths(state.channels.log, [devALSWasmPath])

        let changed = await State__SwitchVersion.SwitchVersionManager.probeVersions(
          manager,
          platform,
        )

        Assert.deepStrictEqual(changed, true)
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, resolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.ALS(WASM, Some(("dev", "2.8.0", None)))),
        )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(VSCode.Uri.file(storagePath))
      },
    )

    Async.it(
      "should treat alias-equivalent candidate selection as unchanged",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let fsPath = "/tmp/dev-als/als.wasm"
        let uriPath = "file:///tmp/dev-als/als.wasm"

        await Memento.PreferredCandidate.set(state.memento, Some(uriPath))

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
            {kind: Memento.ResolvedMetadata.ALS(Native, None), timestamp: Date.make(), error: None},
            false,
          ),
        )

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await Test__Util.wait(50)
        view->State__SwitchVersion.View.destroy

        Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, true)
        Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(uriPath))
      },
    )

    Async.it(
      "should use the selected candidate candidate from the typed picker item payload",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let fsPath = mockAgda.contents
        await Memento.PreferredCandidate.set(state.memento, None)

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
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await onSelectionCompleted
        view->State__SwitchVersion.View.destroy

        Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(fsPath))
      },
    )

    Async.it(
      "should not modify PreferredCandidate when user clicks download action",
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

          let expectedDownloadPath = switch selectedVariant {
          | State__SwitchVersion.Download.Native =>
            NodeJs.Path.join([
              VSCode.Uri.fsPath(state.globalStorageUri),
              "releases",
              "dev",
              "als-dev-Agda-2.8.0-macos-arm64",
              "als",
            ])
          | State__SwitchVersion.Download.WASM =>
            NodeJs.Path.join([
              VSCode.Uri.fsPath(state.globalStorageUri),
              "releases",
              "dev",
              "als-dev-Agda-2.8.0-wasm",
              "als.wasm",
            ])
          }

          let previouslyPicked = Some("/usr/bin/agda")
          await Memento.PreferredCandidate.set(state.memento, previouslyPicked)
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
            ref(Connection__Download.Channel.DevALS),
            _downloadItems => Promise.resolve(),
            view,
            [selectedItem],
          )

          await onOperationComplete

          // Manual UI download must not modify PreferredCandidate
          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), previouslyPicked)

          let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
            manager,
            await State__SwitchVersion.Download.getAllAvailableDownloads(
              state,
              Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath),
            ),
          )
          let selectedCandidates =
            itemData
            ->Array.filterMap(item =>
              switch item {
              | Candidate(path, _, _, true) => Some(path)
              | _ => None
              }
            )
          let currentPaths = Config.Connection.getAgdaPaths()
          let expectedSelectedCandidates =
            if currentPaths->Array.some(candidate => candidate == previouslyPicked->Option.getExn) {
              [previouslyPicked->Option.getExn]
            } else if currentPaths->Array.some(candidate => candidate == activePath) {
              [activePath]
            } else {
              []
            }
          Assert.deepStrictEqual(selectedCandidates, expectedSelectedCandidates)
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

        let fsPath = "/tmp/dev-als/als.wasm"
        let uriPath = "file:///tmp/dev-als/als.wasm"

        // Set PreferredCandidate to the URI form
        await Memento.PreferredCandidate.set(state.memento, Some(uriPath))
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
        await Memento.PreferredCandidate.set(state.memento, Some("/opt/homebrew/bin/agda"))

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
      "SwitchVersionManager.getItemData default header should be Download (Development)",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, [])

        let downloadSeparator = itemData->Array.find(data =>
          switch data {
          | Separator(label) => String.startsWith(label, "Download (")
          | _ => false
          }
        )

        switch downloadSeparator {
        | Some(Separator(label)) => Assert.deepStrictEqual(label, "Download (Development)")
        | _ => Assert.fail("Expected Download separator")
        }
      },
    )

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
              }

          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
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

    it(
      "should expose DevALS and LatestALS channels",
      () => {
        Assert.deepStrictEqual(Connection__Download.Channel.all, [
          Connection__Download.Channel.DevALS,
          Connection__Download.Channel.LatestALS,
        ])
      },
    )

    Async.it(
      "should render channel switch item when only unavailable downloads are present",
      async () => {
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
        let state = createTestStateWithPlatform(makeMockPlatform())
        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
          state,
          makeMockPlatform(),
        )

        let hasChannelTaggedItems =
          downloadItems->Array.some(((_, _, downloadType)) =>
            downloadType == "latest" || downloadType == "dev"
          )

        Assert.deepStrictEqual(hasChannelTaggedItems, false)
      },
    )

    // Helper: create a DevALS mock platform returning a specific release descriptor
    let makeDevALSPlatformWith = (assetName): Platform.t => {
      let makeDevAsset = (name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/dev/" ++ name,
        id: 0,
        node_id: "",
        name,
        label: Some(""),
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2024-01-01T00:00:00Z",
        updated_at: "2024-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/dev/" ++ name,
      }
      let devRelease: Connection__Download__GitHub.Release.t = {
        url: "", assets_url: "", upload_url: "", html_url: "",
        id: 1, node_id: "dev", tag_name: "dev", target_commitish: "main", name: "dev",
        draft: false, prerelease: true,
        created_at: "2024-01-01T00:00:00Z", published_at: "2024-01-01T00:00:00Z",
        assets: [makeDevAsset(assetName)], tarball_url: "", zipball_url: "", body: None,
      }
      let devDescriptor: Connection__Download__GitHub.DownloadDescriptor.t = {
        asset: makeDevAsset(assetName),
        release: devRelease,
        saveAsFileName: "dev-als",
      }
      module MockDevALS = {
        let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
        let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
        let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
        let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
          switch channel {
          | Connection__Download.Channel.DevALS =>
            Ok(Connection__Download.Source.FromGitHub(Connection__Download.Channel.DevALS, devDescriptor))
          | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          }
        )
        let download = (_globalStorageUri, _, ~trace as _=Connection__Download__Trace.noop) =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        let findCommand = (_command, ~timeout as _=1000) =>
          Promise.resolve(Error(Connection__Command.Error.NotFound))
      }
      module(MockDevALS)
    }

    Async.it(
      "should hide native download variant when its release-managed path is already in connection.paths",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-suppress-hide-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let platform = makeDevALSPlatformWith("als-dev-Agda-2.8.0-macos-arm64.zip")
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        // Release-managed native path: releases/dev/als-dev-Agda-2.8.0-macos-arm64/als
        let nativePath = VSCode.Uri.fsPath(
          VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
        )
        let nativeDir = NodeJs.Path.dirname(nativePath)
        await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(nativePath, NodeJs.Buffer.fromString(""))
        await Config.Connection.setAgdaPaths(state.channels.log, [nativePath])

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, downloadItems)

        let hasNativeDownloadAction =
          itemData->Array.some(item =>
            switch item { | DownloadAction(_, _, "native") => true | _ => false }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(hasNativeDownloadAction, false)
      },
    )

    Async.it(
      "should hide native download variant when its release-managed path is present as file:// URI in connection.paths",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-suppress-uri-hide-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let platform = makeDevALSPlatformWith("als-dev-Agda-2.8.0-macos-arm64.zip")
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        let nativePath = VSCode.Uri.fsPath(
          VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
        )
        let nativeDir = NodeJs.Path.dirname(nativePath)
        await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(nativePath, NodeJs.Buffer.fromString(""))
        let nativeUri = VSCode.Uri.file(nativePath)->VSCode.Uri.toString
        await Config.Connection.setAgdaPaths(state.channels.log, [nativeUri])

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, downloadItems)

        let hasNativeDownloadAction =
          itemData->Array.some(item =>
            switch item { | DownloadAction(_, _, "native") => true | _ => false }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(hasNativeDownloadAction, false)
      },
    )

    Async.it(
      "should show native download variant when its release-managed path is in config but file no longer exists on disk",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-suppress-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let platform = makeDevALSPlatformWith("als-dev-Agda-2.8.0-macos-arm64.zip")
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        // Add release-managed native path to config, but do NOT create the file
        let nativePath = VSCode.Uri.fsPath(
          VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
        )
        await Config.Connection.setAgdaPaths(state.channels.log, [nativePath])
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(nativePath), false)

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, downloadItems)

        let hasNativeDownloadAction =
          itemData->Array.some(item =>
            switch item { | DownloadAction(_, _, "native") => true | _ => false }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(hasNativeDownloadAction, true)
      },
    )

    Async.it(
      "should show native download variant when its file:// URI alias is in config but file no longer exists on disk",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-suppress-uri-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let platform = makeDevALSPlatformWith("als-dev-Agda-2.8.0-macos-arm64.zip")
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        let nativePath = VSCode.Uri.fsPath(
          VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
        )
        let nativeUri = VSCode.Uri.file(nativePath)->VSCode.Uri.toString
        await Config.Connection.setAgdaPaths(state.channels.log, [nativeUri])
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(nativePath), false)

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, downloadItems)

        let hasNativeDownloadAction =
          itemData->Array.some(item =>
            switch item { | DownloadAction(_, _, "native") => true | _ => false }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(hasNativeDownloadAction, true)
      },
    )

    Async.it(
      "should show WASM download variant when its release-managed path is in config but file no longer exists on disk",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-suppress-wasm-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let platform = makeDevALSPlatformWith("als-dev-Agda-2.8.0-wasm.wasm")
        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let previousPaths = Config.Connection.getAgdaPaths()

        // WASM release-managed path: releases/dev/als-dev-Agda-2.8.0-wasm/als.wasm
        let wasmPath = VSCode.Uri.toString(
          VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm"]),
        )
        await Config.Connection.setAgdaPaths(state.channels.log, [wasmPath])
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(VSCode.Uri.fsPath(VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm"]))), false)

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform)
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(manager, downloadItems)

        let hasWasmDownloadAction =
          itemData->Array.some(item =>
            switch item { | DownloadAction(_, _, "wasm") => true | _ => false }
          )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(hasWasmDownloadAction, true)
      },
    )

    Async.it(
      "should show DevALS native download action when managed path is in config but file no longer exists on disk",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-suppress-devals-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let makeDevAsset = (name): Connection__Download__GitHub.Asset.t => {
          url: "https://github.com/agda/agda-language-server/releases/download/dev/" ++ name,
          id: 0,
          node_id: "",
          name,
          label: Some(""),
          content_type: "application/zip",
          state: "uploaded",
          size: 1000000,
          created_at: "2024-01-01T00:00:00Z",
          updated_at: "2024-01-01T00:00:00Z",
          browser_download_url: "https://github.com/agda/agda-language-server/releases/download/dev/" ++ name,
        }
        let devRelease: Connection__Download__GitHub.Release.t = {
          url: "",
          assets_url: "",
          upload_url: "",
          html_url: "",
          id: 1,
          node_id: "dev",
          tag_name: "dev",
          target_commitish: "main",
          name: "dev",
          draft: false,
          prerelease: true,
          created_at: "2024-01-01T00:00:00Z",
          published_at: "2024-01-01T00:00:00Z",
          assets: [makeDevAsset("als-dev-Agda-2.8.0-macos-arm64.zip")],
          tarball_url: "",
          zipball_url: "",
          body: None,
        }
        let devDescriptor: Connection__Download__GitHub.DownloadDescriptor.t = {
          asset: makeDevAsset("als-dev-Agda-2.8.0-macos-arm64.zip"),
          release: devRelease,
          saveAsFileName: "dev-als",
        }
        let platform: Platform.t = {
          module MockDevALS = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
            let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.DevALS =>
                Ok(
                  Connection__Download.Source.FromGitHub(
                    Connection__Download.Channel.DevALS,
                    devDescriptor,
                  ),
                )
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, _, ~trace as _trace=Connection__Download__Trace.noop) =>
              Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          module(MockDevALS)
        }

        let state = createTestStateWithPlatformAndStorage(platform, storageUri)
        let previousPaths = Config.Connection.getAgdaPaths()

        // Add the release-managed native path to config, but do NOT create the file
        let nativePath = VSCode.Uri.fsPath(
          VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
        )
        await Config.Connection.setAgdaPaths(state.channels.log, [nativePath])
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(nativePath), false)

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
          state,
          platform,
          ~channel=Connection__Download.Channel.DevALS,
        )

        let hasNativeItem =
          downloadItems->Array.some(((_, _, tag)) => tag == "native")

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)

        Assert.deepStrictEqual(hasNativeItem, true)
      },
    )

    Async.it(
      "should show native download candidate from canonical v6 LatestALS assets on desktop",
      async () => {
        // Build a canonical v6 release with assets using the new naming convention
        let windowsAsset = {
          Connection__Download__GitHub.Asset.url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-windows.zip",
          id: 1,
          node_id: "",
          name: "als-v6-Agda-2.8.0-windows.zip",
          label: None,
          content_type: "application/zip",
          state: "uploaded",
          size: 1000000,
          created_at: "2025-01-01T00:00:00Z",
          updated_at: "2025-01-01T00:00:00Z",
          browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-windows.zip",
        }
        let wasmAsset = {
          Connection__Download__GitHub.Asset.url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-wasm.wasm",
          id: 2,
          node_id: "",
          name: "als-v6-Agda-2.8.0-wasm.wasm",
          label: None,
          content_type: "application/octet-stream",
          state: "uploaded",
          size: 500000,
          created_at: "2025-01-01T00:00:00Z",
          updated_at: "2025-01-01T00:00:00Z",
          browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-wasm.wasm",
        }
        let v6Release = {
          Connection__Download__GitHub.Release.url: "https://api.github.com/repos/agda/agda-language-server/releases/v6",
          assets_url: "https://api.github.com/repos/agda/agda-language-server/releases/v6/assets",
          upload_url: "",
          html_url: "",
          id: 100,
          node_id: "",
          tag_name: "v6",
          target_commitish: "main",
          name: "v6",
          draft: false,
          prerelease: false,
          created_at: "2025-01-01T00:00:00Z",
          published_at: "2025-01-01T00:00:00Z",
          assets: [windowsAsset, wasmAsset],
          tarball_url: "",
          zipball_url: "",
          body: None,
        }
        let latestDescriptor = {
          Connection__Download__GitHub.DownloadDescriptor.asset: windowsAsset,
          release: v6Release,
          saveAsFileName: "latest-als",
        }

        module MockLatestALSv6 = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Windows)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
          let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
          let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
            switch channel {
            | Connection__Download.Channel.LatestALS =>
              Ok(
                Connection__Download.Source.FromGitHub(
                  Connection__Download.Channel.LatestALS,
                  latestDescriptor,
                ),
              )
            | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
            }
          )
          let download = (_globalStorageUri, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let findCommand = (_command, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }
        let platform: Platform.t = module(MockLatestALSv6)
        let state = createTestStateWithPlatform(platform)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
          state,
          platform,
          ~channel=Connection__Download.Channel.LatestALS,
        )
        let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
          manager,
          downloadItems,
        )

        let hasNativeDownloadAction =
          itemData->Array.some(item =>
            switch item {
            | DownloadAction(_, _, "native") => true
            | _ => false
            }
          )

        Assert.deepStrictEqual(hasNativeDownloadAction, true)
      },
    )

    Async.it(
      "should show unavailable rows (not empty list) when canonical v6 LatestALS assets do not match current platform",
      async () => {
        // v6 release contains only a windows native asset — no wasm asset, no macos asset
        let windowsOnlyAsset = {
          Connection__Download__GitHub.Asset.url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-windows.zip",
          id: 1,
          node_id: "",
          name: "als-v6-Agda-2.8.0-windows.zip",
          label: None,
          content_type: "application/zip",
          state: "uploaded",
          size: 1000000,
          created_at: "2025-01-01T00:00:00Z",
          updated_at: "2025-01-01T00:00:00Z",
          browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-windows.zip",
        }
        let v6ReleaseWindowsOnly = {
          Connection__Download__GitHub.Release.url: "",
          assets_url: "",
          upload_url: "",
          html_url: "",
          id: 100,
          node_id: "",
          tag_name: "v6",
          target_commitish: "main",
          name: "v6",
          draft: false,
          prerelease: false,
          created_at: "2025-01-01T00:00:00Z",
          published_at: "2025-01-01T00:00:00Z",
          assets: [windowsOnlyAsset],
          tarball_url: "",
          zipball_url: "",
          body: None,
        }
        let mismatchDescriptor = {
          Connection__Download__GitHub.DownloadDescriptor.asset: windowsOnlyAsset,
          release: v6ReleaseWindowsOnly,
          saveAsFileName: "latest-als",
        }

        // Platform is MacOS_Arm — does not match the windows native asset; no wasm asset either
        module MockLatestALSMismatch = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
          let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
          let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
            switch channel {
            | Connection__Download.Channel.LatestALS =>
              Ok(
                Connection__Download.Source.FromGitHub(
                  Connection__Download.Channel.LatestALS,
                  mismatchDescriptor,
                ),
              )
            | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
            }
          )
          let download = (_globalStorageUri, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let findCommand = (_command, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }
        let platform: Platform.t = module(MockLatestALSMismatch)
        let state = createTestStateWithPlatform(platform)

        let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
          state,
          platform,
          ~channel=Connection__Download.Channel.LatestALS,
        )

        let hasUnavailableNative =
          downloadItems->Array.some(((_, versionString, tag)) =>
            tag == "native" && versionString == State__SwitchVersion.Constants.downloadUnavailable
          )
        let hasUnavailableWasm =
          downloadItems->Array.some(((_, versionString, tag)) =>
            tag == "wasm" && versionString == State__SwitchVersion.Constants.downloadUnavailable
          )

        Assert.deepStrictEqual(hasUnavailableNative, true)
        Assert.deepStrictEqual(hasUnavailableWasm, true)
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
          ref(Connection__Download.Channel.DevALS),
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
          ref(Connection__Download.Channel.DevALS),
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
      "should call channel picker with correct items when DevALS is selected",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let capturedItems: ref<array<Connection__Download.Channel.pickerItem>> = ref([])
        let capturedPlaceholder: ref<string> = ref("")
        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        let selectedItem = makePickerItem(state, SelectOtherChannels)
        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
          ~showChannelPicker=async (items, placeholder) => {
            capturedItems := items
            capturedPlaceholder := placeholder
            resolvePickerCalled.contents()
            None
          },
        )

        await pickerCalled

        Assert.deepStrictEqual(Array.length(capturedItems.contents), 2)
        Assert.deepStrictEqual(
          capturedItems.contents,
          [
            {label: "Latest", description: "", detail: "Tracks the latest stable release", value: "latest"},
            {
              label: "Development",
              description: "selected",
              detail: "Tracks the latest commit of the master branch",
              value: "dev",
            },
          ],
        )
        Assert.deepStrictEqual(capturedPlaceholder.contents, "Select download channel")

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should call channel picker with description 'selected' on LatestALS when it is the active channel",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let capturedItems: ref<array<Connection__Download.Channel.pickerItem>> = ref([])
        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        let selectedItem = makePickerItem(state, SelectOtherChannels)
        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref(Connection__Download.Channel.LatestALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
          ~showChannelPicker=async (items, _placeholder) => {
            capturedItems := items
            resolvePickerCalled.contents()
            None
          },
        )

        await pickerCalled

        Assert.deepStrictEqual(Array.length(capturedItems.contents), 2)
        Assert.deepStrictEqual(
          capturedItems.contents->Array.map(i => i.description),
          ["selected", ""],
        )

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should pass full channelPickerItems (with description and detail) to the runtime VS Code picker",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        // Capture what the runtime picker (vscode.window.showQuickPick) actually receives.
        // Use a plain JS array (not a ref) so raw JS can push into it directly.
        let capturedItems: array<{"label": string, "description": string, "detail": string}> =
          %raw(`[]`)
        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        let mockShowQuickPick: (
          array<{"label": string, "description": string, "detail": string}>,
          ref<unit => unit>,
        ) => unit = %raw(`function(capturedItems, resolvePickerCalled) {
          globalThis.__savedShowQuickPick2 = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = function(items) {
            if (Array.isArray(items)) items.forEach(function(item) { capturedItems.push(item); });
            resolvePickerCalled.contents();
            return Promise.resolve(undefined);
          };
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick2;
          delete globalThis.__savedShowQuickPick2;
        }`)
        mockShowQuickPick(capturedItems, resolvePickerCalled)

        let selectedItem = makePickerItem(state, SelectOtherChannels)
        // No ~showChannelPicker seam — uses the runtime default
        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )

        await pickerCalled
        restoreShowQuickPick()

        // Runtime picker must receive full QuickPickItems, not bare label strings
        Assert.deepStrictEqual(
          capturedItems->Array.map(i => (i["label"], i["description"], i["detail"])),
          [
            ("Latest", "", "Tracks the latest stable release"),
            ("Development", "selected", "Tracks the latest commit of the master branch"),
          ],
        )

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should switch channel from LatestALS to DevALS via onSelection when showChannelPicker returns dev value",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.LatestALS)

        let downloadHeaderCapture = ref("")
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Others(header)) => downloadHeaderCapture := header
          | _ => ()
          }
        )

        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        // view.show is the final step in all onSelection branches — use it as completion signal.
        // orig() is called before resolve so VS Code's synchronous event dispatch completes first.
        let resolveViewShown = ref((_: unit) => ())
        let viewShown = Promise.make((resolve, _) => resolveViewShown := resolve)
        let patchShow: (State__SwitchVersion.View.t, unit => unit) => unit = %raw(`
          function(view, resolve) {
            var orig = view.quickPick.show.bind(view.quickPick);
            view.quickPick.show = function() { var r = orig(); resolve(undefined); return r; };
          }
        `)
        patchShow(view, resolveViewShown.contents)

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          selectedChannel,
          async _downloadItems => {
            let downloadHeader =
              "Download (" ++
                Connection__Download.Channel.pickerItem(
                  selectedChannel.contents,
                  ~selectedChannel=selectedChannel.contents,
                ).label ++ ")"
            state.channels.log->Chan.emit(Log.SwitchVersionUI(Others(downloadHeader)))
          },
          view,
          [selectedItem],
          ~showChannelPicker=async (_items, _placeholder) => {
            resolvePickerCalled.contents()
            Some("dev")
          },
        )

        await pickerCalled
        await viewShown

        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(downloadHeaderCapture.contents, "Download (Development)")

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should persist DevALS in memento when switching from LatestALS via onSelection",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.LatestALS)

        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        // Resolve when onSelection's fire-and-forget reaches view.show (final step)
        let resolveViewShown = ref((_: unit) => ())
        let viewShown = Promise.make((resolve, _) => resolveViewShown := resolve)
        // Call orig() before resolve so VS Code's synchronous event dispatch from show
        // has already completed by the time viewShown settles.
        let patchShow: (State__SwitchVersion.View.t, unit => unit) => unit = %raw(`
          function(view, resolve) {
            var orig = view.quickPick.show.bind(view.quickPick);
            view.quickPick.show = function() { var r = orig(); resolve(undefined); return r; };
          }
        `)
        patchShow(view, resolveViewShown.contents)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
          ~showChannelPicker=async (_items, _placeholder) => {
            resolvePickerCalled.contents()
            Some("dev")
          },
        )

        await pickerCalled
        await viewShown

        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), Some("dev"))

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should not destroy view when onDidHide fires after suppressHide is reset",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)

        let sawDestroyed = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
          | _ => ()
          }
        )

        view->State__SwitchVersion.View.onHide(() =>
          State__SwitchVersion.Handler.onHide(view)
        )

        // Simulate the race: picker opened (pendingHides incremented), then VS Code fires
        // onDidHide before the picker closes — must be consumed, not destroy.
        view.pendingHides = 1
        State__SwitchVersion.Handler.onHide(view) // stale onDidHide fires, consumes pending

        Assert.deepStrictEqual(sawDestroyed.contents, false)

        view->State__SwitchVersion.View.destroy
      },
    )

    Async.it(
      "should re-show main QuickPick after channel selection completes",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)

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

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
          ~showChannelPicker=async (_items, _placeholder) => {
            // Simulate VS Code hiding the main QuickPick when secondary picker opens
            view.quickPick->VSCode.QuickPick.hide
            Some("dev")
          },
        )

        await Test__Util.wait(200)

        // Channel switch must have resolved to DevALS
        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        // View MUST NOT be destroyed during channel selection
        Assert.deepStrictEqual(sawDestroyed.contents, false)
        // After channel selection completes, the main QuickPick MUST be re-shown
        Assert.deepStrictEqual(showCallCount.contents, 1)

        view->State__SwitchVersion.View.destroy
      },
    )

    describe("delete downloads", () => {
      Async.it(
        "selecting DeleteDownloads should invoke the action and complete selection",
        async () => {
          let storagePath = NodeJs.Path.join([
            NodeJs.Os.tmpdir(),
            "agda-state-delete-downloads-action-" ++ string_of_int(int_of_float(Js.Date.now())),
          ])
          let storageUri = VSCode.Uri.file(storagePath)
          let _ = await FS.createDirectory(storageUri)
          let inFlightPath = NodeJs.Path.join([storagePath, "in-flight.download"])
          NodeJs.Fs.writeFileSync(inFlightPath, NodeJs.Buffer.fromString("partial download"))
          let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
          let view = State__SwitchVersion.View.make(state.channels.log)
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)
          let selectedItem = makePickerItem(state, DeleteDownloads)
          let sawDestroyed = ref(false)
          let onSelectionCompleted = Log.on(
            state.channels.log,
            log =>
              switch log {
              | Log.SwitchVersionUI(SelectionCompleted) => true
              | _ => false
              },
          )
          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
            | _ => ()
            }
          )

          State__SwitchVersion.Handler.onSelection(
            state,
            makeMockPlatform(),
            manager,
            ref(Connection__Download.Channel.DevALS),
            _downloadInfo => Promise.resolve(),
            view,
            [selectedItem],
          )
          await onSelectionCompleted

          Assert.deepStrictEqual(sawDestroyed.contents, true)
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightPath), false)

          let _ = await FS.deleteRecursive(storageUri)
        },
      )
    })

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

        // SIMULATE: No preferred candidate (fresh state)
        await Memento.PreferredCandidate.set(state.memento, None)

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
      "should not modify PreferredCandidate when downloading via handler",
      async () => {
        let testCases = [
          (Some("/usr/bin/agda"), State__SwitchVersion.Download.Native, false),
          (Some("/usr/bin/agda"), State__SwitchVersion.Download.WASM, false),
          (None, State__SwitchVersion.Download.Native, false),
          (None, State__SwitchVersion.Download.WASM, false),
        ]

        let runCase = async (
          initialPicked: option<string>,
          variant: State__SwitchVersion.Download.variant,
          downloaded: bool,
        ) => {
          let state = createTestState()
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)

          await Memento.PreferredCandidate.set(state.memento, initialPicked)

          let activePath = "/opt/homebrew/bin/agda"
          let activeConnection = TestData.makeMockConnection(activePath, "2.6.3")
          Registry__Connection.status :=
            Active({
              connection: activeConnection,
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          let expectedDownloadPath = switch variant {
          | State__SwitchVersion.Download.Native =>
            NodeJs.Path.join([VSCode.Uri.fsPath(state.globalStorageUri), "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"])
          | State__SwitchVersion.Download.WASM =>
            VSCode.Uri.toString(VSCode.Uri.joinPath(state.globalStorageUri, ["releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm"]))
          }
          let platform =
            Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath)
          let versionString = "Agda v2.8.0 Language Server (dev build)"

          await State__SwitchVersion.Handler.handleDownload(
            state,
            platform,
            variant,
            downloaded,
            versionString,
            ~refreshUI=None,
          )

          let preferredAfter = Memento.PreferredCandidate.get(state.memento)
          // Manual UI download must not modify PreferredCandidate
          Assert.deepStrictEqual(preferredAfter, initialPicked)

          let itemData = await State__SwitchVersion.SwitchVersionManager.getItemData(
            manager,
            await State__SwitchVersion.Download.getAllAvailableDownloads(state, platform),
          )

          let selectedCandidates =
            itemData
            ->Array.filterMap(item =>
              switch item {
              | Candidate(path, _, _, true) => Some(path)
              | _ => None
              }
            )
          let currentPaths = Config.Connection.getAgdaPaths()
          let expectedSelectedCandidates = switch initialPicked {
          | Some(path) when currentPaths->Array.some(candidate => candidate == path) => [path]
          | _ when currentPaths->Array.some(candidate => candidate == activePath) => [activePath]
          | _ => []
          }
          Assert.deepStrictEqual(selectedCandidates, expectedSelectedCandidates)
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
      "should persist channel selection in memento across UI activations",
      async () => {
        // Channel selection MUST be stored in memento
        //
        // Test: call handleChannelSwitch (the real code path that runs after
        // user picks a channel in showQuickPick), then verify memento persistence.

        let state = createTestState()
        let platform = makeMockPlatform()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)

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

        // After channel switch, memento MUST store the canonical channel string
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), Some("dev"))
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
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let view = State__SwitchVersion.View.make(logChannel)

        // Set up config with existing paths including downloaded ALS from different channels
        let existingPaths = ["/usr/bin/agda", "/downloaded/als-v1", "/downloaded/als-dev"]
        await Config.Connection.setAgdaPaths(logChannel, existingPaths)

        // Build a real updateUI closure (same as onActivate creates)
        let updateUI = async (downloadItems: array<(bool, string, string)>): unit => {
          let downloadHeader =
            "Download (" ++
              Connection__Download.Channel.pickerItem(
                selectedChannel.contents,
                ~selectedChannel=selectedChannel.contents,
              ).label ++ ")"
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
      "should default to DevALS channel on fresh activation",
      async () => {
        let state = createTestState()
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
        | Some(label) => Connection__Download.Channel.fromString(label)->Option.getOr(Connection__Download.Channel.DevALS)
        | None => Connection__Download.Channel.DevALS
        }

        Assert.deepStrictEqual(restoredChannel, Connection__Download.Channel.DevALS)
      },
    )

    Async.it(
      "should default to DevALS channel when memento contains invalid channel string",
      async () => {
        let state = createTestState()
        await Memento.SelectedChannel.set(state.memento, "InvalidChannel")

        let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
        | Some(label) =>
          Connection__Download.Channel.fromString(label)->Option.getOr(
            Connection__Download.Channel.DevALS,
          )
        | None => Connection__Download.Channel.DevALS
        }

        Assert.deepStrictEqual(restoredChannel, Connection__Download.Channel.DevALS)
      },
    )

    Async.it(
      "should restore a valid persisted channel on activation",
      async () => {
        let state = createTestState()

        await Memento.SelectedChannel.set(
          state.memento,
          Connection__Download.Channel.toString(Connection__Download.Channel.DevALS),
        )

        let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
        | Some(label) =>
          Connection__Download.Channel.fromString(label)->Option.getOr(
            Connection__Download.Channel.DevALS,
          )
        | None => Connection__Download.Channel.DevALS
        }

        Assert.deepStrictEqual(restoredChannel, Connection__Download.Channel.DevALS)
      },
    )

    Async.it(
      "should use selected channel for manual UI-triggered downloads (handleDownload end-to-end)",
      async () => {
        let state = createTestState()
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let downloadedChannel = ref(None)
        let downloadedPath = VSCode.Uri.toString(
          VSCode.Uri.joinPath(state.globalStorageUri, ["releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm"]),
        )

        let platform: Platform.t = {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.DevALS =>
                Ok(Connection__Download.Source.FromURL(Connection__Download.Channel.DevALS, "https://example.invalid/dev-als.wasm", "dev-als"))
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) => {
              downloadedChannel := switch source {
              | Connection__Download.Source.FromURL(ch, _, _) => Some(ch)
              | Connection__Download.Source.FromGitHub(ch, _) => Some(ch)
              }
              Promise.resolve(Ok(downloadedPath))
            }
            let findCommand = (_command, ~timeout as _=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          module(MockPlatform)
        }

        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        await State__SwitchVersion.Handler.handleChannelSwitch(
          state, platform, manager, selectedChannel, Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
        )

        await State__SwitchVersion.Handler.handleDownload(
          state, platform, State__SwitchVersion.Download.WASM, false, "ALS vTest",
          ~channel=Connection__Download.Channel.DevALS,
        )

        Assert.deepStrictEqual(downloadedChannel.contents, Some(Connection__Download.Channel.DevALS))
      },
    )

    Async.it(
      "should restore LatestALS channel from memento on activation",
      async () => {
        let state = createTestState()
        await Memento.SelectedChannel.set(state.memento, "latest")

        let loggedHeaders = []
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Others(msg)) if String.startsWith(msg, "Download (") =>
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

        let platform = makeMockPlatform()
        await State__SwitchVersion.Handler.onActivate(state, platform)
        await onShown

        Assert.ok(Array.length(loggedHeaders) > 0)
        let header = loggedHeaders[0]->Option.getExn
        Assert.deepStrictEqual(header, "Download (Latest)")
      },
    )

    Async.it(
      "should emit Download (Development) header on fresh activation",
      async () => {
        let state = createTestState()
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        let loggedHeaders = []
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Others(msg)) if String.startsWith(msg, "Download (") =>
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

        let platform = makeMockPlatform()
        await State__SwitchVersion.Handler.onActivate(state, platform)
        await onShown

        Assert.ok(Array.length(loggedHeaders) > 0)
        let header = loggedHeaders[0]->Option.getExn
        Assert.deepStrictEqual(header, "Download (Development)")
      },
    )

    it(
      "should expose DevALS and LatestALS channels on Desktop",
      () => {
        Assert.deepStrictEqual(Connection__Download.Channel.all, [
          Connection__Download.Channel.DevALS,
          Connection__Download.Channel.LatestALS,
        ])
      },
    )

    describe("background update", () => {
      // Races promise p against a ms-millisecond timer; rejects immediately on timeout
      let withTimeout = (ms, p) => {
        let timerId: ref<option<Js.Global.timeoutId>> = ref(None)
        let clearTimer = () => timerId.contents->Option.forEach(id => Js.Global.clearTimeout(id))
        let wrappedP = p->Promise.then(v => { clearTimer(); Promise.resolve(v) })
        wrappedP->Promise.catch(_ => { clearTimer(); Promise.resolve() })->ignore
        Promise.race([
          wrappedP,
          Promise.make((_, reject) => {
            timerId :=
              Some(
                Js.Global.setTimeout(
                  () => {
                    let err: Js.Exn.t = %raw(`new Error("timed out after " + ms + "ms")`)
                    reject(err->Obj.magic)
                  },
                  ms,
                ),
              )
          }),
        ])
      }

      Async.it(
        "should replace Checking availability... with unavailable items when the background download-items promise fails",
        async () => {
          let state = createTestState()
          let previousPaths = Config.Connection.getAgdaPaths()
          await Config.Connection.setAgdaPaths(state.channels.log, [])

          // Narrow seam: inject a deferred promise via ~downloadItemsPromiseOverride.
          // Rejection happens AFTER await onActivate(...) returns, at which point
          // runBackgroundUpdate has already subscribed to the promise.
          module MockPlatform = {
            let determinePlatform = () =>
              Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))
            let findCommand = (_, ~timeout as _=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
            let alreadyDownloaded = (_, _) => Promise.resolve(None)
            let resolveDownloadChannel = (_, _) =>
              async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
            let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
              Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
            let askUserAboutDownloadPolicy = () =>
              Promise.resolve(Config.Connection.DownloadPolicy.Yes)
          }

          let (downloadItemsDeferred, _, rejectDownloadItems) = Util.Promise_.pending()

          // Capture UpdatedDownloadItems log events; resolve bgDone when the second arrives
          let downloadItemLogs: ref<array<array<(bool, string, string)>>> = ref([])
          let bgDoneResolve: ref<unit => unit> = ref(_ => ())
          let bgDone = Promise.make((resolve, _) => bgDoneResolve := resolve)

          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedDownloadItems(items)) =>
              downloadItemLogs := Array.concat(downloadItemLogs.contents, [items])
              if Array.length(downloadItemLogs.contents) >= 2 { bgDoneResolve.contents() }
            | _ => ()
            }
          )

          let restoreConfig = async () =>
            await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
          // finally: run restoreConfig regardless of success or failure
          let withFinally = async (p, finally_) => {
            let result = try { Ok(await p) } catch { | exn => Error(exn) }
            await finally_()
            switch result {
            | Ok(v) => v
            | Error(exn) => raise(exn)
            }
          }
          await withFinally(
            (async () => {
              await State__SwitchVersion.Handler.onActivate(
                state,
                module(MockPlatform),
                ~downloadItemsPromiseOverride=Some(downloadItemsDeferred),
              )
              // onActivate has returned; runBackgroundUpdate is now subscribed to the deferred.
              // Arm the timeout promise before rejecting to avoid a race where bgDone resolves
              // synchronously before the await is reached.
              let timeoutP = withTimeout(2000, bgDone)
              rejectDownloadItems(Failure("simulated background download-items failure"))
              await timeoutP
            })(),
            restoreConfig,
          )

          // Phase 1: both download items show "Checking availability..."
          Assert.deepStrictEqual(
            downloadItemLogs.contents[0],
            Some([
              (false, State__SwitchVersion.Constants.checkingAvailability, "native"),
              (false, State__SwitchVersion.Constants.checkingAvailability, "wasm"),
            ]),
          )

          // Background fallback: both show "Not available for this platform"
          Assert.deepStrictEqual(
            downloadItemLogs.contents[1],
            Some([
              (false, State__SwitchVersion.Constants.downloadUnavailable, "native"),
              (false, State__SwitchVersion.Constants.downloadUnavailable, "wasm"),
            ]),
          )
        },
      )
    })
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
            let entry = TestData.createMockEntry(ALS(Native, Some(("1.2.3", "2.6.4", None))), ())
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
            let entry = TestData.createMockEntry(ALS(Native, Some(("1.2.3", "2.6.4", None))), ())
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
            let entry = TestData.createMockEntry(ALS(Native, Some(("1.2.3", "2.6.4", None))), ())
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
            let entry = TestData.createMockEntry(ALS(Native, None), ())
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

        it(
          "should create WASM ALS item with version in label",
          () => {
            let entry = TestData.createMockEntry(ALS(WASM, Some(("1.2.3", "2.6.4", None))), ())
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
            Assert.strictEqual(
              actual.label,
              "$(squirrel)  Agda 2.6.4 Language Server v1.2.3 WASM",
            )
          },
        )

        it(
          "should create DevALS WASM item with dev-build label",
          () => {
            let entry = TestData.createMockEntry(ALS(WASM, Some(("dev", "2.8.0", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "vscode-userdata:/global/dev-als/als-dev-Agda-2.8.0-wasm/als.wasm",
              "vscode-userdata:/global/dev-als/als-dev-Agda-2.8.0-wasm/als.wasm",
              entry,
              false,
            )
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(
              actual.label,
              "$(squirrel)  Agda 2.8.0 Language Server (dev build) WASM",
            )
          },
        )

        it(
          "should create stable release WASM item with release tag label before probing",
          () => {
            let entry = TestData.createMockEntry(ALS(WASM, Some(("v6", "2.8.0", None))), ())
            let itemData: State__SwitchVersion.ItemData.t = Candidate(
              "vscode-userdata:/global/releases/v6/als-v6-Agda-2.8.0-wasm/als.wasm",
              "vscode-userdata:/global/releases/v6/als-v6-Agda-2.8.0-wasm/als.wasm",
              entry,
              false,
            )
            let actual = State__SwitchVersion.Item.fromItemData(
              itemData,
              VSCode.Uri.file("/extension/path"),
            )
            Assert.strictEqual(
              actual.label,
              "$(squirrel)  Agda 2.8.0 Language Server v6 WASM",
            )
          },
        )

        it(
          "should create WASM ALS item with unknown version label",
          () => {
            let entry = TestData.createMockEntry(ALS(WASM, None), ())
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
            Assert.strictEqual(actual.label, "$(squirrel)  Agda Language Server (version unknown) WASM")
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

  describe("background update failure fallback", () => {
    Async.it(
      "should swallow updateUI failure in error fallback",
      async () => {
        module MockDesktopPlatform = {
          let determinePlatform = () =>
            Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = (_, _) => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        // updateUI rejects — the nested try/catch in the fallback must swallow it
        let throwingUpdateUI = async (_items: array<(bool, string, string)>) => {
          raise(Failure("updateUI failure"))
        }

        // Should complete without throwing
        await State__SwitchVersion.Handler.backgroundUpdateFailureFallback(
          module(MockDesktopPlatform),
          throwingUpdateUI,
        )
      },
    )

    Async.it(
      "should show unavailable native and WASM items when determinePlatform returns an error",
      async () => {
        module MockErrorPlatform = {
          let determinePlatform = () =>
            Promise.resolve(
              Error(
                %raw(`{ os: "unknown", dist: "unknown", codename: "unknown", release: "unknown" }`),
              ),
            )
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = (_, _) => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let capturedItems: ref<array<(bool, string, string)>> = ref([])
        let mockUpdateUI = async items => capturedItems := items

        await State__SwitchVersion.Handler.backgroundUpdateFailureFallback(
          module(MockErrorPlatform),
          mockUpdateUI,
        )

        Assert.deepStrictEqual(capturedItems.contents, [
          (false, State__SwitchVersion.Constants.downloadUnavailable, "native"),
          (false, State__SwitchVersion.Constants.downloadUnavailable, "wasm"),
        ])
      },
    )
  })

  describe("background update catch path", () => {
    Async.it(
      "failing downloadItemsPromise on desktop should route to unavailable native and WASM fallback",
      async () => {
        module MockDesktopPlatform = {
          let determinePlatform = () =>
            Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = (_, _) => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let failingPromise: promise<array<(bool, string, string)>> =
          Promise.reject(Failure("simulated getAllAvailableDownloads failure"))
        // manager is intentionally unreachable: the failing promise rejects before
        // SwitchVersionManager.probeVersions can use the manager
        let manager: State__SwitchVersion.SwitchVersionManager.t = Obj.magic(())
        let capturedItems: ref<array<(bool, string, string)>> = ref([])
        let mockUpdateUI = async items => capturedItems := items

        await State__SwitchVersion.Handler.runBackgroundUpdate(
          failingPromise,
          module(MockDesktopPlatform),
          manager,
          mockUpdateUI,
        )

        Assert.deepStrictEqual(capturedItems.contents, [
          (false, State__SwitchVersion.Constants.downloadUnavailable, "native"),
          (false, State__SwitchVersion.Constants.downloadUnavailable, "wasm"),
        ])
      },
    )

    Async.it(
      "failing downloadItemsPromise on web should route to unavailable WASM-only fallback",
      async () => {
        module MockWebPlatform = {
          let determinePlatform = () =>
            Promise.resolve(Ok(Connection__Download__Platform.Web))
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = (_, _) => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let failingPromise: promise<array<(bool, string, string)>> =
          Promise.reject(Failure("simulated getAllAvailableDownloads failure"))
        // manager is intentionally unreachable: the failing promise rejects before
        // SwitchVersionManager.probeVersions can use the manager
        let manager: State__SwitchVersion.SwitchVersionManager.t = Obj.magic(())
        let capturedItems: ref<array<(bool, string, string)>> = ref([])
        let mockUpdateUI = async items => capturedItems := items

        await State__SwitchVersion.Handler.runBackgroundUpdate(
          failingPromise,
          module(MockWebPlatform),
          manager,
          mockUpdateUI,
        )

        Assert.deepStrictEqual(capturedItems.contents, [
          (false, State__SwitchVersion.Constants.downloadUnavailable, "wasm"),
        ])
      },
    )
  })

})
