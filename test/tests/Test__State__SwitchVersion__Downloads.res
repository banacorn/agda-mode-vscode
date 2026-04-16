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
  })


  describe("QuickPick", () => {
    let extensionUri = TestData.createMockExtensionUri()

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
            | Some(Separator("Download (channel: DevALS)")) => () // Expected
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
            | Some(Separator("Download (channel: DevALS)")) => () // Expected
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
                | Separator("Download (channel: DevALS)") => true
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
                | Separator("Download (channel: DevALS)") => true
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
              ~downloadHeader="Download (channel: DevALS)",
            )

            let hasDownloadSeparator = itemData->Array.some(
              data =>
                switch data {
                | Separator("Download (channel: DevALS)") => true
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
    let writeMockNativeAgdaAt = async (targetUri: VSCode.Uri.t) => {
      let targetPath = targetUri->VSCode.Uri.fsPath
      let parentDir = targetPath->NodeJs.Path.dirname->VSCode.Uri.file
      let _ = await FS.createDirectory(parentDir)
      let output = "Agda version 2.7.0.1"
      let content = if OS.onUnix {
        "#!/bin/sh\necho '" ++ output ++ "'\nexit 0"
      } else {
        "@echo " ++ output
      }

      NodeJs.Fs.writeFileSync(targetPath, NodeJs.Buffer.fromString(content))
      if OS.onUnix {
        switch await NodeJs.Fs.chmod(targetPath, ~mode=0o755) {
        | exception _ => Assert.fail("Expected mock native ALS executable to be chmod +x")
        | _ => ()
        }
      }
    }

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

    Async.it(
      "should expose DevALS and LatestALS channels",
      async () => {
        let channels = await State__SwitchVersion.Download.getAvailableChannels(makeMockPlatform())
        Assert.deepStrictEqual(channels, [
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
          ref([Connection__Download.Channel.DevALS]),
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
          ref([Connection__Download.Channel.DevALS]),
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

        let capturedItems: ref<array<State__SwitchVersion.Download.channelPickerItem>> = ref([])
        let capturedPlaceholder: ref<string> = ref("")
        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        let selectedItem = makePickerItem(state, SelectOtherChannels)
        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.LatestALS, Connection__Download.Channel.DevALS]),
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
            {label: "Latest", description: "", detail: "Tracks the latest stable release"},
            {
              label: "Development",
              description: "selected",
              detail: "Tracks the latest commit of the master branch",
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

        let capturedItems: ref<array<State__SwitchVersion.Download.channelPickerItem>> = ref([])
        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        let selectedItem = makePickerItem(state, SelectOtherChannels)
        State__SwitchVersion.Handler.onSelection(
          state,
          makeMockPlatform(),
          manager,
          ref([Connection__Download.Channel.LatestALS, Connection__Download.Channel.DevALS]),
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
          ref([Connection__Download.Channel.LatestALS, Connection__Download.Channel.DevALS]),
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
      "should switch channel from LatestALS to DevALS via onSelection when showChannelPicker returns Development label",
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
          ref([Connection__Download.Channel.LatestALS, Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => {
            let downloadHeader =
              "Download (channel: " ++
                State__SwitchVersion.Download.channelToLabel(selectedChannel.contents) ++ ")"
            state.channels.log->Chan.emit(Log.SwitchVersionUI(Others(downloadHeader)))
          },
          view,
          [selectedItem],
          ~showChannelPicker=async (_items, _placeholder) => {
            resolvePickerCalled.contents()
            Some("Development")
          },
        )

        await pickerCalled
        await viewShown

        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(downloadHeaderCapture.contents, "Download (channel: DevALS)")

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
          ref([Connection__Download.Channel.LatestALS, Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
          ~showChannelPicker=async (_items, _placeholder) => {
            resolvePickerCalled.contents()
            Some("Development")
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
          ref([Connection__Download.Channel.DevALS]),
          selectedChannel,
          async _downloadItems => (),
          view,
          [selectedItem],
          ~showChannelPicker=async (_items, _placeholder) => {
            // Simulate VS Code hiding the main QuickPick when secondary picker opens
            view.quickPick->VSCode.QuickPick.hide
            Some("Development")
          },
        )

        await Test__Util.wait(200)

        // View MUST NOT be destroyed during channel selection
        Assert.deepStrictEqual(sawDestroyed.contents, false)
        // After channel selection completes, the main QuickPick MUST be re-shown
        Assert.deepStrictEqual(showCallCount.contents, 1)

        view->State__SwitchVersion.View.destroy
      },
    )

    describe("delete downloads", () => {
      let runDeleteDownloads = async (state, view, manager) => {
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
          ref([Connection__Download.Channel.DevALS]),
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )
        await onSelectionCompleted
      }

      describe("connection.paths", () => {
      Async.it(
        "should remove <globalStorage>/releases/ directory from disk on Delete Downloads",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-releases-dir-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let releasesUri = VSCode.Uri.joinPath(storageUri, ["releases"])
        let nativeArtifactDir = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64",
        ])
        let wasmArtifactDir = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-wasm",
        ])
        let _ = await FS.createDirectory(nativeArtifactDir)
        let _ = await FS.createDirectory(wasmArtifactDir)
        NodeJs.Fs.writeFileSync(
          VSCode.Uri.joinPath(nativeArtifactDir, ["als"])->VSCode.Uri.fsPath,
          NodeJs.Buffer.fromString("mock native als"),
        )
        NodeJs.Fs.writeFileSync(
          VSCode.Uri.joinPath(wasmArtifactDir, ["als.wasm"])->VSCode.Uri.fsPath,
          NodeJs.Buffer.fromString("mock wasm"),
        )

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        await runDeleteDownloads(state, view, manager)

        Assert.deepStrictEqual((await FS.stat(releasesUri))->Result.isError, true)

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )

      Async.it(
        "should remove release-managed native candidate from connection.paths",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-release-native-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let nativePath = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath

        await Config.Connection.setAgdaPaths(state.channels.log, [keepPath, nativePath])
        await runDeleteDownloads(state, view, manager)

        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath])

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )

      Async.it(
        "should remove release-managed WASM candidate from connection.paths",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-release-wasm-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let wasmUri = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm",
        ])->VSCode.Uri.toString

        await Config.Connection.setAgdaPaths(state.channels.log, [keepPath, wasmUri])
        await runDeleteDownloads(state, view, manager)

        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath])

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )

      Async.it(
        "should remove all release-managed candidates and preserve user-managed candidates",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-release-all-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let keepBareCommand = "agda"
        let keepUri = "vscode-userdata:/global/user-managed/als.wasm"
        let devNativePath = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath
        let devWasmUri = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm",
        ])->VSCode.Uri.toString
        let v6NativePath = VSCode.Uri.joinPath(storageUri, [
          "releases", "v6", "als-v6-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath

        await Config.Connection.setAgdaPaths(
          state.channels.log,
          [keepPath, devNativePath, keepBareCommand, devWasmUri, keepUri, v6NativePath],
        )
        await runDeleteDownloads(state, view, manager)

        Assert.deepStrictEqual(
          Config.Connection.getAgdaPaths(),
          [keepPath, keepBareCommand, keepUri],
        )

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )
      })

      describe("resolved metadata", () => {
      Async.it(
        "should preserve non-download ResolvedMetadata and remove release-managed ResolvedMetadata on Delete Downloads",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-metadata-release-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let releasedPath = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath

        let keepResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(keepPath),
          resource: VSCode.Uri.file(keepPath),
        }
        let releasedResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(releasedPath),
          resource: VSCode.Uri.file(releasedPath),
        }

        await Memento.ResolvedMetadata.setKind(
          state.memento,
          keepResolved,
          Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
        )
        await Memento.ResolvedMetadata.setKind(
          state.memento,
          releasedResolved,
          Memento.ResolvedMetadata.ALS(Native, None),
        )

        await runDeleteDownloads(state, view, manager)

        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, keepResolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
        )
        Assert.deepStrictEqual(Memento.ResolvedMetadata.get(state.memento, releasedResolved), None)

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )

      Async.it(
        "should remove ResolvedMetadata for all release-managed artifacts and preserve unrelated resources",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-metadata-all-release-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepFilePath = "/usr/local/bin/agda"
        let keepUri = VSCode.Uri.parse("vscode-userdata:/global/user-managed/als.wasm")

        let devNativeResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(
            VSCode.Uri.joinPath(storageUri, [
              "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
            ])->VSCode.Uri.fsPath,
          ),
          resource: VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
          ]),
        }
        let devWasmResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(
            VSCode.Uri.joinPath(storageUri, [
              "releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm",
            ])->VSCode.Uri.toString,
          ),
          resource: VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm",
          ]),
        }
        let v6NativeResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(
            VSCode.Uri.joinPath(storageUri, [
              "releases", "v6", "als-v6-Agda-2.8.0-macos-arm64", "als",
            ])->VSCode.Uri.fsPath,
          ),
          resource: VSCode.Uri.joinPath(storageUri, [
            "releases", "v6", "als-v6-Agda-2.8.0-macos-arm64", "als",
          ]),
        }
        let keepFileResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(keepFilePath),
          resource: VSCode.Uri.file(keepFilePath),
        }
        let keepUriResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(keepUri->VSCode.Uri.toString),
          resource: keepUri,
        }

        await Memento.ResolvedMetadata.setKind(state.memento, devNativeResolved, Memento.ResolvedMetadata.ALS(Native, None))
        await Memento.ResolvedMetadata.setKind(state.memento, devWasmResolved, Memento.ResolvedMetadata.ALS(WASM, None))
        await Memento.ResolvedMetadata.setKind(state.memento, v6NativeResolved, Memento.ResolvedMetadata.ALS(Native, None))
        await Memento.ResolvedMetadata.setKind(state.memento, keepFileResolved, Memento.ResolvedMetadata.Agda(Some("2.7.0.1")))
        await Memento.ResolvedMetadata.setKind(state.memento, keepUriResolved, Memento.ResolvedMetadata.ALS(WASM, None))

        await runDeleteDownloads(state, view, manager)

        Assert.deepStrictEqual(Memento.ResolvedMetadata.get(state.memento, devNativeResolved), None)
        Assert.deepStrictEqual(Memento.ResolvedMetadata.get(state.memento, devWasmResolved), None)
        Assert.deepStrictEqual(Memento.ResolvedMetadata.get(state.memento, v6NativeResolved), None)
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, keepFileResolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
        )
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, keepUriResolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.ALS(WASM, None)),
        )

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )

      Async.it(
        "should preserve ResolvedMetadata under releases/ when that directory fails to delete",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-metadata-partial-release-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let releasesUri = VSCode.Uri.joinPath(storageUri, ["releases"])
        let _ = await FS.createDirectory(releasesUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let keepPath = "/usr/local/bin/agda"
        let releasedPath = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath

        let keepResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(keepPath),
          resource: VSCode.Uri.file(keepPath),
        }
        let releasedResolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(releasedPath),
          resource: VSCode.Uri.file(releasedPath),
        }

        await Memento.ResolvedMetadata.setKind(
          state.memento,
          keepResolved,
          Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
        )
        await Memento.ResolvedMetadata.setKind(
          state.memento,
          releasedResolved,
          Memento.ResolvedMetadata.ALS(Native, None),
        )

        let restoreDeleteRecursive = Test__Util.TestFS.withDeleteFailureFor(
          releasesUri->VSCode.Uri.fsPath,
        )

        await runDeleteDownloads(state, view, manager)
        restoreDeleteRecursive()

        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, keepResolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
        )
        Assert.deepStrictEqual(
          Memento.ResolvedMetadata.get(state.memento, releasedResolved)->Option.map(entry => entry.kind),
          Some(Memento.ResolvedMetadata.ALS(Native, None)),
        )

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )
      })

      describe("release cache", () => {
      Async.it(
        "should clear managed ALSReleaseCache repos and preserve unrelated repo cache on Delete Downloads",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-switch-version-delete-release-cache-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

        let now = Date.make()
        await Memento.ALSReleaseCache.setTimestamp(state.memento, "agda", "agda-language-server", now)
        await Memento.ALSReleaseCache.setReleases(state.memento, "agda", "agda-language-server", "agda-cache")
        await Memento.ALSReleaseCache.setTimestamp(state.memento, "banacorn", "agda-language-server", now)
        await Memento.ALSReleaseCache.setReleases(state.memento, "banacorn", "agda-language-server", "banacorn-cache")
        await Memento.ALSReleaseCache.setTimestamp(state.memento, "other", "repo", now)
        await Memento.ALSReleaseCache.setReleases(state.memento, "other", "repo", "other-cache")

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
          ref([Connection__Download.Channel.DevALS]),
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )
        await onSelectionCompleted

        let agdaReleases: option<string> = Memento.ALSReleaseCache.getReleases(
          state.memento,
          "agda",
          "agda-language-server",
        )
        let banacornReleases: option<string> = Memento.ALSReleaseCache.getReleases(
          state.memento,
          "banacorn",
          "agda-language-server",
        )
        let otherReleases: option<string> = Memento.ALSReleaseCache.getReleases(
          state.memento,
          "other",
          "repo",
        )

        Assert.deepStrictEqual(Memento.ALSReleaseCache.getTimestamp(state.memento, "agda", "agda-language-server"), None)
        Assert.deepStrictEqual(agdaReleases, None)
        Assert.deepStrictEqual(Memento.ALSReleaseCache.getTimestamp(state.memento, "banacorn", "agda-language-server"), None)
        Assert.deepStrictEqual(banacornReleases, None)
        Assert.deepStrictEqual(Memento.ALSReleaseCache.getTimestamp(state.memento, "other", "repo")->Option.isSome, true)
        Assert.deepStrictEqual(otherReleases, Some("other-cache"))

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )
      })

      describe("memento", () => {
      Async.it(
        "should leave PreferredCandidate unchanged when it points to a release-managed path",
        async () => {
          let storagePath = NodeJs.Path.join([
            NodeJs.Os.tmpdir(),
            "agda-delete-memento-release-preferred-" ++ string_of_int(int_of_float(Js.Date.now())),
          ])
          let storageUri = VSCode.Uri.file(storagePath)
          let _ = await FS.createDirectory(storageUri)

          let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
          let view = State__SwitchVersion.View.make(state.channels.log)
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)

          let releaseManagedPath = VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
          ])->VSCode.Uri.fsPath

          await Memento.PreferredCandidate.set(state.memento, Some(releaseManagedPath))
          await Config.Connection.setAgdaPaths(
            state.channels.log,
            ["/usr/bin/agda", releaseManagedPath],
          )

          await runDeleteDownloads(state, view, manager)

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(releaseManagedPath))
          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["/usr/bin/agda"])

          let _ = await FS.deleteRecursive(storageUri)
          view->State__SwitchVersion.View.destroy
        },
      )

      Async.it(
        "should leave PreferredCandidate unchanged when it points to a user-managed path",
        async () => {
          let storagePath = NodeJs.Path.join([
            NodeJs.Os.tmpdir(),
            "agda-delete-memento-user-preferred-" ++ string_of_int(int_of_float(Js.Date.now())),
          ])
          let storageUri = VSCode.Uri.file(storagePath)
          let _ = await FS.createDirectory(storageUri)

          let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
          let view = State__SwitchVersion.View.make(state.channels.log)
          let manager = State__SwitchVersion.SwitchVersionManager.make(state)

          let userPath = "/usr/local/bin/agda"
          let releaseManagedPath = VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
          ])->VSCode.Uri.fsPath

          await Memento.PreferredCandidate.set(state.memento, Some(userPath))
          await Config.Connection.setAgdaPaths(
            state.channels.log,
            [userPath, releaseManagedPath],
          )

          await runDeleteDownloads(state, view, manager)

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(userPath))
          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [userPath])

          let _ = await FS.deleteRecursive(storageUri)
          view->State__SwitchVersion.View.destroy
        },
      )
      })

      describe("in-flight files", () => {
      Async.it(
        "should delete orphaned in-flight.download and in-flight.download.zip files on Delete Downloads",
        async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-switch-delete-inflight-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)

        // Create orphaned in-flight temp files (left behind by an interrupted download)
        let inFlightUri = VSCode.Uri.joinPath(storageUri, ["in-flight.download"])
        let inFlightZipUri = VSCode.Uri.joinPath(storageUri, ["in-flight.download.zip"])
        NodeJs.Fs.writeFileSync(inFlightUri->VSCode.Uri.fsPath, NodeJs.Buffer.fromString("partial download"))
        NodeJs.Fs.writeFileSync(inFlightZipUri->VSCode.Uri.fsPath, NodeJs.Buffer.fromString("partial zip"))

        let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)

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
          ref([Connection__Download.Channel.DevALS]),
          ref(Connection__Download.Channel.DevALS),
          _downloadInfo => Promise.resolve(),
          view,
          [selectedItem],
        )
        await onSelectionCompleted

        Assert.deepStrictEqual((await FS.stat(inFlightUri))->Result.isError, true)
        Assert.deepStrictEqual((await FS.stat(inFlightZipUri))->Result.isError, true)

        let _ = await FS.deleteRecursive(storageUri)
        view->State__SwitchVersion.View.destroy
        },
      )
      })
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

        // After channel switch, memento MUST store the exact channel label
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
      "should default to DevALS channel when memento contains invalid channel label",
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

        let platform = makeMockPlatform()
        await State__SwitchVersion.Handler.onActivate(state, platform)
        await onShown

        Assert.ok(Array.length(loggedHeaders) > 0)
        let header = loggedHeaders[0]->Option.getExn
        Assert.deepStrictEqual(header, "Download (channel: LatestALS)")
      },
    )

    Async.it(
      "should expose DevALS and LatestALS channels on Desktop",
      async () => {
        let platform = makeMockPlatform()
        let channels = await State__SwitchVersion.Download.getAvailableChannels(platform)
        Assert.deepStrictEqual(channels, [
          Connection__Download.Channel.DevALS,
          Connection__Download.Channel.LatestALS,
        ])
      },
    )

    describe("getAllAvailableDownloads — DevALS", () => {
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

      let makeDevRelease = (): Connection__Download__GitHub.Release.t => {
        url: "https://api.github.com/repos/agda/agda-language-server/releases/dev",
        assets_url: "https://api.github.com/repos/agda/agda-language-server/releases/dev/assets",
        upload_url: "https://uploads.github.com/repos/agda/agda-language-server/releases/dev/assets",
        html_url: "https://github.com/agda/agda-language-server/releases/tag/dev",
        id: 1,
        node_id: "dev",
        tag_name: "dev",
        target_commitish: "main",
        name: "dev",
        draft: false,
        prerelease: true,
        created_at: "2024-01-01T00:00:00Z",
        published_at: "2024-01-01T00:00:00Z",
        assets: [
          makeDevAsset("als-dev-Agda-2.8.0-macos-arm64.zip"),
          makeDevAsset("als-dev-Agda-2.8.0-macos-x64.zip"),
          makeDevAsset("als-dev-Agda-2.8.0-ubuntu.zip"),
          makeDevAsset("als-dev-Agda-2.8.0-windows.zip"),
          makeDevAsset("als-dev-Agda-2.8.0-wasm.wasm"),
          makeDevAsset("als-dev-Agda-2.7.0.1-macos-arm64.zip"),
          makeDevAsset("als-dev-Agda-2.7.0.1-macos-x64.zip"),
          makeDevAsset("als-dev-Agda-2.7.0.1-ubuntu.zip"),
          makeDevAsset("als-dev-Agda-2.7.0.1-windows.zip"),
          makeDevAsset("als-dev-Agda-2.7.0.1-wasm.wasm"),
          makeDevAsset("als-dev-Agda-2.6.4.3-macos-arm64.zip"),
          makeDevAsset("als-dev-Agda-2.6.4.3-macos-x64.zip"),
          makeDevAsset("als-dev-Agda-2.6.4.3-ubuntu.zip"),
          makeDevAsset("als-dev-Agda-2.6.4.3-windows.zip"),
          makeDevAsset("als-dev-Agda-2.6.4.3-wasm.wasm"),
        ],
        tarball_url: "https://api.github.com/repos/agda/agda-language-server/tarball/dev",
        zipball_url: "https://api.github.com/repos/agda/agda-language-server/zipball/dev",
        body: Some("Dev build"),
      }

      let makeDevDescriptor = (): Connection__Download__GitHub.DownloadDescriptor.t => {
        let release = makeDevRelease()
        {
          Connection__Download__GitHub.DownloadDescriptor.asset: makeDevAsset(
            "als-dev-Agda-2.8.0-macos-arm64.zip",
          ),
          release,
          saveAsFileName: "dev-als",
        }
      }

      let makeDevSource = (assetName: string): Connection__Download.Source.t => {
        let release = makeDevRelease()
        Connection__Download.Source.FromGitHub(Connection__Download.Channel.DevALS, {
          Connection__Download__GitHub.DownloadDescriptor.asset: makeDevAsset(assetName),
          release,
          saveAsFileName: "dev-als",
        })
      }

      let makeDevALSPlatform = (): Platform.t => {
        let devDescriptor = makeDevDescriptor()
        {
          module MockDevALSPlatform = {
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
          module(MockDevALSPlatform)
        }
      }

      let fileParentUri = (fileUri: VSCode.Uri.t): VSCode.Uri.t =>
        VSCode.Uri.file(NodeJs.Path.dirname(VSCode.Uri.fsPath(fileUri)))

      Async.it(
        "should return 6 items (3 native + 3 wasm) for MacOS_Arm with 3 Agda versions",
        async () => {
          let platform = makeDevALSPlatform()
          let state = createTestStateWithPlatform(platform)
          let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
            state,
            platform,
            ~channel=Connection__Download.Channel.DevALS,
          )

          Assert.deepStrictEqual(downloadItems, [
            (false, "Agda v2.8.0 Language Server (dev build)", "native"),
            (false, "Agda v2.7.0.1 Language Server (dev build)", "native"),
            (false, "Agda v2.6.4.3 Language Server (dev build)", "native"),
            (false, "Agda v2.8.0 Language Server (dev build)", "wasm"),
            (false, "Agda v2.7.0.1 Language Server (dev build)", "wasm"),
            (false, "Agda v2.6.4.3 Language Server (dev build)", "wasm"),
          ])
        },
      )

      Async.it(
        "should mark only the matching DevALS native source as downloaded",
        async () => {
          let platform = makeDevALSPlatform()
          let storagePath = NodeJs.Path.join([
            NodeJs.Os.tmpdir(),
            "agda-devals-source-downloaded-" ++ string_of_int(int_of_float(Js.Date.now())),
          ])
          let storageUri = VSCode.Uri.file(storagePath)
          let downloadedSource = makeDevSource("als-dev-Agda-2.8.0-macos-arm64.zip")
          let otherSource = makeDevSource("als-dev-Agda-2.7.0.1-macos-arm64.zip")
          let downloadedNative = Connection__Download.expectedUriForSource(storageUri, downloadedSource)
          let downloadedDir = fileParentUri(downloadedNative)
          Assert.notDeepStrictEqual(
            Connection__Download.expectedPathForSource(storageUri, downloadedSource),
            Connection__Download.expectedPathForSource(storageUri, otherSource),
          )
          let _ = await FS.createDirectory(downloadedDir)
          let _ = await FS.writeFile(downloadedNative, Uint8Array.fromLength(0))

          let state = createTestStateWithPlatformAndStorage(platform, storageUri)
          let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
            state,
            platform,
            ~channel=Connection__Download.Channel.DevALS,
          )

          let _ = await FS.deleteRecursive(storageUri)

          Assert.deepStrictEqual(downloadItems, [
            (true, "Agda v2.8.0 Language Server (dev build)", "native"),
            (false, "Agda v2.7.0.1 Language Server (dev build)", "native"),
            (false, "Agda v2.6.4.3 Language Server (dev build)", "native"),
            (false, "Agda v2.8.0 Language Server (dev build)", "wasm"),
            (false, "Agda v2.7.0.1 Language Server (dev build)", "wasm"),
            (false, "Agda v2.6.4.3 Language Server (dev build)", "wasm"),
          ])
        },
      )

      Async.it(
        "should suppress only the configured matching DevALS native source",
        async () => {
          let platform = makeDevALSPlatform()
          let storagePath = NodeJs.Path.join([
            NodeJs.Os.tmpdir(),
            "agda-devals-source-suppress-" ++ string_of_int(int_of_float(Js.Date.now())),
          ])
          let storageUri = VSCode.Uri.file(storagePath)
          let downloadedSource = makeDevSource("als-dev-Agda-2.8.0-macos-arm64.zip")
          let downloadedNative = Connection__Download.expectedUriForSource(storageUri, downloadedSource)
          let downloadedDir = fileParentUri(downloadedNative)
          let _ = await FS.createDirectory(downloadedDir)
          let _ = await FS.writeFile(downloadedNative, Uint8Array.fromLength(0))

          let state = createTestStateWithPlatformAndStorage(platform, storageUri)
          let previousPaths = Config.Connection.getAgdaPaths()
          await Config.Connection.setAgdaPaths(state.channels.log, [
            Connection__Download.expectedPathForSource(storageUri, downloadedSource),
          ])

          let downloadItems = await State__SwitchVersion.Download.getAllAvailableDownloads(
            state,
            platform,
            ~channel=Connection__Download.Channel.DevALS,
          )

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
          let _ = await FS.deleteRecursive(storageUri)

          Assert.deepStrictEqual(downloadItems, [
            (false, "Agda v2.7.0.1 Language Server (dev build)", "native"),
            (false, "Agda v2.6.4.3 Language Server (dev build)", "native"),
            (false, "Agda v2.8.0 Language Server (dev build)", "wasm"),
            (false, "Agda v2.7.0.1 Language Server (dev build)", "wasm"),
            (false, "Agda v2.6.4.3 Language Server (dev build)", "wasm"),
          ])
        },
      )
    })

    describe("onActivate — background update wiring (Fix 6)", () => {
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


  describe("Channel picker items", () => {
    it("labels should be exactly Latest and Development", () => {
      let items = State__SwitchVersion.Download.channelPickerItems(
        ~selectedChannel=Connection__Download.Channel.DevALS,
      )
      Assert.deepStrictEqual(
        items->Array.map(i => i.label),
        ["Latest", "Development"],
      )
    })

    it("order should be Latest then Development", () => {
      let items = State__SwitchVersion.Download.channelPickerItems(
        ~selectedChannel=Connection__Download.Channel.LatestALS,
      )
      Assert.deepStrictEqual(
        items->Array.map(i => i.label),
        ["Latest", "Development"],
      )
    })

    it("selected channel should have description 'selected', other should be empty", () => {
      let itemsDevSelected = State__SwitchVersion.Download.channelPickerItems(
        ~selectedChannel=Connection__Download.Channel.DevALS,
      )
      Assert.deepStrictEqual(
        itemsDevSelected->Array.map(i => i.description),
        ["", "selected"],
      )

      let itemsLatestSelected = State__SwitchVersion.Download.channelPickerItems(
        ~selectedChannel=Connection__Download.Channel.LatestALS,
      )
      Assert.deepStrictEqual(
        itemsLatestSelected->Array.map(i => i.description),
        ["selected", ""],
      )
    })

    it("Latest should have detail 'Tracks the latest stable release'", () => {
      let items = State__SwitchVersion.Download.channelPickerItems(
        ~selectedChannel=Connection__Download.Channel.DevALS,
      )
      let latestItem = items->Array.find(i => i.label == "Latest")
      Assert.deepStrictEqual(
        latestItem->Option.map(i => i.detail),
        Some("Tracks the latest stable release"),
      )
    })

    it("Development should have detail 'Tracks the latest commit of the master branch'", () => {
      let items = State__SwitchVersion.Download.channelPickerItems(
        ~selectedChannel=Connection__Download.Channel.LatestALS,
      )
      let devItem = items->Array.find(i => i.label == "Development")
      Assert.deepStrictEqual(
        devItem->Option.map(i => i.detail),
        Some("Tracks the latest commit of the master branch"),
      )
    })

    describe("channelFromLabel", () => {
      it("should parse 'Latest' to LatestALS", () => {
        Assert.deepStrictEqual(
          State__SwitchVersion.Download.channelFromLabel("Latest"),
          Some(Connection__Download.Channel.LatestALS),
        )
      })

      it("should parse 'Development' to DevALS", () => {
        Assert.deepStrictEqual(
          State__SwitchVersion.Download.channelFromLabel("Development"),
          Some(Connection__Download.Channel.DevALS),
        )
      })

      it("should reject non-UI label 'DevALS'", () => {
        Assert.deepStrictEqual(
          State__SwitchVersion.Download.channelFromLabel("DevALS"),
          None,
        )
      })

      it("should reject non-UI label 'LatestALS'", () => {
        Assert.deepStrictEqual(
          State__SwitchVersion.Download.channelFromLabel("LatestALS"),
          None,
        )
      })

      it("should reject garbage input", () => {
        Assert.deepStrictEqual(
          State__SwitchVersion.Download.channelFromLabel("garbage"),
          None,
        )
      })
    })
  })

  describe("ItemCreation", () => {
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

  describe("Release-managed download detection", () => {
    Async.it(
      "DevALS native artifact at releases/dev/<artifact>/als is detected as downloaded",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-release-native-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let globalStorageUri = VSCode.Uri.file(storagePath)
        let artifactDir = VSCode.Uri.joinPath(globalStorageUri, [
          "releases",
          "dev",
          "als-dev-Agda-2.8.0-macos-arm64",
        ])
        let _ = await FS.createDirectory(artifactDir)
        let alsPath = NodeJs.Path.join([VSCode.Uri.fsPath(artifactDir), "als"])
        NodeJs.Fs.writeFileSync(alsPath, NodeJs.Buffer.fromString("mock-native"))

        let result = await Connection__Download.alreadyDownloaded(
          globalStorageUri,
          Connection__Download.Channel.DevALS,
        )
        Assert.deepStrictEqual(result, Some(alsPath))

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )

    Async.it(
      "DevALS WASM artifact at releases/dev/<artifact>/als.wasm is detected as downloaded",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-release-wasm-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let globalStorageUri = VSCode.Uri.file(storagePath)
        let artifactDir = VSCode.Uri.joinPath(globalStorageUri, [
          "releases",
          "dev",
          "als-dev-Agda-2.8.0-wasm",
        ])
        let _ = await FS.createDirectory(artifactDir)
        let wasmPath = NodeJs.Path.join([VSCode.Uri.fsPath(artifactDir), "als.wasm"])
        NodeJs.Fs.writeFileSync(wasmPath, NodeJs.Buffer.fromString("mock-wasm"))

        let result = await Connection__Download.alreadyDownloaded(
          globalStorageUri,
          Connection__Download.Channel.DevALS,
        )
        Assert.deepStrictEqual(result, Some(wasmPath))

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )

    Async.it(
      "Different release/artifact paths do not count as downloaded",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-release-wrong-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let globalStorageUri = VSCode.Uri.file(storagePath)
        // Artifact exists but under a different release tag ("v1.0.0" not "dev")
        let wrongArtifactDir = VSCode.Uri.joinPath(globalStorageUri, [
          "releases",
          "v1.0.0",
          "als-dev-Agda-2.8.0-macos-arm64",
        ])
        let _ = await FS.createDirectory(wrongArtifactDir)
        let alsPath = NodeJs.Path.join([VSCode.Uri.fsPath(wrongArtifactDir), "als"])
        NodeJs.Fs.writeFileSync(alsPath, NodeJs.Buffer.fromString("mock-native"))

        let result = await Connection__Download.alreadyDownloaded(
          globalStorageUri,
          Connection__Download.Channel.DevALS,
        )
        // artifact's releaseTag is "dev" but directory is under "v1.0.0" — not a match
        Assert.deepStrictEqual(result, None)

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )

    Async.it(
      "Malformed artifact directory names are ignored",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-release-malformed-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let globalStorageUri = VSCode.Uri.file(storagePath)
        // Directory name does not match the DownloadArtifact.parseName format
        let malformedDir = VSCode.Uri.joinPath(globalStorageUri, [
          "releases",
          "dev",
          "not-a-valid-artifact-name",
        ])
        let _ = await FS.createDirectory(malformedDir)
        let alsPath = NodeJs.Path.join([VSCode.Uri.fsPath(malformedDir), "als"])
        NodeJs.Fs.writeFileSync(alsPath, NodeJs.Buffer.fromString("mock-binary"))

        let result = await Connection__Download.alreadyDownloaded(
          globalStorageUri,
          Connection__Download.Channel.DevALS,
        )
        Assert.deepStrictEqual(result, None)

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )
  })

  describe("backgroundUpdate — error fallback (Fix 6)", () => {
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

  describe("runBackgroundUpdate — catch-path wiring (Fix 6)", () => {
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
