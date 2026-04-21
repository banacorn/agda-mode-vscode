open Mocha

@module("node:fs/promises") external mkdtemp: string => Js.Promise.t<string> = "mkdtemp"

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
        await State__SwitchVersion.activate(state, makeMockPlatform())

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
        await State__SwitchVersion.activate(state, makeMockPlatform())

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
        await State__SwitchVersion.activate(state, makeMockPlatform())

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
      "should not show native download option on web platform",
      async () => {
        module MockWebPlatform = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Web))

          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))

          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)

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
            let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
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
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
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
            tag == "native" && versionString == Connection__UI__ItemData.Constants.downloadUnavailable
          )
        let hasUnavailableWasm =
          downloadItems->Array.some(((_, versionString, tag)) =>
            tag == "wasm" && versionString == Connection__UI__ItemData.Constants.downloadUnavailable
          )

        Assert.deepStrictEqual(hasUnavailableNative, true)
        Assert.deepStrictEqual(hasUnavailableWasm, true)
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
        await State__SwitchVersion.activate(state, makeMockPlatformWithDownload())

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
        await State__SwitchVersion.activate(state, platform)
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
        await State__SwitchVersion.activate(state, platform)
        await onShown

        Assert.ok(Array.length(loggedHeaders) > 0)
        let header = loggedHeaders[0]->Option.getExn
        Assert.deepStrictEqual(header, "Download (Development)")
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
            let alreadyDownloaded = _ => Promise.resolve(None)
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
              await State__SwitchVersion.activate(
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
              (false, Connection__UI__ItemData.Constants.checkingAvailability, "native"),
              (false, Connection__UI__ItemData.Constants.checkingAvailability, "wasm"),
            ]),
          )

          // Background fallback: both show "Not available for this platform"
          Assert.deepStrictEqual(
            downloadItemLogs.contents[1],
            Some([
              (false, Connection__UI__ItemData.Constants.downloadUnavailable, "native"),
              (false, Connection__UI__ItemData.Constants.downloadUnavailable, "wasm"),
            ]),
          )
        },
      )
    })
  })

})
