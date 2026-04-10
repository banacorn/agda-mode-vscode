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
            ref([Connection__Download.Channel.Hardcoded]),
            ref(Connection__Download.Channel.Hardcoded),
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
      "should treat alias-equivalent candidate selection as unchanged",
      async () => {
        let state = createTestState()
        let view = State__SwitchVersion.View.make(state.channels.log)
        let manager = State__SwitchVersion.SwitchVersionManager.make(state)
        let fsPath = "/tmp/hardcoded-als/als.wasm"
        let uriPath = "file:///tmp/hardcoded-als/als.wasm"

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
          ref([Connection__Download.Channel.Hardcoded]),
          ref(Connection__Download.Channel.Hardcoded),
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

          let expectedDownloadPath =
            State__SwitchVersion.Download.expectedPathForVariant(state.globalStorageUri, selectedVariant)

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
            ref([Connection__Download.Channel.Hardcoded]),
            ref(Connection__Download.Channel.Hardcoded),
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

        let fsPath = "/tmp/hardcoded-als/als.wasm"
        let uriPath = "file:///tmp/hardcoded-als/als.wasm"

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

  })


})
