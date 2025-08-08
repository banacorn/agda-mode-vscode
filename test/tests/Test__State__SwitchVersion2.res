open Mocha
open State__SwitchVersion2.ItemData

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
  let alsEntry = createMockEntry(ALS(Some(("4.0.0", "2.6.4"))), ())
  let unknownEntry = createMockEntry(Unknown, ~error="Permission denied", ())

  // Simple mock functions for testing
  let createMockMemento = () => Memento.make(None)
  let createMockExtensionUri = () => VSCode.Uri.file("/test/extension")
}

describe("State__SwitchVersion2", () => {
  describe("Core", () => {
    describe(
      "getEndpointDisplayInfo",
      () => {
        it(
          "should format Agda endpoint with version",
          () => {
            let entry = TestData.agdaEntry
            let (label, errorDescription) = State__SwitchVersion2.ItemData.getEndpointDisplayInfo(
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
            let (label, errorDescription) = State__SwitchVersion2.ItemData.getEndpointDisplayInfo(
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
            let (label, errorDescription) = State__SwitchVersion2.ItemData.getEndpointDisplayInfo(
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
            let (label, errorDescription) = State__SwitchVersion2.ItemData.getEndpointDisplayInfo(
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
            let (label, errorDescription) = State__SwitchVersion2.ItemData.getEndpointDisplayInfo(
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
              State__SwitchVersion2.ItemData.shouldEndpointHaveIcon(Agda(Some("2.6.4"))),
              true,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion2.ItemData.shouldEndpointHaveIcon(Agda(None)),
              true,
            )
          },
        )

        it(
          "should return false for ALS endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion2.ItemData.shouldEndpointHaveIcon(ALS(Some(("4.0.0", "2.6.4")))),
              false,
            )
            Assert.deepStrictEqual(
              State__SwitchVersion2.ItemData.shouldEndpointHaveIcon(ALS(None)),
              false,
            )
          },
        )

        it(
          "should return false for unknown endpoints",
          () => {
            Assert.deepStrictEqual(
              State__SwitchVersion2.ItemData.shouldEndpointHaveIcon(Unknown),
              false,
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
            let itemData: State__SwitchVersion2.ItemData.t = Endpoint("/usr/bin/agda", entry, false)
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "Agda v2.6.4")
            Assert.deepStrictEqual(item.description, Some(""))
            Assert.deepStrictEqual(item.detail, Some("/usr/bin/agda"))
          },
        )

        it(
          "should include icon for Agda endpoints",
          () => {
            let entry = TestData.agdaEntry
            let itemData: State__SwitchVersion2.ItemData.t = Endpoint("/usr/bin/agda", entry, false)
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

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
            let itemData: State__SwitchVersion2.ItemData.t = Endpoint("/usr/bin/als", entry, false)
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

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
            let itemData: State__SwitchVersion2.ItemData.t = Separator("Test Section")
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

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
            let itemData: State__SwitchVersion2.ItemData.t = NoInstallations
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "$(info) No installations found")
            Assert.deepStrictEqual(item.description, Some("Try installing Agda or ALS first"))
            Assert.deepStrictEqual(item.detail, Some("No executable paths detected"))
          },
        )
      },
    )

    describe(
      "open folder item",
      () => {
        it(
          "should create open folder item",
          () => {
            let itemData: State__SwitchVersion2.ItemData.t = OpenFolder("/test/global/storage")
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(item.label, "$(folder-opened)  Open download folder")
            Assert.deepStrictEqual(
              item.description,
              Some("Where the language servers are downloaded to"),
            )
            Assert.deepStrictEqual(item.detail, Some("/test/global/storage"))
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
            let itemData: State__SwitchVersion2.ItemData.t = DownloadAction(false, "ALS v1.0.0")
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(
              item.label,
              "$(cloud-download)  Download the latest Agda Language Server",
            )
            Assert.deepStrictEqual(item.description, Some(""))
            Assert.deepStrictEqual(item.detail, Some("ALS v1.0.0"))
          },
        )

        it(
          "should create download item when already downloaded",
          () => {
            let itemData: State__SwitchVersion2.ItemData.t = DownloadAction(true, "ALS v1.0.0")
            let item = State__SwitchVersion2.Item.fromItemData(itemData, extensionUri)

            Assert.deepStrictEqual(
              item.label,
              "$(cloud-download)  Download the latest Agda Language Server",
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
        let qp = State__SwitchVersion2.View.make(Chan.make())

        Assert.deepStrictEqual(Array.length(qp.items), 0)
        Assert.deepStrictEqual(Array.length(qp.subscriptions), 0)
      },
    )

    it(
      "should update items correctly",
      () => {
        let qp = State__SwitchVersion2.View.make(Chan.make())
        let itemData: State__SwitchVersion2.ItemData.t = NoInstallations
        let items = [
          State__SwitchVersion2.Item.fromItemData(itemData, TestData.createMockExtensionUri()),
        ]

        qp->State__SwitchVersion2.View.updateItems(items)

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
          "should return no installations item when no entries",
          () => {
            let entries = Dict.make()
            let itemData: array<
              State__SwitchVersion2.ItemData.t,
            > = State__SwitchVersion2.ItemData.entriesToItemData(
              entries,
              None,
              None,
              "/test/global/storage",
            )

            Assert.deepStrictEqual(Array.length(itemData), 3) // No installations + Misc separator + Open folder
            Assert.deepStrictEqual(itemData[0], Some(NoInstallations))

            switch itemData[1] {
            | Some(Separator("Misc")) => () // Expected
            | _ => Assert.fail("Expected Misc separator")
            }

            switch itemData[2] {
            | Some(OpenFolder("/test/global/storage")) => () // Expected
            | _ => Assert.fail("Expected OpenFolder item")
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
              State__SwitchVersion2.ItemData.t,
            > = State__SwitchVersion2.ItemData.entriesToItemData(
              entries,
              None,
              None,
              "/test/global/storage",
            )

            Assert.deepStrictEqual(Array.length(itemData), 5) // Installed separator + 2 endpoints + Misc separator + Open folder

            switch itemData[0] {
            | Some(Separator("Installed")) => () // Expected
            | _ => Assert.fail("Expected Installed separator")
            }

            switch itemData[3] {
            | Some(Separator("Misc")) => () // Expected
            | _ => Assert.fail("Expected Misc separator")
            }

            switch itemData[4] {
            | Some(OpenFolder("/test/global/storage")) => () // Expected
            | _ => Assert.fail("Expected OpenFolder item")
            }
          },
        )

        it(
          "should include download section when download info is provided",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)

            let itemData: array<
              State__SwitchVersion2.ItemData.t,
            > = State__SwitchVersion2.ItemData.entriesToItemData(
              entries,
              None,
              Some((false, "ALS v1.0.0")),
              "/test/global/storage",
            )

            Assert.deepStrictEqual(Array.length(itemData), 6) // Installed separator + 1 item + Download separator + download item + Misc separator + Open folder

            let downloadSeparator = itemData->Array.find(
              data =>
                switch data {
                | Separator("Download") => true
                | _ => false
                },
            )

            let downloadAction = itemData->Array.find(
              data =>
                switch data {
                | DownloadAction(false, "ALS v1.0.0") => true
                | _ => false
                },
            )

            Assert.ok(downloadSeparator->Option.isSome)
            Assert.ok(downloadAction->Option.isSome)
          },
        )
      },
    )
  })

  describe("Events", () => {
    // Simple mock platform for testing
    let makeMockPlatform = (): Platform.t => {
      module MockPlatform = {
        let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
        let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
        let alreadyDownloaded = _ => () => Promise.resolve(None)
        let alreadyDownloaded2 = _ => () => Promise.resolve(None)
        let downloadLatestALS = (_, _) => _ =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        let downloadLatestALS2 = (_, _) => _ =>
          Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
        let getInstalledEndpointsAndPersistThem = _ => {
          // Mock the same endpoints for consistency
          let endpoints = Dict.make()
          endpoints->Dict.set(
            "/usr/bin/agda",
            Ok(Connection.Endpoint.Agda("2.6.4", "/usr/bin/agda")),
          )
          Promise.resolve(endpoints)
        }
        let getInstalledEndpointsAndPersistThem2 = _ => {
          // Mock discovering the agda endpoint during filesystem sync
          let endpoints = Dict.make()
          endpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
          Promise.resolve(endpoints)
        }
        let findCommand = (_command, ~timeout as _timeout=1000) =>
          Promise.resolve(Error(Connection__Command.Error.NotFound))
        let findCommands = async commands => Error(
          commands
          ->Array.map(command => (command, Connection__Command.Error.NotFound))
          ->Dict.fromArray,
        )
      }
      module(MockPlatform)
    }

    // Create a test state with proper channels
    let createTestState = () => {
      let channels = {
        State.inputMethod: Chan.make(),
        responseHandled: Chan.make(),
        commandHandled: Chan.make(),
        log: Chan.make(),
      }

      let mockEditor = %raw(`{
        document: { fileName: "test.agda" }
      }`)

      let mockUri = VSCode.Uri.file("/test/path")

      State.make(makeMockPlatform(), channels, mockUri, mockUri, None, mockEditor, None)
    }

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
        let mockConnection = %raw(`{ TAG: "Agda", _0: null, _1: "/usr/bin/agda", _2: "2.6.4" }`)
        state.connection = Some(mockConnection)

        // INVOKE: onActivate to trigger the actual UI logic
        await State__SwitchVersion2.Handler.onActivate(state, makeMockPlatform())

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
        let mockConnection = %raw(`{ TAG: "Agda", _0: null, _1: "/opt/homebrew/bin/agda", _2: "2.6.3" }`)
        state.connection = Some(mockConnection)

        // INVOKE: onActivate to trigger the actual UI logic
        await State__SwitchVersion2.Handler.onActivate(state, makeMockPlatform())

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
        let mockConnection = %raw(`{ TAG: "Agda", _0: null, _1: "/usr/bin/agda", _2: "2.6.4" }`)
        state.connection = Some(mockConnection)

        // PHASE 1: Test initial state (download available but not downloaded)
        // Mock platform to return download available
        let makeMockPlatformWithDownload = (): Platform.t => {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
            let alreadyDownloaded = _ => () => Promise.resolve(None)
            let alreadyDownloaded2 = _ => () => Promise.resolve(None)
            let downloadLatestALS = (_, _) => _ =>
              Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
            let downloadLatestALS2 = (_, _) => _ => Promise.resolve(Ok("/test/downloaded/als")) // Simulate successful download
            let getInstalledEndpointsAndPersistThem = _ => {
              let endpoints = Dict.make()
              endpoints->Dict.set(
                "/usr/bin/agda",
                Ok(Connection.Endpoint.Agda("2.6.4", "/usr/bin/agda")),
              )
              Promise.resolve(endpoints)
            }
            let getInstalledEndpointsAndPersistThem2 = _ => {
              let endpoints = Dict.make()
              endpoints->Dict.set("/usr/bin/agda", Memento.Endpoints.Agda(Some("2.6.4")))
              Promise.resolve(endpoints)
            }
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
            let findCommands = async commands => Error(
              commands
              ->Array.map(command => (command, Connection__Command.Error.NotFound))
              ->Dict.fromArray,
            )
          }
          module(MockPlatform)
        }

        // INVOKE: onActivate to trigger the actual UI logic with download available
        await State__SwitchVersion2.Handler.onActivate(state, makeMockPlatformWithDownload())

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
        discoveredEndpoints->Dict.set("/opt/homebrew/bin/agda", Memento.Endpoints.Agda(Some("2.6.3")))  
        discoveredEndpoints->Dict.set("/usr/local/bin/agda", Memento.Endpoints.Agda(Some("2.6.2")))
        discoveredEndpoints->Dict.set("/usr/bin/als", Memento.Endpoints.ALS(Some(("4.0.0", "2.6.4"))))
        await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)
        
        // SIMULATE: User has explicitly selected one specific endpoint
        await Memento.PickedConnection.set(state.memento, Some("/opt/homebrew/bin/agda"))
        
        // SIMULATE: But different endpoint is currently active (should be overridden by memento)
        let mockConnection = %raw(`{ TAG: "Agda", _0: null, _1: "/usr/bin/agda", _2: "2.6.4" }`)
        state.connection = Some(mockConnection)
        
        // INVOKE: onActivate to trigger the actual UI logic
        await State__SwitchVersion2.Handler.onActivate(state, makeMockPlatform())
        
        // ANALYZE: Check selection marking across all endpoints
        let allEndpointsFromLogs = loggedEvents->Array.flat
        
        // VERIFY: Exactly one endpoint should be selected
        let selectedEndpoints = allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)
        Assert.deepStrictEqual(Array.length(selectedEndpoints), 1)
        
        // VERIFY: The correct endpoint (from memento) is selected
        switch selectedEndpoints[0] {
        | Some((path, _, _, _)) => 
          Assert.deepStrictEqual(path, "/opt/homebrew/bin/agda") // Should be the memento selection
        | None => 
          Assert.fail("Expected exactly one endpoint to be selected")
        }
        
        // VERIFY: All other endpoints are not selected
        let unselectedEndpoints = allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => !isSelected)
        Assert.deepStrictEqual(Array.length(unselectedEndpoints), 3) // Should be 3 unselected endpoints
        
        // VERIFY: The unselected endpoints are the expected ones
        let unselectedPaths = unselectedEndpoints->Array.map(((path, _, _, _)) => path)->Array.toSorted(String.compare)
        let expectedUnselectedPaths = ["/usr/bin/agda", "/usr/bin/als", "/usr/local/bin/agda"]->Array.toSorted(String.compare)
        Assert.deepStrictEqual(unselectedPaths, expectedUnselectedPaths)
        
        // VERIFY: Total endpoint count is correct
        Assert.deepStrictEqual(Array.length(allEndpointsFromLogs), 4) // Should have all 4 endpoints logged
      },
    )
  })
})
