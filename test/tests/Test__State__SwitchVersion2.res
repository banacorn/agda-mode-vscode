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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: Endpoint("/usr/bin/agda", entry),
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: Endpoint("/usr/bin/agda", entry),
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: Endpoint("/usr/bin/als", entry),
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: Separator("Test Section"),
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: NoInstallations,
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: OpenFolder("/test/global/storage"),
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(false, "ALS v1.0.0"),
              isSelected: false,
            }
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
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(true, "ALS v1.0.0"),
              isSelected: false,
            }
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
        let qp = State__SwitchVersion2.View.make()

        Assert.deepStrictEqual(Array.length(qp.items), 0)
        Assert.deepStrictEqual(Array.length(qp.subscriptions), 0)
      },
    )

    it(
      "should update items correctly",
      () => {
        let qp = State__SwitchVersion2.View.make()
        let itemData: State__SwitchVersion2.ItemData.t = {
          itemType: NoInstallations,
          isSelected: false,
        }
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
            Assert.deepStrictEqual(
              itemData[0]->Option.map(item => item.itemType),
              Some(NoInstallations),
            )

            switch itemData[1]->Option.map(item => item.itemType) {
            | Some(Separator("Misc")) => () // Expected
            | _ => Assert.fail("Expected Misc separator")
            }

            switch itemData[2]->Option.map(item => item.itemType) {
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

            switch itemData[0]->Option.map(item => item.itemType) {
            | Some(Separator("Installed")) => () // Expected
            | _ => Assert.fail("Expected Installed separator")
            }

            switch itemData[3]->Option.map(item => item.itemType) {
            | Some(Separator("Misc")) => () // Expected
            | _ => Assert.fail("Expected Misc separator")
            }

            switch itemData[4]->Option.map(item => item.itemType) {
            | Some(OpenFolder("/test/global/storage")) => () // Expected
            | _ => Assert.fail("Expected OpenFolder item")
            }
          },
        )

        it(
          "should mark picked connection correctly",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
            entries->Dict.set("/usr/bin/als", TestData.alsEntry)

            let itemData: array<
              State__SwitchVersion2.ItemData.t,
            > = State__SwitchVersion2.ItemData.entriesToItemData(
              entries,
              Some("/usr/bin/agda"),
              None,
              "/test/global/storage",
            )

            let agdaItemData = itemData->Array.find(
              data =>
                switch data.itemType {
                | Endpoint("/usr/bin/agda", _) => true
                | _ => false
                },
            )

            switch agdaItemData {
            | Some(data) => Assert.deepStrictEqual(data.isSelected, true)
            | None => Assert.fail("Could not find Agda item data")
            }

            let alsItemData = itemData->Array.find(
              data =>
                switch data.itemType {
                | Endpoint("/usr/bin/als", _) => true
                | _ => false
                },
            )

            switch alsItemData {
            | Some(data) => Assert.deepStrictEqual(data.isSelected, false)
            | None => Assert.fail("Could not find ALS item data")
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
                switch data.itemType {
                | Separator("Download") => true
                | _ => false
                },
            )

            let downloadAction = itemData->Array.find(
              data =>
                switch data.itemType {
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

  describe("SwitchVersionManager Integration", () => {
    describe(
      "getItemData",
      () => {
        Async.it(
          "should create item data with correct selection marking",
          async () => {
            // Create a mock state
            let memento = TestData.createMockMemento()
            let globalStorageUri = VSCode.Uri.file("/test/global/storage")
            await Memento.PickedConnection.set(memento, Some("/usr/bin/agda"))

            // Mock entries
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)

            // Create manager manually since we can't easily create a State.t in tests
            let manager: State__SwitchVersion2.SwitchVersionManager.t = {
              entries,
              memento,
              globalStorageUri,
            }

            let itemData =
              await manager->State__SwitchVersion2.SwitchVersionManager.getItemData(None)

            // Should have: "Installed" separator + agda item + "Misc" separator + open folder item
            Assert.deepStrictEqual(Array.length(itemData), 4)

            // Find the Agda endpoint item
            let agdaItemData = itemData->Array.find(
              data =>
                switch data.itemType {
                | Endpoint("/usr/bin/agda", _) => true
                | _ => false
                },
            )

            switch agdaItemData {
            | Some(data) => Assert.deepStrictEqual(data.isSelected, true)
            | None => Assert.fail("Could not find Agda item data")
            }
          },
        )
      },
    )
  })

  describe("End-to-End Download Integration", () => {
    describe(
      "Download Item Creation",
      () => {
        it(
          "should create download item with correct description based on download status",
          () => {
            // Test when not downloaded
            let itemData1: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(false, "ALS v1.0.0"),
              isSelected: false,
            }
            let item1 = State__SwitchVersion2.Item.fromItemData(
              itemData1,
              TestData.createMockExtensionUri(),
            )
            Assert.deepStrictEqual(item1.description, Some(""))

            // Test when already downloaded
            let itemData2: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(true, "ALS v1.0.0"),
              isSelected: false,
            }
            let item2 = State__SwitchVersion2.Item.fromItemData(
              itemData2,
              TestData.createMockExtensionUri(),
            )
            Assert.deepStrictEqual(item2.description, Some("Downloaded and installed"))
          },
        )
      },
    )

    describe(
      "Download Status Logic",
      () => {
        it(
          "should correctly determine download status from description",
          () => {
            let itemData1: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(false, "ALS v1.0.0"),
              isSelected: false,
            }
            let notDownloadedItem = State__SwitchVersion2.Item.fromItemData(
              itemData1,
              TestData.createMockExtensionUri(),
            )
            let itemData2: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(true, "ALS v1.0.0"),
              isSelected: false,
            }
            let downloadedItem = State__SwitchVersion2.Item.fromItemData(
              itemData2,
              TestData.createMockExtensionUri(),
            )

            // Test the same logic used in the actual download click handler
            let isNotDownloaded = switch notDownloadedItem.description {
            | Some("Downloaded and installed") => false
            | _ => true
            }

            let isDownloaded = switch downloadedItem.description {
            | Some("Downloaded and installed") => true
            | _ => false
            }

            Assert.deepStrictEqual(isNotDownloaded, true)
            Assert.deepStrictEqual(isDownloaded, true)
          },
        )
      },
    )

    describe(
      "UI Section Layout",
      () => {
        it(
          "should include download section when download item is provided",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)

            let extensionUri = TestData.createMockExtensionUri()
            let globalStorageUri = VSCode.Uri.file("/test/global/storage")
            let folderPath = VSCode.Uri.fsPath(globalStorageUri)
            let itemDataArray: array<
              State__SwitchVersion2.ItemData.t,
            > = State__SwitchVersion2.ItemData.entriesToItemData(
              entries,
              None,
              Some((false, "ALS v1.0.0")),
              folderPath,
            )
            let items = State__SwitchVersion2.Item.fromItemDataArray(itemDataArray, extensionUri)

            // Should have: Installed separator + agda item + Download separator + download item + Misc separator + open folder
            Assert.deepStrictEqual(Array.length(items), 6)

            // Check that download section exists
            let downloadSeparator = items->Array.find(item => item.label == "Download")
            let downloadItemFound =
              items->Array.find(
                item => item.label == "$(cloud-download)  Download the latest Agda Language Server",
              )

            Assert.ok(downloadSeparator->Option.isSome)
            Assert.ok(downloadItemFound->Option.isSome)
          },
        )

        it(
          "should not include download section when no download item is provided",
          () => {
            let entries = Dict.make()
            entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)

            let extensionUri = TestData.createMockExtensionUri()
            let globalStorageUri = VSCode.Uri.file("/test/global/storage")
            let folderPath = VSCode.Uri.fsPath(globalStorageUri)
            let itemDataArray: array<
              State__SwitchVersion2.ItemData.t,
            > = State__SwitchVersion2.ItemData.entriesToItemData(entries, None, None, folderPath)
            let items = State__SwitchVersion2.Item.fromItemDataArray(itemDataArray, extensionUri)

            // Should have: Installed separator + agda item + Misc separator + open folder (no download section)
            Assert.deepStrictEqual(Array.length(items), 4)

            // Check that no download section exists
            let downloadSeparator = items->Array.find(item => item.label == "Download")
            let downloadItemFound =
              items->Array.find(
                item => item.label == "$(cloud-download)  Download the latest Agda Language Server",
              )

            Assert.ok(downloadSeparator->Option.isNone)
            Assert.ok(downloadItemFound->Option.isNone)
          },
        )
      },
    )

    describe(
      "Message Formatting",
      () => {
        it(
          "should format already downloaded message correctly",
          () => {
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(true, "Agda v2.6.4 Language Server v1.0.0"),
              isSelected: false,
            }
            let downloadItem = State__SwitchVersion2.Item.fromItemData(
              itemData,
              TestData.createMockExtensionUri(),
            )
            let message = downloadItem.detail->Option.getOr("ALS") ++ " is already downloaded"

            Assert.ok(String.includes(message, "is already downloaded"))
            Assert.ok(String.includes(message, "Agda v2.6.4 Language Server v1.0.0"))
          },
        )

        it(
          "should format successfully downloaded message correctly",
          () => {
            let itemData: State__SwitchVersion2.ItemData.t = {
              itemType: DownloadAction(false, "Agda v2.6.4 Language Server v1.0.0"),
              isSelected: false,
            }
            let downloadItem = State__SwitchVersion2.Item.fromItemData(
              itemData,
              TestData.createMockExtensionUri(),
            )
            let message = downloadItem.detail->Option.getOr("ALS") ++ " successfully downloaded"

            Assert.ok(String.includes(message, "successfully downloaded"))
            Assert.ok(String.includes(message, "Agda v2.6.4 Language Server v1.0.0"))
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
        let getInstalledEndpointsAndPersistThem = _ => Promise.resolve(Dict.make())
        let getInstalledEndpointsAndPersistThem2 = _ => Promise.resolve(Dict.make())
        let findCommand = (_command, ~timeout as _timeout=1000) =>
          Promise.resolve(Error(Connection__Command.Error.NotFound("test")))
        let findCommands = _ => Promise.resolve(Error([Connection__Command.Error.NotFound("test")]))
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

    it(
      "onHide should properly destroy view",
      () => {
        let state = createTestState()
        let events = []

        // Subscribe to log channel to capture SwitchVersionUI events
        let _ = state.channels.log->Chan.on(
          logEvent => {
            switch logEvent {
            | State.Log.SwitchVersionUI(event) => events->Array.push(event)
            | _ => ()
            }
          },
        )

        // Test the Handler.onHide function directly
        let view = State__SwitchVersion2.View.make()
        State__SwitchVersion2.Handler.onHide(state, view)

        // Verify the logged events in correct order
        Assert.deepStrictEqual(
          events,
          [Others("Handler.onHide called"), Others("View.destroy completed")],
        )
      },
    )

    Async.it_only(
      "onActivate should have an endpoint marked as selected",
      async () => {
        let state = createTestState()
        let events = []

        // Set up a picked connection so there's something to mark as selected
        await Memento.PickedConnection.set(state.memento, Some("/usr/bin/agda"))

        // Populate endpoints in memento so the manager has entries
        await Memento.Endpoints.setVersion(state.memento, "/usr/bin/agda", Agda(Some("2.6.4")))

        // Subscribe to log channel to capture SwitchVersionUI events
        state.channels.log
        ->Chan.on(
          logEvent => {
            switch logEvent {
            | State.Log.SwitchVersionUI(event) => events->Array.push(event)
            | _ => ()
            }
          },
        )
        ->ignore

        // Trigger onActivate
        await State__SwitchVersion2.Handler.onActivate(state, makeMockPlatform())

        // Verify an endpoint is marked as selected
        Assert.deepStrictEqual(
          events,
          [
            UpdateEndpoints([("/usr/bin/agda", Agda(Some("2.6.4")), None, true)]),
            Others("QuickPick shown"),
          ],
        )
      },
    )

    Async.it_only(
      "should expose real-world selection problem: empty initial state",
      async () => {
        let state = createTestState()
        let events = []

        // Set up picked connection like real usage (user had previously selected agda)
        await Memento.PickedConnection.set(state.memento, Some("/usr/bin/agda"))
        
        // DON'T pre-populate endpoints in memento - this mimics real world where
        // endpoints are discovered through filesystem sync during background update

        // Subscribe to log channel
        state.channels.log
        ->Chan.on(
          logEvent => {
            switch logEvent {
            | State.Log.SwitchVersionUI(event) => events->Array.push(event)
            | _ => ()
            }
          },
        )
        ->ignore

        // Trigger onActivate with empty initial state
        await State__SwitchVersion2.Handler.onActivate(state, makeMockPlatform())

        // In real world: initial UI has no endpoints, so no selection marking
        // Even if background discovery happens, the selection marking logic might fail

        // Check if any endpoint was marked as selected (isSelected: true)
        let hasSelectedEndpoint = events->Array.some(event => {
          switch event {
          | UpdateEndpoints(endpoints) => 
            endpoints->Array.some(((_, _, _, isSelected)) => isSelected)
          | _ => false
          }
        })

        if hasSelectedEndpoint {
          Assert.ok(true) // Selection marking works
        } else {
          Assert.fail("No endpoint was marked as selected despite having picked connection. This exposes the real-world problem where items don't show as 'Selected'.")
        }
      },
    )

    // it(
    //   "should log Handler.onSelection events correctly",
    //   () => {
    //     let state = createTestState()
    //     let loggedEvents = ref([])

    //     // Subscribe to log channel to capture SwitchVersionUI events
    //     let _ = state.channels.log->Chan.on(
    //       logEvent => {
    //         switch logEvent {
    //         | State.Log.SwitchVersionUI(event) => loggedEvents := [event, ...loggedEvents.contents]
    //         | _ => ()
    //         }
    //       },
    //     )

    //     // Test the Handler.onSelection function directly
    //     let view = State__SwitchVersion2.View.make()
    //     let manager = State__SwitchVersion2.SwitchVersionManager.make(state)
    //     let updateUI = async (_downloadInfo: option<(bool, string)>): unit => ()
    //     let mockSelectedItems = []

    //     State__SwitchVersion2.Handler.onSelection(
    //       state,
    //       makeMockPlatform(),
    //       manager,
    //       updateUI,
    //       view,
    //       mockSelectedItems,
    //     )

    //     // Verify the logged events in correct order
    //     let events = loggedEvents.contents->Array.toReversed
    //     Assert.deepStrictEqual(
    //       events,
    //       ["Handler.onSelection called", "View.destroy completed"],
    //     )
    //   },
    // )
  })
})
