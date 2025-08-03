open Mocha
open Test__Util

module TestData = {
  // Mock endpoint entries for testing
  let createMockEntry = (endpoint: Memento.Endpoints.endpoint, ~error: option<string>=?, ()): Memento.Endpoints.entry => {
    endpoint: endpoint,
    timestamp: Date.make(),
    error: error,
  }

  let agdaEntry = createMockEntry(Agda(Some("2.6.4")), ())
  let agdaUnknownEntry = createMockEntry(Agda(None), ())
  let alsEntry = createMockEntry(ALS(Some(("4.0.0", "2.6.4"))), ())
  let errorEntry = createMockEntry(Unknown, ~error="Permission denied", ())

  // Test data for pure functions
  let createMockEndpointInfo = (endpoint: Memento.Endpoints.endpoint, ~error: option<string>=?, ()): State__SwitchVersion2.ItemFormatting.endpointInfo => {
    endpoint: endpoint,
    error: error,
  }

  let agdaEndpointInfo = createMockEndpointInfo(Agda(Some("2.6.4")), ())
  let alsEndpointInfo = createMockEndpointInfo(ALS(Some(("4.0.0", "2.6.4"))), ())
  let unknownEndpointInfo = createMockEndpointInfo(Unknown, ~error="Permission denied", ())

  // Simple mock functions for testing
  let createMockMemento = () => Memento.make(None)
  let createMockExtensionUri = () => VSCode.Uri.file("/test/extension")
}

describe("State__SwitchVersion2", () => {
  describe("ItemFormatting", () => {
    describe("formatEndpointInfo", () => {
      it("should format Agda endpoint with version", () => {
        let endpointInfo = TestData.agdaEndpointInfo
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpointInfo("agda", endpointInfo, false)
        
        Assert.deepStrictEqual(label, "Agda v2.6.4")
        Assert.deepStrictEqual(description, "")
      })

      it("should format Agda endpoint without version", () => {
        let endpointInfo = TestData.createMockEndpointInfo(Agda(None), ())
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpointInfo("agda", endpointInfo, false)
        
        Assert.deepStrictEqual(label, "Agda (version unknown)")
        Assert.deepStrictEqual(description, "")
      })

      it("should format ALS endpoint with versions", () => {
        let endpointInfo = TestData.alsEndpointInfo
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpointInfo("als", endpointInfo, false)
        
        Assert.deepStrictEqual(label, "$(squirrel)  ALS v4.0.0, Agda v2.6.4")
        Assert.deepStrictEqual(description, "")
      })

      it("should add Selected when picked", () => {
        let endpointInfo = TestData.agdaEndpointInfo
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpointInfo("agda", endpointInfo, true)
        
        Assert.deepStrictEqual(label, "Agda v2.6.4")
        Assert.deepStrictEqual(description, "Selected")
      })

      it("should format error endpoint", () => {
        let endpointInfo = TestData.unknownEndpointInfo
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpointInfo("broken-agda", endpointInfo, false)
        
        Assert.deepStrictEqual(label, "$(error) broken-agda")
        Assert.deepStrictEqual(description, "Error: Permission denied")
      })

      it("should format unknown endpoint without error", () => {
        let endpointInfo = TestData.createMockEndpointInfo(Unknown, ())
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpointInfo("mystery", endpointInfo, false)
        
        Assert.deepStrictEqual(label, "$(question) mystery")
        Assert.deepStrictEqual(description, "Unknown executable")
      })
    })

    describe("shouldHaveIcon", () => {
      it("should return true for Agda endpoints", () => {
        Assert.deepStrictEqual(State__SwitchVersion2.ItemFormatting.shouldHaveIcon(Agda(Some("2.6.4"))), true)
        Assert.deepStrictEqual(State__SwitchVersion2.ItemFormatting.shouldHaveIcon(Agda(None)), true)
      })

      it("should return false for ALS endpoints", () => {
        Assert.deepStrictEqual(State__SwitchVersion2.ItemFormatting.shouldHaveIcon(ALS(Some(("4.0.0", "2.6.4")))), false)
        Assert.deepStrictEqual(State__SwitchVersion2.ItemFormatting.shouldHaveIcon(ALS(None)), false)
      })

      it("should return false for unknown endpoints", () => {
        Assert.deepStrictEqual(State__SwitchVersion2.ItemFormatting.shouldHaveIcon(Unknown), false)
      })
    })

    describe("formatEndpoint", () => {
      it("should format endpoint from entry", () => {
        let entry = TestData.agdaEntry
        let (label, description) = State__SwitchVersion2.ItemFormatting.formatEndpoint("agda", entry, true)
        
        Assert.deepStrictEqual(label, "Agda v2.6.4")
        Assert.deepStrictEqual(description, "Selected")
      })
    })
  })

  describe("ItemCreation", () => {
    let extensionUri = TestData.createMockExtensionUri()

    describe("createEndpointItem", () => {
      it("should create quickpick item with correct properties", () => {
        let entry = TestData.agdaEntry
        let item = State__SwitchVersion2.ItemCreation.createEndpointItem("/usr/bin/agda", entry, extensionUri, false)
        
        Assert.deepStrictEqual(item.label, "Agda v2.6.4")
        Assert.deepStrictEqual(item.description, Some(""))
        Assert.deepStrictEqual(item.detail, Some("/usr/bin/agda"))
      })

      it("should include icon for Agda endpoints", () => {
        let entry = TestData.agdaEntry
        let item = State__SwitchVersion2.ItemCreation.createEndpointItem("/usr/bin/agda", entry, extensionUri, false)
        
        // Check that iconPath is present for Agda
        switch item.iconPath {
        | Some(_) => () // Expected
        | None => Assert.fail("Expected iconPath for Agda endpoint")
        }
      })

      it("should not include icon for ALS endpoints", () => {
        let entry = TestData.alsEntry
        let item = State__SwitchVersion2.ItemCreation.createEndpointItem("/usr/bin/als", entry, extensionUri, false)
        
        // Check that iconPath is absent for ALS
        switch item.iconPath {
        | None => () // Expected
        | Some(_) => Assert.fail("Did not expect iconPath for ALS endpoint")
        }
      })
    })

    describe("createSeparatorItem", () => {
      it("should create separator with correct kind", () => {
        let item = State__SwitchVersion2.ItemCreation.createSeparatorItem("Test Section")
        
        Assert.deepStrictEqual(item.label, "Test Section")
        Assert.deepStrictEqual(item.kind, Some(VSCode.QuickPickItemKind.Separator))
      })
    })

    describe("createNoInstallationsItem", () => {
      it("should create placeholder item", () => {
        let item = State__SwitchVersion2.ItemCreation.createNoInstallationsItem()
        
        Assert.deepStrictEqual(item.label, "$(info) No installations found")
        Assert.deepStrictEqual(item.description, Some("Try installing Agda or ALS first"))
        Assert.deepStrictEqual(item.detail, Some("No executable paths detected"))
      })
    })

    describe("createOpenFolderItem", () => {
      it("should create open folder item", () => {
        let globalStorageUri = VSCode.Uri.file("/test/global/storage")
        let item = State__SwitchVersion2.ItemCreation.createOpenFolderItem(globalStorageUri)
        
        Assert.deepStrictEqual(item.label, "$(folder-opened)  Open download folder")
        Assert.deepStrictEqual(item.description, Some("Where the language servers are downloaded to"))
        Assert.deepStrictEqual(item.detail, Some("/test/global/storage"))
      })
    })

    describe("createDownloadItem", () => {
      it("should create download item when not downloaded", () => {
        let item = State__SwitchVersion2.ItemCreation.createDownloadItem(false, "ALS v1.0.0")
        
        Assert.deepStrictEqual(item.label, "$(cloud-download)  Download the latest Agda Language Server")
        Assert.deepStrictEqual(item.description, Some(""))
        Assert.deepStrictEqual(item.detail, Some("ALS v1.0.0"))
      })

      it("should create download item when already downloaded", () => {
        let item = State__SwitchVersion2.ItemCreation.createDownloadItem(true, "ALS v1.0.0")
        
        Assert.deepStrictEqual(item.label, "$(cloud-download)  Download the latest Agda Language Server")
        Assert.deepStrictEqual(item.description, Some("Downloaded and installed"))
        Assert.deepStrictEqual(item.detail, Some("ALS v1.0.0"))
      })
    })
  })

  describe("QuickPickManager", () => {
    it("should create quickpick with correct initial state", () => {
      let qp = State__SwitchVersion2.QuickPickManager.make()
      
      Assert.deepStrictEqual(Array.length(qp.items), 0)
      Assert.deepStrictEqual(Array.length(qp.subscriptions), 0)
    })

    it("should update items correctly", () => {
      let qp = State__SwitchVersion2.QuickPickManager.make()
      let items = [State__SwitchVersion2.ItemCreation.createNoInstallationsItem()]
      
      qp->State__SwitchVersion2.QuickPickManager.updateItems(items)
      
      Assert.deepStrictEqual(Array.length(qp.items), 1)
      Assert.deepStrictEqual(qp.items[0]->Option.map(item => item.label), Some("$(info) No installations found"))
    })
  })

  describe("EndpointLogic", () => {
    describe("getPickedPath", () => {
      Async.it("should return picked path from memento", async () => {
        let memento = TestData.createMockMemento()
        await Memento.PickedConnection.set(memento, Some("/usr/bin/agda"))
        
        let pickedPath = State__SwitchVersion2.EndpointLogic.getPickedPath(memento)
        Assert.deepStrictEqual(pickedPath, Some("/usr/bin/agda"))
      })

      it("should return None when no picked path", () => {
        let memento = TestData.createMockMemento()
        let pickedPath = State__SwitchVersion2.EndpointLogic.getPickedPath(memento)
        Assert.deepStrictEqual(pickedPath, None)
      })
    })

    describe("entriesToItems", () => {
      let extensionUri = TestData.createMockExtensionUri()
      let globalStorageUri = VSCode.Uri.file("/test/global/storage")

      it("should return no installations item and misc section when no entries", () => {
        let entries = Dict.make()
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, None, ())
        
        Assert.deepStrictEqual(Array.length(items), 3) // No installations + Misc separator + Open folder
        Assert.deepStrictEqual(items[0]->Option.map(item => item.label), Some("$(info) No installations found"))
        Assert.deepStrictEqual(items[1]->Option.map(item => item.label), Some("Misc"))
        Assert.deepStrictEqual(items[2]->Option.map(item => item.label), Some("$(folder-opened)  Open download folder"))
      })

      it("should create items with separator when entries exist", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        entries->Dict.set("/usr/bin/als", TestData.alsEntry)
        
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, None, ())
        
        Assert.deepStrictEqual(Array.length(items), 5) // Installed separator + 2 endpoints + Misc separator + Open folder
        Assert.deepStrictEqual(items[0]->Option.map(item => item.label), Some("Installed"))
        Assert.deepStrictEqual(items[0]->Option.flatMap(item => item.kind), Some(VSCode.QuickPickItemKind.Separator))
        Assert.deepStrictEqual(items[3]->Option.map(item => item.label), Some("Misc"))
        Assert.deepStrictEqual(items[4]->Option.map(item => item.label), Some("$(folder-opened)  Open download folder"))
      })

      it("should mark picked connection correctly", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        entries->Dict.set("/usr/bin/als", TestData.alsEntry)
        
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, Some("/usr/bin/agda"), ())
        
        let agdaItem = items->Array.find(item => 
          switch item.detail {
          | Some("/usr/bin/agda") => true
          | _ => false
          }
        )
        
        switch agdaItem {
        | Some(item) => 
          switch item.description {
          | Some(desc) => Assert.ok(desc->String.includes("Selected"))
          | None => Assert.fail("Expected description to be present")
          }
        | None => Assert.fail("Could not find Agda item")
        }
      })

      it("should not mark any item when no picked path", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, None, ())
        
        let agdaItem = items->Array.find(item => 
          switch item.detail {
          | Some("/usr/bin/agda") => true
          | _ => false
          }
        )
        
        switch agdaItem {
        | Some(item) => 
          switch item.description {
          | Some(desc) => Assert.ok(!(desc->String.includes("Selected")))
          | None => Assert.fail("Expected description to be present")
          }
        | None => Assert.fail("Could not find Agda item")
        }
      })

      it("should include download section when download item is provided", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        
        let downloadItem = State__SwitchVersion2.ItemCreation.createDownloadItem(false, "ALS v1.0.0")
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, None, ~downloadItem, ())
        
        Assert.deepStrictEqual(Array.length(items), 6) // Installed separator + 1 item + Download separator + download item + Misc separator + Open folder
        
        // Check that download section exists
        let downloadSeparator = items->Array.find(item => item.label == "Download")
        let downloadItemFound = items->Array.find(item => item.label == "$(cloud-download)  Download the latest Agda Language Server")
        
        Assert.ok(downloadSeparator->Option.isSome)
        Assert.ok(downloadItemFound->Option.isSome)
      })
    })

    describe("getPathsNeedingProbe", () => {
      it("should return paths with unknown versions", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry) // Has version
        entries->Dict.set("/usr/bin/agda-unknown", TestData.agdaUnknownEntry) // No version
        entries->Dict.set("/usr/bin/als-unknown", TestData.createMockEntry(ALS(None), ())) // No version
        
        let pathsToProbe = State__SwitchVersion2.EndpointLogic.getPathsNeedingProbe(entries)
        
        Assert.deepStrictEqual(Array.length(pathsToProbe), 2)
        Assert.ok(pathsToProbe->Array.includes("/usr/bin/agda-unknown"))
        Assert.ok(pathsToProbe->Array.includes("/usr/bin/als-unknown"))
        Assert.ok(!(pathsToProbe->Array.includes("/usr/bin/agda")))
      })

      it("should return empty array when no paths need probing", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        entries->Dict.set("/usr/bin/als", TestData.alsEntry)
        
        let pathsToProbe = State__SwitchVersion2.EndpointLogic.getPathsNeedingProbe(entries)
        
        Assert.deepStrictEqual(Array.length(pathsToProbe), 0)
      })
    })

    describe("entriesChanged", () => {
      it("should detect when entries are different objects", () => {
        let entries1 = Dict.make()
        let entries2 = Dict.make()
        
        let changed = State__SwitchVersion2.EndpointLogic.entriesChanged(entries1, entries2)
        Assert.deepStrictEqual(changed, true)
      })

      it("should return false when entries are same object", () => {
        let entries = Dict.make()
        
        let changed = State__SwitchVersion2.EndpointLogic.entriesChanged(entries, entries)
        Assert.deepStrictEqual(changed, false)
      })
    })
  })

  describe("EndpointManager Integration", () => {
    describe("toItems", () => {
      Async.it("should delegate to EndpointLogic.entriesToItems", async () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        
        let memento = TestData.createMockMemento()
        let extensionUri = TestData.createMockExtensionUri()
        await Memento.PickedConnection.set(memento, Some("/usr/bin/agda"))
        
        let manager: State__SwitchVersion2.EndpointManager.t = {
          entries: entries,
          extensionUri: extensionUri,
        }
        
        let items = State__SwitchVersion2.EndpointManager.toItems(manager, memento, extensionUri, ())
        
        Assert.deepStrictEqual(Array.length(items), 4) // Installed separator + 1 item + Misc separator + Open folder
        
        let agdaItem = items->Array.find(item => 
          switch item.detail {
          | Some("/usr/bin/agda") => true
          | _ => false
          }
        )
        
        switch agdaItem {
        | Some(item) => 
          switch item.description {
          | Some(desc) => Assert.ok(desc->String.includes("Selected"))
          | None => Assert.fail("Expected description to be present")
          }
        | None => Assert.fail("Could not find Agda item")
        }
      })
    })
  })

  describe("End-to-End Download Integration", () => {
    describe("Download Item Creation", () => {
      it("should create download item with correct description based on download status", () => {
        // Test when not downloaded
        let item1 = State__SwitchVersion2.ItemCreation.createDownloadItem(false, "ALS v1.0.0")
        Assert.deepStrictEqual(item1.description, Some(""))
        
        // Test when already downloaded  
        let item2 = State__SwitchVersion2.ItemCreation.createDownloadItem(true, "ALS v1.0.0")
        Assert.deepStrictEqual(item2.description, Some("Downloaded and installed"))
      })
    })

    describe("Download Status Logic", () => {
      it("should correctly determine download status from description", () => {
        let notDownloadedItem = State__SwitchVersion2.ItemCreation.createDownloadItem(false, "ALS v1.0.0")
        let downloadedItem = State__SwitchVersion2.ItemCreation.createDownloadItem(true, "ALS v1.0.0")
        
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
      })
    })

    describe("UI Section Layout", () => {
      it("should include download section when download item is provided", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        
        let extensionUri = TestData.createMockExtensionUri()
        let globalStorageUri = VSCode.Uri.file("/test/global/storage")
        let downloadItem = State__SwitchVersion2.ItemCreation.createDownloadItem(false, "ALS v1.0.0")
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, None, ~downloadItem, ())
        
        // Should have: Installed separator + agda item + Download separator + download item + Misc separator + open folder
        Assert.deepStrictEqual(Array.length(items), 6)
        
        // Check that download section exists
        let downloadSeparator = items->Array.find(item => item.label == "Download")
        let downloadItemFound = items->Array.find(item => item.label == "$(cloud-download)  Download the latest Agda Language Server")
        
        Assert.ok(downloadSeparator->Option.isSome)
        Assert.ok(downloadItemFound->Option.isSome)
      })
      
      it("should not include download section when no download item is provided", () => {
        let entries = Dict.make()
        entries->Dict.set("/usr/bin/agda", TestData.agdaEntry)
        
        let extensionUri = TestData.createMockExtensionUri()
        let globalStorageUri = VSCode.Uri.file("/test/global/storage")
        let items = State__SwitchVersion2.EndpointLogic.entriesToItems(entries, extensionUri, globalStorageUri, None, ())
        
        // Should have: Installed separator + agda item + Misc separator + open folder (no download section)
        Assert.deepStrictEqual(Array.length(items), 4)
        
        // Check that no download section exists
        let downloadSeparator = items->Array.find(item => item.label == "Download")
        let downloadItemFound = items->Array.find(item => item.label == "$(cloud-download)  Download the latest Agda Language Server")
        
        Assert.ok(downloadSeparator->Option.isNone)
        Assert.ok(downloadItemFound->Option.isNone)
      })
    })

    describe("Message Formatting", () => {
      it("should format already downloaded message correctly", () => {
        let downloadItem = State__SwitchVersion2.ItemCreation.createDownloadItem(true, "Agda v2.6.4 Language Server v1.0.0")
        let message = downloadItem.detail->Option.getOr("ALS") ++ " is already downloaded"
        
        Assert.ok(String.includes(message, "is already downloaded"))
        Assert.ok(String.includes(message, "Agda v2.6.4 Language Server v1.0.0"))
      })
      
      it("should format successfully downloaded message correctly", () => {
        let downloadItem = State__SwitchVersion2.ItemCreation.createDownloadItem(false, "Agda v2.6.4 Language Server v1.0.0")
        let message = downloadItem.detail->Option.getOr("ALS") ++ " successfully downloaded"
        
        Assert.ok(String.includes(message, "successfully downloaded"))
        Assert.ok(String.includes(message, "Agda v2.6.4 Language Server v1.0.0"))
      })
    })
  })
})