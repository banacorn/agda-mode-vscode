open Mocha

// Import the modules we want to test
module VersionDisplay = State__SwitchVersion.VersionDisplay
module SelectionParsing = State__SwitchVersion.SelectionParsing
module DownloadWorkflow = State__SwitchVersion.DownloadWorkflow
module ItemCreation = State__SwitchVersion.ItemCreation

describe("State__SwitchVersion", () => {
  describe("VersionDisplay", () => {
    describe("formatAgdaVersion", () => {
      it("should format Agda version correctly", () => {
        let actual = VersionDisplay.formatAgdaVersion("2.6.4")
        Assert.strictEqual(actual, "Agda v2.6.4")
      })
      
      it("should handle version with patch numbers", () => {
        let actual = VersionDisplay.formatAgdaVersion("2.6.4.1")
        Assert.strictEqual(actual, "Agda v2.6.4.1")
      })
      
      it("should handle development versions", () => {
        let actual = VersionDisplay.formatAgdaVersion("2.7.0-dev")
        Assert.strictEqual(actual, "Agda v2.7.0-dev")
      })
    })
    
    describe("formatALSVersion", () => {
      it("should format ALS version correctly", () => {
        let actual = VersionDisplay.formatALSVersion("1.2.3", "2.6.4")
        Assert.strictEqual(actual, "Agda v2.6.4 Language Server v1.2.3")
      })
      
      it("should handle different version formats", () => {
        let actual = VersionDisplay.formatALSVersion("0.3.16", "2.6.4.1")
        Assert.strictEqual(actual, "Agda v2.6.4.1 Language Server v0.3.16")
      })
    })
    
    describe("formatSwitchingMessage", () => {
      it("should format switching message correctly", () => {
        let actual = VersionDisplay.formatSwitchingMessage("Agda v2.6.4")
        Assert.strictEqual(actual, "Switching to Agda v2.6.4")
      })
      
      it("should work with ALS versions", () => {
        let actual = VersionDisplay.formatSwitchingMessage("Agda v2.6.4 Language Server v1.2.3")
        Assert.strictEqual(actual, "Switching to Agda v2.6.4 Language Server v1.2.3")
      })
    })
    
    describe("formatSwitchedMessage", () => {
      it("should format switched message correctly", () => {
        let actual = VersionDisplay.formatSwitchedMessage("Agda v2.6.4")
        Assert.strictEqual(actual, "Switched to Agda v2.6.4")
      })
      
      it("should work with ALS versions", () => {
        let actual = VersionDisplay.formatSwitchedMessage("Agda v2.6.4 Language Server v1.2.3")
        Assert.strictEqual(actual, "Switched to Agda v2.6.4 Language Server v1.2.3")
      })
    })
  })
  
  describe("SelectionParsing", () => {
    describe("parseSelection", () => {
      it("should parse open folder action", () => {
        let actual = SelectionParsing.parseSelection("$(folder-opened)  Open download folder", None)
        Assert.deepStrictEqual(actual, SelectionParsing.OpenFolder)
      })
      
      it("should parse download ALS action", () => {
        let actual = SelectionParsing.parseSelection("$(cloud-download)  Download the latest Agda Language Server", None)
        Assert.deepStrictEqual(actual, SelectionParsing.DownloadLatestALS)
      })
      
      it("should parse switch to target with detail", () => {
        let actual = SelectionParsing.parseSelection("Agda v2.6.4", Some("/usr/bin/agda"))
        Assert.deepStrictEqual(actual, SelectionParsing.SwitchToTarget("/usr/bin/agda"))
      })
      
      it("should parse switch to target without detail", () => {
        let actual = SelectionParsing.parseSelection("Some unknown label", None)
        Assert.deepStrictEqual(actual, SelectionParsing.SwitchToTarget(""))
      })
      
      it("should handle ALS target selection", () => {
        let actual = SelectionParsing.parseSelection("$(squirrel)  Agda v2.6.4 Language Server v1.2.3", Some("lsp://localhost:4096"))
        Assert.deepStrictEqual(actual, SelectionParsing.SwitchToTarget("lsp://localhost:4096"))
      })
    })
  })
  
  describe("ItemCreation", () => {
    describe("createSeparatorItem", () => {
      it("should create separator item correctly", () => {
        let actual = ItemCreation.createSeparatorItem("Installed")
        Assert.strictEqual(actual.label, "Installed")
        // Check if kind is set to Separator
        switch actual.kind {
        | Some(kind) => Assert.deepStrictEqual(kind, VSCode.QuickPickItemKind.Separator)
        | None => Assert.fail("Expected kind to be set to Separator")
        }
      })
    })
    
    describe("createFolderItem", () => {
      it("should create folder item correctly", () => {
        let actual = ItemCreation.createFolderItem()
        Assert.strictEqual(actual.label, "$(folder-opened)  Open download folder")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "Where the language servers are downloaded to")
        | None => Assert.fail("Expected description to be set")
        }
      })
    })
    
    describe("createDownloadItem", () => {
      it("should create download item for not downloaded version", () => {
        let actual = ItemCreation.createDownloadItem(false, "Agda v2.6.4 Language Server v1.2.3")
        Assert.strictEqual(actual.label, "$(cloud-download)  Download the latest Agda Language Server")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => Assert.strictEqual(detail, "Agda v2.6.4 Language Server v1.2.3")
        | None => Assert.fail("Expected detail to be set")
        }
      })
      
      it("should create download item for already downloaded version", () => {
        let actual = ItemCreation.createDownloadItem(true, "Agda v2.6.4 Language Server v1.2.3")
        Assert.strictEqual(actual.label, "$(cloud-download)  Download the latest Agda Language Server")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "Downloaded and installed")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => Assert.strictEqual(detail, "Agda v2.6.4 Language Server v1.2.3")
        | None => Assert.fail("Expected detail to be set")
        }
      })
    })
    
    describe("createAgdaItem", () => {
      it("should create Agda item for non-selected version", () => {
        let actual = ItemCreation.createAgdaItem("2.6.4", "/usr/bin/agda", false, "/extension/path")
        Assert.strictEqual(actual.label, "Agda v2.6.4")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => Assert.strictEqual(detail, "/usr/bin/agda")
        | None => Assert.fail("Expected detail to be set")
        }
        // iconPath is a complex object, just check it exists
        switch actual.iconPath {
        | Some(_) => Assert.ok(true)
        | None => Assert.fail("Expected iconPath to be set")
        }
      })
      
      it("should create Agda item for selected version", () => {
        let actual = ItemCreation.createAgdaItem("2.6.4", "/usr/bin/agda", true, "/extension/path")
        Assert.strictEqual(actual.label, "Agda v2.6.4")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "Selected")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => Assert.strictEqual(detail, "/usr/bin/agda")
        | None => Assert.fail("Expected detail to be set")
        }
      })
    })
    
    describe("createALSItem", () => {
      it("should create ALS item for TCP connection", () => {
        let mockUrl = {
          "protocol": "lsp:",
          "hostname": "localhost", 
          "port": "4096",
          "toString": () => "lsp://localhost:4096"
        }
        let method = Connection__Target__IPC.ViaTCP(mockUrl->Obj.magic)
        let actual = ItemCreation.createALSItem("1.2.3", "2.6.4", method, false)
        Assert.strictEqual(actual.label, "$(squirrel)  Agda v2.6.4 Language Server v1.2.3")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => Assert.strictEqual(detail, "lsp://localhost:4096")
        | None => Assert.fail("Expected detail to be set")
        }
      })
      
      it("should create ALS item for pipe connection", () => {
        let method = Connection__Target__IPC.ViaPipe("/path/to/als", [], None)
        let actual = ItemCreation.createALSItem("1.2.3", "2.6.4", method, true)
        Assert.strictEqual(actual.label, "$(squirrel)  Agda v2.6.4 Language Server v1.2.3")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "Selected")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => Assert.strictEqual(detail, "/path/to/als")
        | None => Assert.fail("Expected detail to be set")
        }
      })
    })
    
    describe("createErrorItem", () => {
      it("should create error item correctly", () => {
        let mockError = Connection__Target.Error.NotAgdaOrALS(Connection__URI.parse("/bad/path"), "invalid output")
        let actual = ItemCreation.createErrorItem(mockError)
        Assert.strictEqual(actual.label, "$(error)  Bad path")
        switch actual.description {
        | Some(desc) => Assert.strictEqual(desc, "")
        | None => Assert.fail("Expected description to be set")
        }
        switch actual.detail {
        | Some(detail) => {
          Assert.ok(detail->String.includes("/bad/path"))
          Assert.ok(detail->String.includes("doesn't seem to be an Agda executable"))
        }
        | None => Assert.fail("Expected detail to be set")
        }
      })
    })
  })
})