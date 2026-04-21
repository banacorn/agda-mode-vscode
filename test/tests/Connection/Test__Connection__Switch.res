open Mocha

describe("Connection__Switch", () => {
  describe("SwitchVersionManager", () => {
    describe("inferCandidateKind", () => {
      it("should recognize als.wasm as ALS candidate", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("als.wasm"),
          Memento.ResolvedMetadata.ALS(WASM, None),
        )
      })

      it("should recognize als as ALS candidate", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("als"),
          Memento.ResolvedMetadata.ALS(Native, None),
        )
      })

      it("should recognize als.exe as ALS candidate", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("als.exe"),
          Memento.ResolvedMetadata.ALS(Native, None),
        )
      })

      it("should recognize agda as Agda candidate", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("agda"),
          Memento.ResolvedMetadata.Agda(None),
        )
      })

      it("should recognize agda-2.6.4 as Agda candidate", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("agda-2.6.4"),
          Memento.ResolvedMetadata.Agda(None),
        )
      })

      it("should recognize unknown executables", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("unknown"),
          Memento.ResolvedMetadata.Unknown,
        )
      })

      it("should extract basename from full paths", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/dev-als/als.wasm",
          ),
          Memento.ResolvedMetadata.ALS(WASM, None),
        )
      })

      it("should infer DevALS native metadata from source-specific cached path", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/dev-als/als-dev-Agda-2.8.0-macos-arm64/als",
          ),
          Memento.ResolvedMetadata.ALS(Native, Some(("dev", "2.8.0", None))),
        )
      })

      it("should infer DevALS WASM metadata from source-specific cached path", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/dev-als/als-dev-Agda-2.8.0-wasm/als.wasm",
          ),
          Memento.ResolvedMetadata.ALS(WASM, Some(("dev", "2.8.0", None))),
        )
      })

      it("should infer stable release native metadata from source-specific cached path", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/stable-als/als-v6-Agda-2.8.0-macos-arm64/als",
          ),
          Memento.ResolvedMetadata.ALS(Native, Some(("v6", "2.8.0", None))),
        )
      })

      it("should infer stable release WASM metadata from source-specific cached path", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/stable-als/als-v6-Agda-2.8.0-wasm/als.wasm",
          ),
          Memento.ResolvedMetadata.ALS(WASM, Some(("v6", "2.8.0", None))),
        )
      })

      it("should infer stable release metadata from release-based managed storage", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/globalStorage/releases/v6/als-v6-Agda-2.8.0-wasm/als.wasm",
          ),
          Memento.ResolvedMetadata.ALS(WASM, Some(("v6", "2.8.0", None))),
        )
      })

      it("should ignore malformed managed release artifact paths", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/globalStorage/releases/v6/als-v6-agda-2.8.0-wasm/als.wasm",
          ),
          Memento.ResolvedMetadata.Unknown,
        )
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind(
            "/path/to/globalStorage/releases/v6/als-v6-Agda-2.8.0-freebsd/als",
          ),
          Memento.ResolvedMetadata.Unknown,
        )
      })

      it("should handle uppercase extensions", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("ALS.WASM"),
          Memento.ResolvedMetadata.ALS(WASM, None),
        )
      })

      it("should recognize als- prefixed executables", () => {
        Assert.deepStrictEqual(
          Connection__Switch.SwitchVersionManager.inferCandidateKind("als-server"),
          Memento.ResolvedMetadata.ALS(Native, None),
        )
      })
    })

    describe("getItemData", () => {
      let mockAgda = ref("")

      Async.before(async () => {
        mockAgda :=
          await Test__Util.Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-switch-raw-key")
      })

      Async.after(async () => {
        await Test__Util.Candidate.Agda.destroy(mockAgda.contents)
      })

      let makeMockPlatform = (): Platform.t => Mock.Platform.makeWithAgda()

      let makeMockPlatformWithBareCommands = (): Platform.t => {
        module MockPlatform = {
          include Desktop.Desktop
          let findCommand = (command, ~timeout as _timeout=1000) =>
            switch command {
            | "agda" => Promise.resolve(Ok(mockAgda.contents))
            | "als" => Promise.resolve(Ok(mockAgda.contents))
            | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
        }
        module(MockPlatform)
      }

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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()
          let configuredCandidates = ["/custom/agda", "vscode-userdata:/global/als.wasm"]
          await Config.Connection.setAgdaPaths(state.channels.log, configuredCandidates)

          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(
            manager,
            [],
            ~platformDeps=Some(platform),
          )
          let candidateRows =
            itemData->Array.filterMap(item =>
              switch item {
              | Connection__UI__ItemData.Candidate(path, _, _, _) => Some(path)
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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          await Config.Connection.setAgdaPaths(state.channels.log, [])

          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(
            manager,
            [],
            ~platformDeps=Some(platform),
          )
          let candidateRows =
            itemData->Array.filterMap(item =>
              switch item {
              | Connection__UI__ItemData.Candidate(path, _, _, _) => Some(path)
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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
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
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(
            manager,
            [],
            ~platformDeps=Some(platform),
          )
          let candidateEntry =
            itemData->Array.findMap(item =>
              switch item {
              | Connection__UI__ItemData.Candidate("agda", detail, entry, _) => Some((detail, entry))
              | _ => None
              }
            )

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

          Assert.deepStrictEqual(
            candidateEntry->Option.map(((detail, entry)) => (detail, entry.kind)),
            Some((
              "agda (" ++ mockAgda.contents ++ ")",
              Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
            )),
          )
        },
      )

      Async.it(
        "should use resolved metadata for resource candidate rows",
        async () => {
          let state = createTestState()
          let manager = Connection__Switch.SwitchVersionManager.make(state)
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
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, [])
          let candidateEntry =
            itemData->Array.findMap(item =>
              switch item {
              | Connection__UI__ItemData.Candidate(path, detail, entry, _)
                when path == resourcePath => Some((detail, entry))
              | _ => None
              }
            )

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

          Assert.deepStrictEqual(
            candidateEntry->Option.map(((detail, entry)) => (detail, entry.kind)),
            Some((
              resourcePath,
              Memento.ResolvedMetadata.ALS(Native, Some(("1.2.3", "2.6.4", None))),
            )),
          )
        },
      )

      Async.it(
        "should mark at most one candidate selected when URI and fsPath aliases coexist",
        async () => {
          let state = createTestState()
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let fsPath = "/tmp/dev-als/als.wasm"
          let uriPath = "file:///tmp/dev-als/als.wasm"

          await Memento.PreferredCandidate.set(state.memento, Some(uriPath))
          await Config.Connection.setAgdaPaths(state.channels.log, [fsPath])

          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(
            manager,
            [],
          )

          let selectedEndpoints =
            itemData->Array.filterMap(item =>
              switch item {
              | Connection__UI__ItemData.Candidate(_, _, _, true) => Some(true)
              | _ => None
              }
            )
          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

          Assert.deepStrictEqual(selectedEndpoints, [true])
        },
      )

      Async.it(
        "SwitchVersionManager.getItemData default header should be Download (Development)",
        async () => {
          let state = createTestState()
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, [])

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
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let nativePath = VSCode.Uri.fsPath(
            VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
          )
          let nativeDir = NodeJs.Path.dirname(nativePath)
          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativePath, NodeJs.Buffer.fromString(""))
          await Config.Connection.setAgdaPaths(state.channels.log, [nativePath])

          let downloadItems = await Connection__Switch.Download.getAllAvailableDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let nativePath = VSCode.Uri.fsPath(
            VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
          )
          let nativeDir = NodeJs.Path.dirname(nativePath)
          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativePath, NodeJs.Buffer.fromString(""))
          let nativeUri = VSCode.Uri.file(nativePath)->VSCode.Uri.toString
          await Config.Connection.setAgdaPaths(state.channels.log, [nativeUri])

          let downloadItems = await Connection__Switch.Download.getAllAvailableDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let nativePath = VSCode.Uri.fsPath(
            VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
          )
          await Config.Connection.setAgdaPaths(state.channels.log, [nativePath])
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(nativePath), false)

          let downloadItems = await Connection__Switch.Download.getAllAvailableDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let nativePath = VSCode.Uri.fsPath(
            VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als"]),
          )
          let nativeUri = VSCode.Uri.file(nativePath)->VSCode.Uri.toString
          await Config.Connection.setAgdaPaths(state.channels.log, [nativeUri])
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(nativePath), false)

          let downloadItems = await Connection__Switch.Download.getAllAvailableDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let wasmPath = VSCode.Uri.toString(
            VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm"]),
          )
          await Config.Connection.setAgdaPaths(state.channels.log, [wasmPath])
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(VSCode.Uri.fsPath(VSCode.Uri.joinPath(storageUri, ["releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm"]))), false)

          let downloadItems = await Connection__Switch.Download.getAllAvailableDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

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
        "should show native download candidate from canonical v6 LatestALS assets on desktop",
        async () => {
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
            let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
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
          let manager = Connection__Switch.SwitchVersionManager.make(state)

          let downloadItems = await Connection__Switch.Download.getAllAvailableDownloads(
            state,
            platform,
            ~channel=Connection__Download.Channel.LatestALS,
          )
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(
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
    })

    describe("probeVersions", () => {
      let mockAgda = ref("")

      Async.before(async () => {
        mockAgda :=
          await Test__Util.Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-probe-raw-key")
      })

      Async.after(async () => {
        await Test__Util.Candidate.Agda.destroy(mockAgda.contents)
      })

      let makeMockPlatform = (): Platform.t => Mock.Platform.makeWithAgda()

      let makeMockPlatformWithBareCommands = (): Platform.t => {
        module MockPlatform = {
          include Desktop.Desktop
          let findCommand = (command, ~timeout as _timeout=1000) =>
            switch command {
            | "agda" => Promise.resolve(Ok(mockAgda.contents))
            | "als" => Promise.resolve(Ok(mockAgda.contents))
            | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
        }
        module(MockPlatform)
      }

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

      Async.it(
        "should write probe metadata under resolved identity for bare command candidates",
        async () => {
          let platform = makeMockPlatformWithBareCommands()
          let state = createTestStateWithPlatform(platform)
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          await Config.Connection.setAgdaPaths(state.channels.log, ["agda"])

          let changed = await Connection__Switch.SwitchVersionManager.probeVersions(
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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          await Config.Connection.setAgdaPaths(state.channels.log, [devALSPath])

          let preProbeItemData = await Connection__Switch.SwitchVersionManager.getItemData(
            manager,
            [],
            ~platformDeps=Some(platform),
          )
          let preProbeKind =
            preProbeItemData->Array.findMap(item =>
              switch item {
              | Connection__UI__ItemData.Candidate(path, _, entry, _) when path == devALSPath => Some(entry.kind)
              | _ => None
              }
            )

          let changed = await Connection__Switch.SwitchVersionManager.probeVersions(
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
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()
          let resolved: Connection__Candidate.Resolved.t = {
            original: Connection__Candidate.make(devALSWasmPath),
            resource: VSCode.Uri.file(devALSWasmPath),
          }

          let _ = await FS.createDirectory(VSCode.Uri.file(parentDir))
          NodeJs.Fs.writeFileSync(devALSWasmPath, NodeJs.Buffer.fromString("wasm"))

          await Config.Connection.setAgdaPaths(state.channels.log, [devALSWasmPath])

          let changed = await Connection__Switch.SwitchVersionManager.probeVersions(
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
    })
  })
})
