open Mocha

@module("node:fs/promises") external mkdtemp: string => Js.Promise.t<string> = "mkdtemp"

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
            | Connection__Download__Channel.DevALS =>
              Ok(Connection__Download__Source.FromGitHub(Connection__Download__Channel.DevALS, devDescriptor))
            | _ => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
            }
          )
          let download = (_globalStorageUri, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
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

          let downloadItems = await Connection.getAvailableSwitchDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

          let hasNativeDownloadAction =
            itemData->Array.some(item =>
              switch item { | DownloadAction(_, _, platform) => !Connection__Download__DownloadArtifact.Platform.isWasm(platform) | _ => false }
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

          let downloadItems = await Connection.getAvailableSwitchDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

          let hasNativeDownloadAction =
            itemData->Array.some(item =>
              switch item { | DownloadAction(_, _, platform) => !Connection__Download__DownloadArtifact.Platform.isWasm(platform) | _ => false }
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

          let downloadItems = await Connection.getAvailableSwitchDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

          let hasNativeDownloadAction =
            itemData->Array.some(item =>
              switch item { | DownloadAction(_, _, platform) => !Connection__Download__DownloadArtifact.Platform.isWasm(platform) | _ => false }
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

          let downloadItems = await Connection.getAvailableSwitchDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

          let hasNativeDownloadAction =
            itemData->Array.some(item =>
              switch item { | DownloadAction(_, _, platform) => !Connection__Download__DownloadArtifact.Platform.isWasm(platform) | _ => false }
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

          let downloadItems = await Connection.getAvailableSwitchDownloads(state, platform)
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(manager, downloadItems)

          let hasWasmDownloadAction =
            itemData->Array.some(item =>
              switch item { | DownloadAction(_, _, platform) => Connection__Download__DownloadArtifact.Platform.isWasm(platform) | _ => false }
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
              | Connection__Download__Channel.LatestALS =>
                Ok(
                  Connection__Download__Source.FromGitHub(
                    Connection__Download__Channel.LatestALS,
                    latestDescriptor,
                  ),
                )
              | _ => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, _, ~trace as _=Connection__Download__Trace.noop) =>
              Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
            let findCommand = (_command, ~timeout as _=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          let platform: Platform.t = module(MockLatestALSv6)
          let state = createTestStateWithPlatform(platform)
          let manager = Connection__Switch.SwitchVersionManager.make(state)

          let downloadItems = await Connection.getAvailableSwitchDownloads(
            state,
            platform,
            ~channel=Connection__Download__Channel.LatestALS,
          )
          let itemData = await Connection__Switch.SwitchVersionManager.getItemData(
            manager,
            downloadItems,
          )

          let hasNativeDownloadAction =
            itemData->Array.some(item =>
              switch item {
              | DownloadAction(_, _, platform) => !Connection__Download__DownloadArtifact.Platform.isWasm(platform)
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

      let observeProbeFlow = (state: State.t) => {
        let events: ref<array<Log.Connection.ProbeFlow.t>> = ref([])
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.Connection(Log.Connection.ProbeFlow(event)) =>
            events.contents = Array.concat(events.contents, [event])
          | _ => ()
          }
        )
        events
      }

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
          let executableName = OS.onUnix ? "als" : "als.bat"
          let devALSPath = NodeJs.Path.join([
            storagePath,
            "dev-als",
            "als-dev-Agda-2.8.0-macos-arm64",
            executableName,
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

          let probeFlowEvents = observeProbeFlow(state)

          let changed = await Connection__Switch.SwitchVersionManager.probeVersions(
            manager,
            platform,
          )

          let resolved: Connection__Candidate.Resolved.t = {
            original: Connection__Candidate.make(devALSPath),
            resource: VSCode.Uri.file(devALSPath),
          }
          let expectedCandidate = Connection__Candidate.make(devALSPath)
          let expectedResolvedResource = switch expectedCandidate {
          | Connection__Candidate.Resource(uri) => uri
          | Connection__Candidate.Command(_) =>
            raise(Failure("Expected DevALS path candidate to parse as resource"))
          }
          let observedProbeFlow =
            probeFlowEvents.contents->Array.map(event =>
              switch event {
              | Log.Connection.ProbeFlow.CandidateResolveStarted(candidate) =>
                ("CandidateResolveStarted", Connection__Candidate.toString(candidate), "", "")
              | Log.Connection.ProbeFlow.CandidateResolved(candidate, resource) =>
                (
                  "CandidateResolved",
                  Connection__Candidate.toString(candidate),
                  VSCode.Uri.toString(resource),
                  "",
                )
              | Log.Connection.ProbeFlow.ProbeStarted(resource) =>
                ("ProbeStarted", VSCode.Uri.toString(resource), "", "")
              | Log.Connection.ProbeFlow.ProbeClassifiedAsALS(path, alsVersion, agdaVersion) =>
                ("ProbeClassifiedAsALS", path, alsVersion, agdaVersion)
              | unexpected =>
                ("Unexpected", Log.Connection.ProbeFlow.toString(unexpected), "", "")
              }
            )

          Assert.deepStrictEqual(
            preProbeKind,
            Some(Memento.ResolvedMetadata.ALS(Native, Some(("dev", "2.8.0", None)))),
          )
          Assert.deepStrictEqual(
            observedProbeFlow,
            [
              (
                "CandidateResolveStarted",
                Connection__Candidate.toString(expectedCandidate),
                "",
                "",
              ),
              (
                "CandidateResolved",
                Connection__Candidate.toString(expectedCandidate),
                VSCode.Uri.toString(expectedResolvedResource),
                "",
              ),
              ("ProbeStarted", VSCode.Uri.toString(expectedResolvedResource), "", ""),
              (
                "ProbeClassifiedAsALS",
                VSCode.Uri.fsPath(expectedResolvedResource),
                "1.2.3",
                "2.8.0",
              ),
            ],
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

      Async.it(
        "should emit ProbeFlow events including ProbeFailed when probing a non-executable file",
        async () => {
          let nonExecPath = NodeJs.Path.join([
            NodeJs.Os.tmpdir(),
            "agda-probe-fail-test-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".txt",
          ])
          NodeJs.Fs.writeFileSync(nonExecPath, NodeJs.Buffer.fromString("not an executable"))

          let platform = makeMockPlatform()
          let state = createTestStateWithPlatform(platform)
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()
          let probeFlowEvents = observeProbeFlow(state)

          await Config.Connection.setAgdaPaths(state.channels.log, [nonExecPath])

          let changed = await Connection__Switch.SwitchVersionManager.probeVersions(
            manager,
            platform,
          )

          let pathKey = VSCode.Uri.fsPath(VSCode.Uri.file(nonExecPath))
          let resourceUri = VSCode.Uri.toString(VSCode.Uri.file(nonExecPath))
          let resolved: Connection__Candidate.Resolved.t = {
            original: Connection__Candidate.make(nonExecPath),
            resource: VSCode.Uri.file(nonExecPath),
          }

          let observedProbeFlow =
            probeFlowEvents.contents->Array.map(event =>
              switch event {
              | Log.Connection.ProbeFlow.CandidateResolveStarted(candidate) =>
                ("CandidateResolveStarted", Connection__Candidate.toString(candidate), "", "")
              | Log.Connection.ProbeFlow.CandidateResolved(candidate, resource) =>
                (
                  "CandidateResolved",
                  Connection__Candidate.toString(candidate),
                  VSCode.Uri.toString(resource),
                  "",
                )
              | Log.Connection.ProbeFlow.ProbeStarted(resource) =>
                ("ProbeStarted", VSCode.Uri.toString(resource), "", "")
              | Log.Connection.ProbeFlow.ProbeFailed(pk, _error) =>
                ("ProbeFailed", pk, "", "")
              | unexpected =>
                ("Unexpected", Log.Connection.ProbeFlow.toString(unexpected), "", "")
              }
            )

          let mementoEntry = Memento.ResolvedMetadata.get(state.memento, resolved)

          let testResult = try {
            Assert.deepStrictEqual(
              observedProbeFlow,
              [
                ("CandidateResolveStarted", resourceUri, "", ""),
                ("CandidateResolved", resourceUri, resourceUri, ""),
                ("ProbeStarted", resourceUri, "", ""),
                ("ProbeFailed", pathKey, "", ""),
              ],
            )
            Assert.deepStrictEqual(changed, true)
            Assert.deepStrictEqual(
              mementoEntry->Option.map(e => e.kind),
              Some(Memento.ResolvedMetadata.Unknown),
            )
            Assert.deepStrictEqual(
              mementoEntry->Option.map(e => Option.isSome(e.error)),
              Some(true),
            )
            Ok(())
          } catch {
          | _ as e => Error(e)
          }

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
          try { NodeJs.Fs.unlinkSync(nonExecPath) } catch { | _ => () }

          switch testResult {
          | Ok(_) => ()
          | Error(e) => raise(e)
          }
        },
      )

      Async.it(
        "should emit CandidateResolveFailed ProbeFlow event when a bare command cannot be resolved",
        async () => {
          let bareCommand = "agda-probe-command-fail-test"
          let platform = makeMockPlatform()
          let state = createTestStateWithPlatform(platform)
          let manager = Connection__Switch.SwitchVersionManager.make(state)
          let previousPaths = Config.Connection.getAgdaPaths()

          let unrelatedResolved: Connection__Candidate.Resolved.t = {
            original: Connection__Candidate.make("/usr/bin/agda-unrelated"),
            resource: VSCode.Uri.file("/usr/bin/agda-unrelated"),
          }
          await Memento.ResolvedMetadata.setKind(
            state.memento,
            unrelatedResolved,
            Memento.ResolvedMetadata.Agda(Some("2.6.0")),
          )

          let probeFlowEvents = observeProbeFlow(state)

          await Config.Connection.setAgdaPaths(state.channels.log, [bareCommand])

          let changed = await Connection__Switch.SwitchVersionManager.probeVersions(
            manager,
            platform,
          )

          let observedProbeFlow =
            probeFlowEvents.contents->Array.map(event =>
              switch event {
              | Log.Connection.ProbeFlow.CandidateResolveStarted(candidate) =>
                ("CandidateResolveStarted", Connection__Candidate.toString(candidate), "", "")
              | Log.Connection.ProbeFlow.CandidateResolveFailed(candidate, error) =>
                (
                  "CandidateResolveFailed",
                  Connection__Candidate.toString(candidate),
                  Connection__Command.Error.toString(error),
                  "",
                )
              | unexpected =>
                ("Unexpected", Log.Connection.ProbeFlow.toString(unexpected), "", "")
              }
            )

          let testResult = try {
            Assert.deepStrictEqual(
              observedProbeFlow,
              [
                ("CandidateResolveStarted", bareCommand, "", ""),
                ("CandidateResolveFailed", bareCommand, "Command not found", ""),
              ],
            )
            Assert.deepStrictEqual(changed, true)
            Assert.deepStrictEqual(
              Memento.ResolvedMetadata.get(state.memento, unrelatedResolved)->Option.map(
                e => e.kind,
              ),
              Some(Memento.ResolvedMetadata.Agda(Some("2.6.0"))),
            )
            Assert.deepStrictEqual(
              Array.length(Memento.ResolvedMetadata.entries(state.memento)->Dict.toArray),
              1,
            )
            Ok(())
          } catch {
          | _ as e => Error(e)
          }

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

          switch testResult {
          | Ok(_) => ()
          | Error(e) => raise(e)
          }
        },
      )
    })

    describe("activate and switching", () => {
      module IntegrationTestData = {
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

      let mockAgda = ref("")

      Async.before(async () => {
        mockAgda :=
          await Test__Util.Candidate.Agda.mock(
            ~version="2.7.0.1",
            ~name="agda-switch-integration-raw-key",
          )
      })

      Async.after(async () => {
        await Test__Util.Candidate.Agda.destroy(mockAgda.contents)
      })

      beforeEach(() => {
        Registry__Connection.status := Empty
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

      let observeConnectionEvents = (state: State.t): ref<array<Log.Connection.t>> => {
        let events: ref<array<Log.Connection.t>> = ref([])
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.Connection(connectionEvent) =>
            events.contents = Array.concat(events.contents, [connectionEvent])
          | _ => ()
          }
        )
        events
      }

      let probeFlowEvents = (observedConnections: ref<array<Log.Connection.t>>) =>
        observedConnections.contents->Array.filterMap(connectionEvent =>
          switch connectionEvent {
          | Log.Connection.ProbeFlow(event) => Some(event)
          | _ => None
          }
        )

      let establishFlowEvents = (observedConnections: ref<array<Log.Connection.t>>) =>
        observedConnections.contents->Array.filterMap(connectionEvent =>
          switch connectionEvent {
          | Log.Connection.EstablishFlow(event) => Some(event)
          | _ => None
          }
        )

      let switchUIFlowEvents = (observedConnections: ref<array<Log.Connection.t>>) =>
        observedConnections.contents->Array.filterMap(connectionEvent =>
          switch connectionEvent {
          | Log.Connection.SwitchUIFlow(event) => Some(event)
          | _ => None
          }
        )

      // Records every `child_process.spawn` call (arguments and cwd) until the
      // returned function is called, which restores `spawn` and returns the log.
      let spySpawn: unit => (unit => array<{"args": array<string>, "cwd": Js.Undefined.t<string>}>) = %raw(`() => {
        const cp = require("node:child_process");
        const original = cp.spawn;
        const calls = [];
        cp.spawn = function (command, args, options) {
          calls.push({ args: args, cwd: options && options.cwd });
          return original.apply(this, arguments);
        };
        return () => {
          cp.spawn = original;
          return calls;
        };
      }`)

      // `switchCandidate` must start Agda in the directory chosen for the
      // document (issue #152), not in the extension host's cwd.
      Async.it("should start the switched-to Agda in the document's directory", async () => {
        let documentPath = NodeJs.Path.join([NodeJs.Os.tmpdir(), "Issue152Wiring.agda"])
        let mockEditor: VSCode.TextEditor.t = %raw(`(fileName, uri) => ({ document: { fileName, uri } })`)(
          documentPath,
          VSCode.Uri.file(documentPath),
        )
        let state = State.make(
          "test-id",
          makeMockPlatformWithBareCommands(),
          {
            State.inputMethod: Chan.make(),
            responseHandled: Chan.make(),
            commandHandled: Chan.make(),
            log: Chan.make(),
          },
          VSCode.Uri.file(NodeJs.Os.tmpdir()),
          VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process)),
          Memento.make(None),
          mockEditor,
          None,
        )

        let stopSpy = spySpawn()
        let calls = switch await Connection.switchCandidate(state, "agda") {
        | () => stopSpy()
        | exception exn =>
          let _ = stopSpy()
          raise(exn)
        }

        let interactionCwds =
          calls
          ->Array.filter(call => call["args"]->Array.includes("--interaction"))
          ->Array.map(call => call["cwd"])
        Assert.deepStrictEqual(interactionCwds, [Js.Undefined.return(NodeJs.Os.tmpdir())])
      })

      Async.it(
        "should keep existing shared connection when switch target cannot be established",
        async () => {
          let state = createTestState()
          let existingPath = "/usr/bin/agda"
          let existingConnection = IntegrationTestData.makeMockConnection(existingPath, "2.6.4")

          Registry__Connection.status :=
            Active({
              connection: existingConnection,
              users: Belt.Set.String.fromArray(["owner-before-switch"]),
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          let missingPath = "/__agda_mode_vscode_nonexistent__/binary_should_not_exist_280"
          let Connection__URI.FileURI(_, missingUri) = Connection__URI.parse(missingPath)
          let missingPathKey = VSCode.Uri.fsPath(missingUri)
          let missingUriString = VSCode.Uri.toString(missingUri)
          let observedConnections = observeConnectionEvents(state)

          let completion =
            Connection.switchCandidate(state, missingPath)->Promise.thenResolve(_ => "done")
          let timeout = Util.Promise_.setTimeout(1000)->Promise.thenResolve(_ => "timeout")
          let winner = await Promise.race([completion, timeout])

          let observedProbeFlow = probeFlowEvents(observedConnections)->Array.map(event =>
            switch event {
            | Log.Connection.ProbeFlow.CandidateResolveStarted(candidate) =>
              ("CandidateResolveStarted", Connection__Candidate.toString(candidate), "", "")
            | Log.Connection.ProbeFlow.CandidateResolved(candidate, resource) =>
              ("CandidateResolved", Connection__Candidate.toString(candidate), VSCode.Uri.toString(resource), "")
            | Log.Connection.ProbeFlow.ProbeStarted(resource) =>
              ("ProbeStarted", VSCode.Uri.toString(resource), "", "")
            | Log.Connection.ProbeFlow.ProbeFailed(pk, _error) =>
              ("ProbeFailed", pk, "", "")
            | unexpected =>
              ("Unexpected", Log.Connection.ProbeFlow.toString(unexpected), "", "")
            }
          )
          let observedEstablishFlow = establishFlowEvents(observedConnections)->Array.map(event =>
            switch event {
            | Log.Connection.EstablishFlow.CandidateAttempted(pathOrCommand, source) =>
              (
                "CandidateAttempted",
                pathOrCommand,
                Connection.Error.Establish.pathSourceToString(source),
              )
            | Log.Connection.EstablishFlow.ConnectionEstablishFailed =>
              ("ConnectionEstablishFailed", "", "")
            | unexpected =>
              ("Unexpected", Log.Connection.EstablishFlow.toString(unexpected), "")
            }
          )
          let observedSwitchUIFlow = switchUIFlowEvents(observedConnections)->Array.map(event =>
            switch event {
            | Log.Connection.SwitchUIFlow.SwitchRequested(path) =>
              ("SwitchRequested", path)
            | Log.Connection.SwitchUIFlow.SwitchSucceeded(path) =>
              ("SwitchSucceeded", path)
            | Log.Connection.SwitchUIFlow.SwitchFailed(path) =>
              ("SwitchFailed", path)
            }
          )

          switch winner {
          | "done" =>
            Assert.deepStrictEqual(observedProbeFlow, [
              ("CandidateResolveStarted", missingUriString, "", ""),
              ("CandidateResolved", missingUriString, missingUriString, ""),
              ("ProbeStarted", missingUriString, "", ""),
              ("ProbeFailed", missingPathKey, "", ""),
            ])
            Assert.deepStrictEqual(observedEstablishFlow, [
              ("CandidateAttempted", missingPath, "from config"),
              ("ConnectionEstablishFailed", "", ""),
            ])
            Assert.deepStrictEqual(observedSwitchUIFlow, [
              ("SwitchRequested", missingPath),
              ("SwitchFailed", missingPath),
            ])
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
        "should write switch metadata under resolved identity for bare command selection",
        async () => {
          let platform = makeMockPlatformWithBareCommands()
          let state = createTestStateWithPlatform(platform)

          let mockAgdaUri = VSCode.Uri.file(mockAgda.contents)
          let mockAgdaUriString = VSCode.Uri.toString(mockAgdaUri)
          let mockAgdaFsPath = VSCode.Uri.fsPath(mockAgdaUri)
          let observedConnections = observeConnectionEvents(state)

          await Connection.switchCandidate(state, "agda")

          let observedProbeFlow = probeFlowEvents(observedConnections)->Array.map(event =>
            switch event {
            | Log.Connection.ProbeFlow.CandidateResolveStarted(candidate) =>
              ("CandidateResolveStarted", Connection__Candidate.toString(candidate), "", "")
            | Log.Connection.ProbeFlow.CandidateResolved(candidate, resource) =>
              ("CandidateResolved", Connection__Candidate.toString(candidate), VSCode.Uri.toString(resource), "")
            | Log.Connection.ProbeFlow.ProbeStarted(resource) =>
              ("ProbeStarted", VSCode.Uri.toString(resource), "", "")
            | Log.Connection.ProbeFlow.ProbeClassifiedAsAgda(path, version) =>
              ("ProbeClassifiedAsAgda", path, version, "")
            | unexpected =>
              ("Unexpected", Log.Connection.ProbeFlow.toString(unexpected), "", "")
            }
          )
          let observedEstablishFlow = establishFlowEvents(observedConnections)->Array.map(event =>
            switch event {
            | Log.Connection.EstablishFlow.CandidateAttempted(pathOrCommand, source) =>
              (
                "CandidateAttempted",
                pathOrCommand,
                Connection.Error.Establish.pathSourceToString(source),
                "",
              )
            | Log.Connection.EstablishFlow.ConnectionCreated(path, kind) =>
              (
                "ConnectionCreated",
                path,
                switch kind {
                | Log.Connection.EstablishFlow.Agda => "Agda"
                | Log.Connection.EstablishFlow.ALS => "ALS"
                | Log.Connection.EstablishFlow.ALSWASM => "ALSWASM"
                },
                "",
              )
            | unexpected =>
              ("Unexpected", Log.Connection.EstablishFlow.toString(unexpected), "", "")
            }
          )
          let observedSwitchUIFlow = switchUIFlowEvents(observedConnections)->Array.map(event =>
            switch event {
            | Log.Connection.SwitchUIFlow.SwitchRequested(path) =>
              ("SwitchRequested", path)
            | Log.Connection.SwitchUIFlow.SwitchSucceeded(path) =>
              ("SwitchSucceeded", path)
            | Log.Connection.SwitchUIFlow.SwitchFailed(path) =>
              ("SwitchFailed", path)
            }
          )

          Assert.deepStrictEqual(observedProbeFlow, [
            ("CandidateResolveStarted", "agda", "", ""),
            ("CandidateResolved", "agda", mockAgdaUriString, ""),
            ("ProbeStarted", mockAgdaUriString, "", ""),
            ("ProbeClassifiedAsAgda", mockAgdaFsPath, "2.7.0.1", ""),
          ])
          Assert.deepStrictEqual(observedEstablishFlow, [
            ("CandidateAttempted", "agda", "from config", ""),
            ("ConnectionCreated", mockAgdaFsPath, "Agda", ""),
          ])
          Assert.deepStrictEqual(observedSwitchUIFlow, [
            ("SwitchRequested", "agda"),
            ("SwitchSucceeded", mockAgdaFsPath),
          ])

          let orderedKeys = observedConnections.contents->Array.filterMap(event =>
            switch event {
            | Log.Connection.EstablishFlow(Log.Connection.EstablishFlow.ConnectionCreated(path, _)) =>
              Some("ConnectionCreated:" ++ path)
            | Log.Connection.SwitchUIFlow(Log.Connection.SwitchUIFlow.SwitchSucceeded(path)) =>
              Some("SwitchSucceeded:" ++ path)
            | Log.Connection.Disconnected(path) =>
              Some("Disconnected:" ++ path)
            | _ => None
            }
          )
          Assert.deepStrictEqual(orderedKeys, [
            "ConnectionCreated:" ++ mockAgdaFsPath,
            "SwitchSucceeded:" ++ mockAgdaFsPath,
            "Disconnected:" ++ mockAgdaFsPath,
          ])

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
        },
      )

      Async.it(
        "should emit ConnectionFinalizeFailed when post-establish finalization throws",
        async () => {
          let platform = makeMockPlatformWithBareCommands()
          let state = createTestStateWithPlatform(platform)
          let mockAgdaUri = VSCode.Uri.file(mockAgda.contents)
          let mockAgdaFsPath = VSCode.Uri.fsPath(mockAgdaUri)
          let observedConnections = observeConnectionEvents(state)

          // Inject a failing setKind to trigger the post-establish catch block
          let restoreSetKind: unit => unit = %raw(`
            (() => {
              const original = Memento$AgdaModeVscode.Module.ResolvedMetadata.setKind;
              Memento$AgdaModeVscode.Module.ResolvedMetadata.setKind = () => Promise.reject(new Error("finalize injection"));
              return () => { Memento$AgdaModeVscode.Module.ResolvedMetadata.setKind = original; };
            })()
          `)

          let completion =
            Connection.switchCandidate(state, "agda")
            ->Promise.catch(_ => Promise.resolve())
            ->Promise.thenResolve(_ => "done")
          let timeout = Util.Promise_.setTimeout(1000)->Promise.thenResolve(_ => "timeout")
          let winner = await Promise.race([completion, timeout])

          try {
            let observedEstablishFlow = establishFlowEvents(observedConnections)->Array.map(event =>
              switch event {
              | Log.Connection.EstablishFlow.CandidateAttempted(pathOrCommand, source) =>
                (
                  "CandidateAttempted",
                  pathOrCommand,
                  Connection.Error.Establish.pathSourceToString(source),
                  "",
                )
              | Log.Connection.EstablishFlow.ConnectionFinalizeFailed(path, kind) =>
                (
                  "ConnectionFinalizeFailed",
                  path,
                  switch kind {
                  | Log.Connection.EstablishFlow.Agda => "Agda"
                  | Log.Connection.EstablishFlow.ALS => "ALS"
                  | Log.Connection.EstablishFlow.ALSWASM => "ALSWASM"
                  },
                  "",
                )
              | unexpected =>
                ("Unexpected", Log.Connection.EstablishFlow.toString(unexpected), "", "")
              }
            )
            let observedSwitchUIFlow = switchUIFlowEvents(observedConnections)->Array.map(event =>
              switch event {
              | Log.Connection.SwitchUIFlow.SwitchRequested(path) => ("SwitchRequested", path)
              | Log.Connection.SwitchUIFlow.SwitchSucceeded(path) => ("SwitchSucceeded", path)
              | Log.Connection.SwitchUIFlow.SwitchFailed(path) => ("SwitchFailed", path)
              }
            )

            switch winner {
            | "done" =>
              Assert.deepStrictEqual(observedEstablishFlow, [
                ("CandidateAttempted", "agda", "from config", ""),
                ("ConnectionFinalizeFailed", mockAgdaFsPath, "Agda", ""),
              ])
              Assert.deepStrictEqual(observedSwitchUIFlow, [
                ("SwitchRequested", "agda"),
                ("SwitchFailed", "agda"),
              ])
            | "timeout" =>
              Registry__Connection.status := Empty
              Assert.fail("switchAgdaVersion hung during post-establish failure")
            | _ => Assert.fail("Unexpected race result")
            }
            restoreSetKind()
          } catch {
          | exn =>
            restoreSetKind()
            raise(exn)
          }
        },
      )

      Async.it(
        "should mark the active connection candidate selected when no preferred candidate is stored",
        async () => {
          let state = createTestState()
          let loggedEvents = []
          let previousPaths = Config.Connection.getAgdaPaths()

          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedCandidates(candidates)) =>
              loggedEvents->Array.push(candidates)
            | _ => ()
            }
          )

          await Memento.PreferredCandidate.set(state.memento, None)
          await Config.Connection.setAgdaPaths(state.channels.log, ["/usr/bin/agda"])

          let mockConnection = IntegrationTestData.makeMockConnection("/usr/bin/agda", "2.6.4")
          Registry__Connection.status :=
            Active({
              connection: mockConnection,
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          await Connection.activateSwitchVersion(state, makeMockPlatform())

          let allEndpointsFromLogs = loggedEvents->Array.flat
          let anyEndpointSelected =
            allEndpointsFromLogs->Array.some(((_, _, _, isSelected)) => isSelected)

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
          Assert.ok(anyEndpointSelected)
        },
      )

      Async.it(
        "should prefer memento selection over active connection inference",
        async () => {
          let state = createTestState()
          let loggedEvents = []
          let previousPaths = Config.Connection.getAgdaPaths()

          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(UpdatedCandidates(candidates)) =>
              loggedEvents->Array.push(candidates)
            | _ => ()
            }
          )

          await Config.Connection.setAgdaPaths(
            state.channels.log,
            ["/usr/bin/agda", "/opt/homebrew/bin/agda"],
          )
          await Memento.PreferredCandidate.set(state.memento, Some("/usr/bin/agda"))

          let mockConnection = IntegrationTestData.makeMockConnection("/opt/homebrew/bin/agda", "2.6.3")
          Registry__Connection.status :=
            Active({
              connection: mockConnection,
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          await Connection.activateSwitchVersion(state, makeMockPlatform())

          let allEndpointsFromLogs = loggedEvents->Array.flat
          let selectedEndpoint =
            allEndpointsFromLogs->Array.find(((_, _, _, isSelected)) => isSelected)
          let selectedCount =
            allEndpointsFromLogs->Array.filter(((_, _, _, isSelected)) => isSelected)->Array.length

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)

          switch selectedEndpoint {
          | Some((path, _, _, _)) => Assert.deepStrictEqual(path, "/usr/bin/agda")
          | None => Assert.fail("Expected one candidate to be marked as selected")
          }

          Assert.deepStrictEqual(selectedCount, 1)
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

          await Connection.activateSwitchVersion(state, makeMockPlatform())
          await onShown

          Assert.ok(Array.length(loggedHeaders) > 0)
          Assert.deepStrictEqual(loggedHeaders[0], Some("Download (Latest)"))
        },
      )

      Async.it(
        "should replace placeholder download items with unavailable items when background refresh fails",
        async () => {
          let storagePath = await mkdtemp(NodeJs.Path.join([NodeJs.Os.tmpdir(), "agda-switch-bg-"]))
          let storageUri = VSCode.Uri.file(storagePath)
          let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
          let previousPaths = Config.Connection.getAgdaPaths()
          await Config.Connection.setAgdaPaths(state.channels.log, [])

          module MockPlatform = {
            let determinePlatform = () =>
              Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))
            let findCommand = (_, ~timeout as _=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
            let alreadyDownloaded = _ => Promise.resolve(None)
            let resolveDownloadChannel = (_, _) =>
              async (_, _, _) => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
            let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
              Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
            let askUserAboutDownloadPolicy = () =>
              Promise.resolve(Config.Connection.DownloadPolicy.Yes)
          }

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

          let (downloadItemsDeferred, _, rejectDownloadItems) = Util.Promise_.pending()
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

          let result = try {
            await Connection.activateSwitchVersion(
              state,
              module(MockPlatform),
              ~downloadItemsPromiseOverride=Some(downloadItemsDeferred),
            )
            let timeoutP = withTimeout(2000, bgDone)
            rejectDownloadItems(Failure("simulated background download-items failure"))
            await timeoutP
            Ok()
          } catch {
          | exn => Error(exn)
          }

          await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
          let _ = await FS.deleteRecursive(storageUri)

          switch result {
          | Error(exn) => raise(exn)
          | Ok() =>
            Assert.deepStrictEqual(
              downloadItemLogs.contents[0],
              Some([
                (false, Connection__UI__Labels.checkingAvailability, "native"),
                (false, Connection__UI__Labels.checkingAvailability, "wasm"),
              ]),
            )
            Assert.deepStrictEqual(
              downloadItemLogs.contents[1],
              Some([
                (false, Connection__UI__Labels.downloadUnavailable, "native"),
                (false, Connection__UI__Labels.downloadUnavailable, "wasm"),
              ]),
            )
          }
        },
      )
    })
  })

  describe("deleteDownloads", () => {
    let createTestState = (storageUri: VSCode.Uri.t) => {
      let channels = {
        State.inputMethod: Chan.make(),
        responseHandled: Chan.make(),
        commandHandled: Chan.make(),
        log: Chan.make(),
      }
      let mockEditor = %raw(`{ document: { fileName: "test.agda" } }`)
      let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))
      State.make(
        "test-id",
        Mock.Platform.makeWithAgda(),
        channels,
        storageUri,
        mockExtensionUri,
        Memento.make(None),
        mockEditor,
        None,
      )
    }

    let managedPath = (storageUri: VSCode.Uri.t, releaseDir: string, fileName: string): string =>
      NodeJs.Path.join([VSCode.Uri.fsPath(storageUri), "releases", "dev", releaseDir, fileName])

    Async.it(
      "removes managed candidates from connection.paths",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-remove-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let state = createTestState(storageUri)
        let previousPaths = Config.Connection.getAgdaPaths()

        let managed = managedPath(storageUri, "als-dev-Agda-2.8.0-macos-arm64", "als")
        await NodeJs.Fs.mkdir(NodeJs.Path.dirname(managed), {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(managed, NodeJs.Buffer.fromString(""))
        let nonManaged = "/usr/bin/agda"
        await Config.Connection.setAgdaPaths(state.channels.log, [managed, nonManaged])

        let _ = await Connection__Switch.deleteDownloads(state)
        let newPaths = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(newPaths, [nonManaged])
      },
    )

    Async.it(
      "clears metadata only under cleaned directories",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-meta-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let state = createTestState(storageUri)
        let previousPaths = Config.Connection.getAgdaPaths()

        let managed = managedPath(storageUri, "als-dev-Agda-2.8.0-macos-arm64", "als")
        let managedUri = VSCode.Uri.file(managed)
        await NodeJs.Fs.mkdir(NodeJs.Path.dirname(managed), {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(managed, NodeJs.Buffer.fromString(""))
        let resolved: Connection__Candidate.Resolved.t = {
          original: Connection__Candidate.make(managed),
          resource: managedUri,
        }
        await Memento.ResolvedMetadata.setKind(
          state.memento,
          resolved,
          Memento.ResolvedMetadata.ALS(Native, None),
        )
        await Config.Connection.setAgdaPaths(state.channels.log, [managed])

        let _ = await Connection__Switch.deleteDownloads(state)
        let metaAfter = Memento.ResolvedMetadata.get(state.memento, resolved)

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(metaAfter, None)
      },
    )

    Async.it(
      "clears ALS release caches",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-cache-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let state = createTestState(storageUri)
        let previousPaths = Config.Connection.getAgdaPaths()

        await Memento.ALSReleaseCache.setTimestamp(
          state.memento,
          "agda",
          "agda-language-server",
          Date.make(),
        )
        await Memento.ALSReleaseCache.setReleases(
          state.memento,
          "agda",
          "agda-language-server",
          "[]",
        )
        await Config.Connection.setAgdaPaths(state.channels.log, [])

        let _ = await Connection__Switch.deleteDownloads(state)
        let timestampAfter = Memento.ALSReleaseCache.getTimestamp(
          state.memento,
          "agda",
          "agda-language-server",
        )
        let releasesAfter = Memento.ALSReleaseCache.getReleases(
          state.memento,
          "agda",
          "agda-language-server",
        )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(timestampAfter, None)
        Assert.deepStrictEqual(releasesAfter, None)
      },
    )

    Async.it(
      "preserves config when managed-root deletion fails",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-fail-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let state = createTestState(storageUri)
        let previousPaths = Config.Connection.getAgdaPaths()

        let managed = managedPath(storageUri, "als-dev-Agda-2.8.0-macos-arm64", "als")
        await NodeJs.Fs.mkdir(NodeJs.Path.dirname(managed), {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(managed, NodeJs.Buffer.fromString(""))
        await Config.Connection.setAgdaPaths(state.channels.log, [managed])

        let failingRoot = VSCode.Uri.joinPath(storageUri, ["releases"])
        let forcedFailure: Connection__Download__Delete.t = {
          cleanedDirectories: [],
          failedUris: [failingRoot],
          deletedInFlightFiles: [],
          failedInFlightFiles: [],
        }
        let _ =
          await Connection__Switch.deleteDownloads(
            ~runDelete=_ => Promise.resolve(forcedFailure),
            state,
          )
        let pathsAfter = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(pathsAfter, [managed])
      },
    )

    Async.it(
      "does not remove in-flight file paths from config even when deleted",
      async () => {
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-delete-inflight-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let storageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(storageUri)
        let state = createTestState(storageUri)
        let previousPaths = Config.Connection.getAgdaPaths()

        let inFlightPath = NodeJs.Path.join([storagePath, "in-flight.download"])
        NodeJs.Fs.writeFileSync(inFlightPath, NodeJs.Buffer.fromString(""))

        let managed = managedPath(storageUri, "als-dev-Agda-2.8.0-macos-arm64", "als")
        await NodeJs.Fs.mkdir(NodeJs.Path.dirname(managed), {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(managed, NodeJs.Buffer.fromString(""))

        await Config.Connection.setAgdaPaths(state.channels.log, [managed, inFlightPath])

        let _ = await Connection__Switch.deleteDownloads(state)
        let pathsAfter = Config.Connection.getAgdaPaths()

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
        Assert.deepStrictEqual(pathsAfter, [inFlightPath])
      },
    )
  })
})
