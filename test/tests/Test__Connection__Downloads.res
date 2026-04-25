open Mocha
open Test__Util

describe("Connection Downloads", () => {
  This.timeout(10000)

  let makeMockPlatform = (): Platform.t => Mock.Platform.makeWithAgda()

  let createTestStateWithStorage = (storageUri: VSCode.Uri.t) => {
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
      makeMockPlatform(),
      channels,
      storageUri,
      mockExtensionUri,
      Memento.make(None),
      mockEditor,
      None,
    )
  }

  let createStorageUri = async prefix => {
    let storagePath = NodeJs.Path.join([
      NodeJs.Os.tmpdir(),
      prefix ++ "-" ++ string_of_int(int_of_float(Js.Date.now())),
    ])
    await NodeJs.Fs.mkdir(storagePath, {recursive: true, mode: 0o777})
    VSCode.Uri.file(storagePath)
  }

  let createDirectoryWithFile = async (
    rootPath: string,
    relativeSegments: array<string>,
    fileName: string,
  ) => {
    let directoryPath = NodeJs.Path.join([rootPath, ...relativeSegments])
    await NodeJs.Fs.mkdir(directoryPath, {recursive: true, mode: 0o777})
    let filePath = NodeJs.Path.join([directoryPath, fileName])
    NodeJs.Fs.writeFileSync(filePath, NodeJs.Buffer.fromString("content"))
    (directoryPath, filePath)
  }

  let invokeDeleteDownloads = async (state: State.t) => {
    let _ = await Connection__Switch.deleteDownloads(state)
    ()
  }

  let runDeletePlan = async (state: State.t) =>
    await Connection__Download__Delete.run(state.globalStorageUri)

  let withDeleteFailureFor: string => unit => unit = %raw(`function(failedFsPath) {
    const fsModule = require("../../src/FS.bs.js");
    const originalDeleteRecursive = fsModule.deleteRecursive;
    fsModule.deleteRecursive = function(uri) {
      if (uri && uri.fsPath === failedFsPath) {
        return Promise.resolve({ TAG: 1, _0: "mock delete failure" });
      }
      return originalDeleteRecursive(uri);
    };
    return function() {
      fsModule.deleteRecursive = originalDeleteRecursive;
    };
  }`)

  describe("`fromDownloads`", () => {
    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        try {
          let path = await Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-mock-2")
          agdaMockEndpoint := Some(path)
        } catch {
        | Failure(msg) => failwith(msg)
        | _ => failwith("Got error when trying to construct target from mock Agda: unknown error")
        }
      },
    )

    Async.after(
      async () => {
        await Config.Connection.setAgdaPaths(Chan.make(), [])
        await Memento.PreferredCandidate.set(Memento.make(None), None)

        switch agdaMockEndpoint.contents {
        | Some(target) =>
          await Candidate.Agda.destroy(target)
          agdaMockEndpoint := None
        | None => ()
        }
      },
    )

    Async.it(
      "should throw the `PlatformNotSupported` error when the platform is not supported",
      async () => {
        let platform = {
          "os": "non-existent-os",
          "dist": "non-existent-dist",
          "codename": "non-existent-codename",
          "release": "non-existent-release",
        }

        let mockPlatformDeps = Mock.Platform.makeWithPlatformError(platform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let actual = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        let expected = Connection.Error.Establish.fromDownloadError(PlatformNotSupported(platform))

        Assert.deepStrictEqual(actual, Error(expected))
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the initial download policy is `No`",
      async () => {
        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.No,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(
          result,
          Error(Connection.Error.Establish.fromDownloadError(OptedNotToDownload)),
        )

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the user clicked `cancel` on the download dialog",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.Undecided,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(
          result,
          Error(Connection.Error.Establish.fromDownloadError(OptedNotToDownload)),
        )

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)
      },
    )

    Async.it(
      "should check if the latest ALS is already downloaded when the download policy is `Yes`",
      async () => {
        let mockEndpoint = switch agdaMockEndpoint.contents {
        | Some(candidate) => candidate
        | None => failwith("Unable to access the Agda mock candidate")
        }
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithCachedDownloadAndFlag(
          mockEndpoint,
          checkedCache,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)

        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(version, "2.7.0.1")
            Assert.deepStrictEqual(path, mockEndpoint)
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(_) => Assert.fail("Expected successful cached download")
        }

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
      },
    )

    Async.it(
      "should throw the `DownloadALS` error when the download policy is `Yes` but the download fails",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadFailureAndFlags(
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)

        let expected = Connection.Error.Establish.fromDownloadError(CannotFindCompatibleALSRelease)

        Assert.deepStrictEqual(result, Error(expected))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
      },
    )

    Async.it(
      "should resolve Web downloads via DevALS channel",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let resolvedChannel = ref(None)
        let checkedDownload = ref(false)

        module MockWebPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
          let resolveDownloadChannel = (channel, _useCache) => {
            resolvedChannel := Some(channel)
            async (_memento, _globalStorageUri, _platform) =>
              Ok(
                Connection__Download__Source.FromURL(
                  Connection__Download__Channel.DevALS,
                  "https://example.invalid/als.wasm",
                  "dev-als",
                ),
              )
          }
          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => {
            checkedDownload := true
            Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
          }
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

        let mockPlatformDeps: Platform.t = module(MockWebPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        let expected = Connection.Error.Establish.fromDownloadError(CannotFindCompatibleALSRelease)
        Assert.deepStrictEqual(result, Error(expected))
        Assert.deepStrictEqual(resolvedChannel.contents, Some(Connection__Download__Channel.DevALS))
        Assert.deepStrictEqual(checkedDownload.contents, true)
      },
    )

    Async.it(
      "should use DevALS channel on Web regardless of memento",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let resolvedChannel = ref(None)

        module MockWebPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
          let resolveDownloadChannel = (channel, _useCache) => {
            resolvedChannel := Some(channel)
            async (_memento, _globalStorageUri, _platform) =>
              Ok(
                Connection__Download__Source.FromURL(
                  Connection__Download__Channel.DevALS,
                  "https://example.invalid/als.wasm",
                  "dev-als",
                ),
              )
          }
          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

        let mockPlatformDeps: Platform.t = module(MockWebPlatform)
        let memento = Memento.make(None)
        await Memento.SelectedChannel.set(memento, "DevALS")
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let _result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        Assert.deepStrictEqual(resolvedChannel.contents, Some(Connection__Download__Channel.DevALS))
      },
    )

    Async.it(
      "should resolve Desktop downloads via DevALS channel",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let resolvedChannel = ref(None)
        let checkedDownload = ref(false)

        module MockDesktopPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
          let resolveDownloadChannel = (channel, _useCache) => {
            resolvedChannel := Some(channel)
            async (_memento, _globalStorageUri, _platform) =>
              Ok(
                Connection__Download__Source.FromURL(
                  Connection__Download__Channel.DevALS,
                  "https://example.invalid/dev-als-native",
                  "dev-als",
                ),
              )
          }
          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => {
            checkedDownload := true
            Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
          }
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

        let mockPlatformDeps: Platform.t = module(MockDesktopPlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        let expected = Connection.Error.Establish.fromDownloadError(CannotFindCompatibleALSRelease)
        Assert.deepStrictEqual(result, Error(expected))
        Assert.deepStrictEqual(resolvedChannel.contents, Some(Connection__Download__Channel.DevALS))
        Assert.deepStrictEqual(checkedDownload.contents, true)
      },
    )

    Async.it(
      "should retry DevALS download with WASM source when native download fails",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let downloadedMock = switch agdaMockEndpoint.contents {
        | Some(path) => path
        | None => failwith("Unable to access Agda mock candidate")
        }
        let checkedCache = ref(false)
        let checkedNativeDownload = ref(false)
        let checkedWasmDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithNativeFailureAndWASMSuccess(
          downloadedMock,
          checkedCache,
          checkedNativeDownload,
          checkedWasmDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        switch result {
        | Ok(Agda(_, path, _version)) =>
          Assert.deepStrictEqual(path, downloadedMock)
        | Ok(_) => Assert.fail("Expected Agda connection")
        | Error(error) =>
          Assert.fail(
            "Expected fallback download success but got: " ++ Connection.Error.Establish.toString(error),
          )
        }

        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedNativeDownload.contents, true)
        Assert.deepStrictEqual(checkedWasmDownload.contents, true)
      },
    )

    Async.it(
      "should propagate error when channel resolution fails (no WASM fallback)",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let checkedResolve = ref(false)

        module MockDesktopResolveFailurePlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
          let resolveDownloadChannel = (_channel, _useCache) =>
            async (_memento, _globalStorageUri, _platform) => {
              checkedResolve := true
              Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
            }
          let download = (_globalStorageUri, _source, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

        let mockPlatformDeps: Platform.t = module(MockDesktopResolveFailurePlatform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        Assert.deepStrictEqual(checkedResolve.contents, true)
        switch result {
        | Ok(_) => Assert.fail("Expected error when channel resolution fails")
        | Error(errors) =>
          Assert.deepStrictEqual(
            errors.download,
            Connection.Error.Establish.Failed(Connection__Download__Error.CannotFindCompatibleALSRelease),
          )
        }
      },
    )

    describe("Download order", () => {
      Async.it(
        "Desktop download order should be [native, WASM]",
        async () => {
          await Config.Connection.DownloadPolicy.set(Undecided)

          let downloadedMock = switch agdaMockEndpoint.contents {
          | Some(path) => path
          | None => failwith("Unable to access Agda mock candidate")
          }

          let downloadAttempts: ref<array<string>> = ref([])

          let makeOrderAsset = (name): Connection__Download__GitHub.Asset.t => {
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
          let orderRelease: Connection__Download__GitHub.Release.t = {
            url: "", assets_url: "", upload_url: "", html_url: "",
            id: 1, node_id: "dev", tag_name: "dev", target_commitish: "main", name: "dev",
            draft: false, prerelease: true,
            created_at: "2024-01-01T00:00:00Z", published_at: "2024-01-01T00:00:00Z",
            assets: [makeOrderAsset("als-dev-Agda-2.8.0-ubuntu.zip"), makeOrderAsset("als-dev-Agda-2.8.0-wasm.wasm")],
            tarball_url: "", zipball_url: "", body: None,
          }
          let orderNativeDescriptor: Connection__Download__GitHub.DownloadDescriptor.t = {
            asset: makeOrderAsset("als-dev-Agda-2.8.0-ubuntu.zip"),
            release: orderRelease,
            saveAsFileName: "dev-als",
          }

          module MockDesktopOrderPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
            let resolveDownloadChannel = (_channel, _useCache) =>
              async (_memento, _globalStorageUri, _platform) =>
                Ok(
                  Connection__Download__Source.FromGitHub(
                    Connection__Download__Channel.DevALS,
                    orderNativeDescriptor,
                  ),
                )
            let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) =>
              switch source {
              | Connection__Download__Source.FromGitHub(_, descriptor) =>
                if descriptor.asset.name->String.includes("wasm") {
                  downloadAttempts := Array.concat(downloadAttempts.contents, ["wasm"])
                  Promise.resolve(Ok(downloadedMock))
                } else {
                  downloadAttempts := Array.concat(downloadAttempts.contents, ["native"])
                  Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
                }
              | _ =>
                Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
              }
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }

          let mockPlatformDeps: Platform.t = module(MockDesktopOrderPlatform)
          let memento = Memento.make(None)
          let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

          let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

          Assert.deepStrictEqual(downloadAttempts.contents, ["native", "wasm"])
          switch result {
          | Ok(Agda(_, path, _)) => Assert.deepStrictEqual(path, downloadedMock)
          | Ok(_) => Assert.fail("Expected Agda connection")
          | Error(e) =>
            Assert.fail(
              "Expected success after WASM fallback but got: " ++
              Connection.Error.Establish.toString(e),
            )
          }
        },
      )

      Async.it(
        "Web download order should be [WASM] only",
        async () => {
          await Config.Connection.DownloadPolicy.set(Undecided)

          let downloadAttempts: ref<array<string>> = ref([])
          let resolvedChannels: ref<array<Connection__Download__Channel.t>> = ref([])

          let nativeUrl = "https://example.invalid/native-should-not-be-used"

          module MockWebOrderPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
            let resolveDownloadChannel = (channel, _useCache) =>
              async (_memento, _globalStorageUri, _platform) => {
                resolvedChannels :=
                  Array.concat(resolvedChannels.contents, [channel])
                switch channel {
                | Connection__Download__Channel.DevALS =>
                  Ok(
                    Connection__Download__Source.FromURL(
                      Connection__Download__Channel.DevALS,
                      "https://example.invalid/als.wasm",
                      "dev-als",
                    ),
                  )
                | _ =>
                  Ok(
                    Connection__Download__Source.FromURL(
                      channel,
                      nativeUrl,
                      "other-als",
                    ),
                  )
                }
              }
            let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) =>
              switch source {
              | Connection__Download__Source.FromURL(_, url, _)
                if url == "https://example.invalid/als.wasm" =>
                downloadAttempts := Array.concat(downloadAttempts.contents, ["wasm"])
                Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
              | Connection__Download__Source.FromURL(_, _, _) =>
                downloadAttempts := Array.concat(downloadAttempts.contents, ["native"])
                Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
              | _ =>
                Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
              }
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }

          let mockPlatformDeps: Platform.t = module(MockWebOrderPlatform)
          let memento = Memento.make(None)
          let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

          let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

          Assert.deepStrictEqual(downloadAttempts.contents, ["wasm"])
          Assert.deepStrictEqual(
            resolvedChannels.contents,
            [Connection__Download__Channel.DevALS],
          )
          switch result {
          | Ok(_) => Assert.fail("Expected failure when all downloads fail")
          | Error(_) => ()
          }
        },
      )
    })
  })

  describe("make fromDownloads scenarios", () => {
    Async.it(
      "should handle platform not supported error with logging",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let platform = {
          "os": "unsupported-os",
          "dist": "unsupported-dist",
          "codename": "unsupported-codename",
          "release": "unsupported-release",
        }

        let mockPlatformDeps = Mock.Platform.makeWithPlatformError(platform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected platform error")
        | Error(error) =>
          switch error {
          | Connection.Error.Establish(errors) =>
            switch errors.download {
            | Failed(Connection__Download__Error.PlatformNotSupported(_)) => ()
            | _ => Assert.fail("Expected PlatformNotSupported download error")
            }
          | _ => Assert.fail("Expected Establish error")
          }
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle download policy No with logging",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.No,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected error due to No download policy")
        | Error(error) =>
          switch error {
          | Connection.Error.Establish(_) => ()
          | _ => Assert.fail("Expected Establish error")
          }

          Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle download policy Undecided (user cancelled) with logging",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.Undecided,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )
        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected error due to user cancelling download")
        | Error(error) =>
          switch error {
          | Connection.Error.Establish(_) => ()
          | _ => Assert.fail("Expected Establish error")
          }

          Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle cached ALS download with logging",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let agdaMockPath = await Test__Util.Candidate.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-cached",
        )
        let mockEndpoint = agdaMockPath

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithCachedDownloadAndFlag(
          mockEndpoint,
          checkedCache,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(checkedCache.contents, true)

          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, version))] =>
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }

          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
        | Error(_) => Assert.fail("Expected successful cached download")
        }

        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle fresh ALS download with logging",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let agdaMockPath = await Test__Util.Candidate.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-fresh-download",
        )
        let mockEndpoint = agdaMockPath

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithSuccessfulDownloadAndFlags(
          mockEndpoint,
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(checkedCache.contents, true)
          Assert.deepStrictEqual(checkedDownload.contents, true)

          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, version))] =>
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
            Assert.deepStrictEqual(path, agdaMockPath)
          | _ => Assert.fail("Expected Agda connection")
          }

          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
        | Error(_) => Assert.fail("Expected successful fresh download")
        }

        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle download failure with logging",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadFailureAndFlags(
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected download failure")
        | Error(error) =>
          Assert.deepStrictEqual(checkedCache.contents, true)

          switch error {
          | Connection.Error.Establish(errors) =>
            switch errors.download {
            | Failed(Connection__Download__Error.CannotFindCompatibleALSRelease) => ()
            | _ => Assert.fail("Expected CannotFindCompatibleALSRelease download error")
            }
          | _ => Assert.fail("Expected Establish error")
          }

          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should fall back to WASM when DevALS native download fails",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let agdaMockPath = await Test__Util.Candidate.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-devals-wasm-fallback",
        )

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedNativeDownload = ref(false)
        let checkedWasmDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithNativeFailureAndWASMSuccess(
          agdaMockPath,
          checkedCache,
          checkedNativeDownload,
          checkedWasmDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(checkedCache.contents, true)
          Assert.deepStrictEqual(checkedNativeDownload.contents, true)
          Assert.deepStrictEqual(checkedWasmDownload.contents, true)

          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(path, version))] =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(error) =>
          let (_title, detail) = Connection.Error.toString(error)
          Assert.fail(
            "Expected WASM fallback success but got: " ++ detail,
          )
        }

        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )
  })

  describe("delete downloads", () => {
    Async.it(
      "should return delete storage report with cleaned managed roots and deleted in-flight files",
      async () => {
        let previousPaths = Config.Connection.getAgdaPaths()
        let storageUri = await createStorageUri("agda-delete-plan")
        let storagePath = storageUri->VSCode.Uri.fsPath
        let state = createTestStateWithStorage(storageUri)
        let releasesUri = VSCode.Uri.joinPath(storageUri, ["releases"])
        let inFlightUri = VSCode.Uri.joinPath(storageUri, ["in-flight.download"])
        let inFlightZipUri = VSCode.Uri.joinPath(storageUri, ["in-flight.download.zip"])
        let devNativePath = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath
        let devWasmUri = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm",
        ])->VSCode.Uri.toString

        let _ = await createDirectoryWithFile(
          storagePath,
          ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64"],
          "als",
        )
        let _ = await createDirectoryWithFile(
          storagePath,
          ["releases", "dev", "als-dev-Agda-2.8.0-wasm"],
          "als.wasm",
        )
        NodeJs.Fs.writeFileSync(inFlightUri->VSCode.Uri.fsPath, NodeJs.Buffer.fromString("partial"))
        NodeJs.Fs.writeFileSync(inFlightZipUri->VSCode.Uri.fsPath, NodeJs.Buffer.fromString("partial"))
        await Config.Connection.setAgdaPaths(
          state.channels.log,
          ["/usr/local/bin/agda", devNativePath, devWasmUri],
        )

        let result = await runDeletePlan(state)

        Assert.deepStrictEqual(
          result.cleanedDirectories->Array.map(uri => uri->VSCode.Uri.toString),
          [releasesUri->VSCode.Uri.toString],
        )
        Assert.deepStrictEqual(result.failedUris->Array.map(uri => uri->VSCode.Uri.toString), [])
        Assert.deepStrictEqual(
          result.deletedInFlightFiles->Array.map(uri => uri->VSCode.Uri.toString),
          [inFlightUri->VSCode.Uri.toString, inFlightZipUri->VSCode.Uri.toString],
        )
        Assert.deepStrictEqual(
          result.failedInFlightFiles->Array.map(uri => uri->VSCode.Uri.toString),
          [],
        )
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [
          "/usr/local/bin/agda",
          devNativePath,
          devWasmUri,
        ])

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
      },
    )

    Async.it(
      "should report failed managed roots separately from in-flight file cleanup",
      async () => {
        let previousPaths = Config.Connection.getAgdaPaths()
        let storageUri = await createStorageUri("agda-delete-plan-failed")
        let storagePath = storageUri->VSCode.Uri.fsPath
        let state = createTestStateWithStorage(storageUri)
        let releasesUri = VSCode.Uri.joinPath(storageUri, ["releases"])
        let nativePath = VSCode.Uri.joinPath(storageUri, [
          "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
        ])->VSCode.Uri.fsPath
        let restoreDelete = withDeleteFailureFor(releasesUri->VSCode.Uri.fsPath)

        let _ = await createDirectoryWithFile(
          storagePath,
          ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64"],
          "als",
        )
        await Config.Connection.setAgdaPaths(state.channels.log, ["/usr/local/bin/agda", nativePath])

        let result = await runDeletePlan(state)
        restoreDelete()

        Assert.deepStrictEqual(result.cleanedDirectories->Array.map(uri => uri->VSCode.Uri.toString), [])
        Assert.deepStrictEqual(
          result.failedUris->Array.map(uri => uri->VSCode.Uri.toString),
          [releasesUri->VSCode.Uri.toString],
        )
        Assert.deepStrictEqual(result.deletedInFlightFiles->Array.map(uri => uri->VSCode.Uri.toString), [])
        Assert.deepStrictEqual(
          result.failedInFlightFiles->Array.map(uri => uri->VSCode.Uri.toString),
          [],
        )

        await Config.Connection.setAgdaPaths(state.channels.log, previousPaths)
        let _ = await FS.deleteRecursive(storageUri)
      },
    )

    describe("file system", () => {
      let managedDirectorySpecs = [
        (["releases", "dev", "als-dev-Agda-2.8.0-wasm"], "als.wasm"),
        (["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64"], "als"),
        (["releases", "v6", "als-v6-Agda-2.8.0-macos-arm64"], "als"),
      ]

      Async.it(
        "should remove all managed download directories from globalStorageUri",
        async () => {
          let storageUri = await createStorageUri("agda-delete-downloads-fs-all")
          let storagePath = storageUri->VSCode.Uri.fsPath

          let managedArtifacts =
            managedDirectorySpecs->Array.map(((relSegments, fileName)) =>
              createDirectoryWithFile(storagePath, relSegments, fileName)
            )
          let managedArtifacts = await Promise.all(managedArtifacts)

          let state = createTestStateWithStorage(storageUri)
          await invokeDeleteDownloads(state)

          managedArtifacts->Array.forEach(((directoryPath, filePath)) => {
            Assert.deepStrictEqual(NodeJs.Fs.existsSync(directoryPath), false)
            Assert.deepStrictEqual(NodeJs.Fs.existsSync(filePath), false)
          })

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

      Async.it(
        "should preserve unrelated sibling files and directories under globalStorageUri",
        async () => {
          let storageUri = await createStorageUri("agda-delete-downloads-fs-preserve")
          let storagePath = storageUri->VSCode.Uri.fsPath

          let _ = await createDirectoryWithFile(storagePath, ["releases", "dev", "als-dev-Agda-2.8.0-macos-arm64"], "als")
          let (keptDirectoryPath, keptNestedFilePath) = await createDirectoryWithFile(
            storagePath,
            ["keep-me"],
            "notes.txt",
          )
          let keptRootFilePath = NodeJs.Path.join([storagePath, "keep-root.txt"])
          NodeJs.Fs.writeFileSync(keptRootFilePath, NodeJs.Buffer.fromString("keep"))

          let state = createTestStateWithStorage(storageUri)
          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(NodeJs.Fs.existsSync(keptDirectoryPath), true)
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(keptNestedFilePath), true)
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(keptRootFilePath), true)

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

      Async.it(
        "should preserve release-managed artifacts when releases directory fails to delete",
        async () => {
          let storageUri = await createStorageUri("agda-delete-downloads-fs-partial")
          let storagePath = storageUri->VSCode.Uri.fsPath

          let managedArtifacts =
            managedDirectorySpecs->Array.map(((relSegments, fileName)) =>
              createDirectoryWithFile(storagePath, relSegments, fileName)
            )
          let managedArtifacts = await Promise.all(managedArtifacts)
          let failedDirectoryPath = NodeJs.Path.join([storagePath, "releases"])
          let restoreDelete = withDeleteFailureFor(failedDirectoryPath)

          let keptSiblingFilePath = NodeJs.Path.join([storagePath, "keep-root.txt"])
          NodeJs.Fs.writeFileSync(keptSiblingFilePath, NodeJs.Buffer.fromString("keep"))

          let state = createTestStateWithStorage(storageUri)
          await invokeDeleteDownloads(state)
          restoreDelete()

          managedArtifacts->Array.forEach(((directoryPath, filePath)) => {
            Assert.deepStrictEqual(NodeJs.Fs.existsSync(directoryPath), true)
            Assert.deepStrictEqual(NodeJs.Fs.existsSync(filePath), true)
          })
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(keptSiblingFilePath), true)

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

    })

    describe("connection.paths", () => {
      Async.it(
        "should remove release-managed native candidate from connection.paths",
        async () => {
          let storageUri = await createStorageUri("agda-delete-release-native")
          let state = createTestStateWithStorage(storageUri)
          let keepPath = "/usr/local/bin/agda"
          let nativePath = VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
          ])->VSCode.Uri.fsPath

          await Config.Connection.setAgdaPaths(state.channels.log, [keepPath, nativePath])
          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath])

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

      Async.it(
        "should remove release-managed WASM candidate from connection.paths",
        async () => {
          let storageUri = await createStorageUri("agda-delete-release-wasm")
          let state = createTestStateWithStorage(storageUri)
          let keepPath = "/usr/local/bin/agda"
          let wasmUri = VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-wasm", "als.wasm",
          ])->VSCode.Uri.toString

          await Config.Connection.setAgdaPaths(state.channels.log, [keepPath, wasmUri])
          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [keepPath])

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

      Async.it(
        "should remove all release-managed candidates and preserve user-managed candidates",
        async () => {
          let storageUri = await createStorageUri("agda-delete-release-all")
          let state = createTestStateWithStorage(storageUri)
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
          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(
            Config.Connection.getAgdaPaths(),
            [keepPath, keepBareCommand, keepUri],
          )

          let _ = await FS.deleteRecursive(storageUri)
        },
      )
    })

    describe("resolved metadata", () => {
      Async.it(
        "should preserve non-download ResolvedMetadata and remove release-managed ResolvedMetadata",
        async () => {
          let storageUri = await createStorageUri("agda-delete-metadata-release")
          let state = createTestStateWithStorage(storageUri)
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

          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(
            Memento.ResolvedMetadata.get(state.memento, keepResolved)->Option.map(entry => entry.kind),
            Some(Memento.ResolvedMetadata.Agda(Some("2.7.0.1"))),
          )
          Assert.deepStrictEqual(Memento.ResolvedMetadata.get(state.memento, releasedResolved), None)

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

      Async.it(
        "should remove ResolvedMetadata for all release-managed artifacts and preserve unrelated resources",
        async () => {
          let storageUri = await createStorageUri("agda-delete-metadata-all-release")
          let state = createTestStateWithStorage(storageUri)
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

          await invokeDeleteDownloads(state)

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
        },
      )

      Async.it(
        "should preserve ResolvedMetadata under releases/ when that directory fails to delete",
        async () => {
          let storageUri = await createStorageUri("agda-delete-metadata-partial-release")
          let releasesUri = VSCode.Uri.joinPath(storageUri, ["releases"])
          let _ = await FS.createDirectory(releasesUri)
          let state = createTestStateWithStorage(storageUri)
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

          let restoreDeleteRecursive = withDeleteFailureFor(releasesUri->VSCode.Uri.fsPath)

          await invokeDeleteDownloads(state)
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
        },
      )
    })

    describe("release cache", () => {
      Async.it(
        "should clear managed ALSReleaseCache repos and preserve unrelated repo cache",
        async () => {
          let storageUri = await createStorageUri("agda-switch-version-delete-release-cache")
          let state = createTestStateWithStorage(storageUri)

          let now = Date.make()
          await Memento.ALSReleaseCache.setTimestamp(state.memento, "agda", "agda-language-server", now)
          await Memento.ALSReleaseCache.setReleases(state.memento, "agda", "agda-language-server", "agda-cache")
          await Memento.ALSReleaseCache.setTimestamp(state.memento, "banacorn", "agda-language-server", now)
          await Memento.ALSReleaseCache.setReleases(state.memento, "banacorn", "agda-language-server", "banacorn-cache")
          await Memento.ALSReleaseCache.setTimestamp(state.memento, "other", "repo", now)
          await Memento.ALSReleaseCache.setReleases(state.memento, "other", "repo", "other-cache")

          await invokeDeleteDownloads(state)

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
        },
      )
    })

    describe("memento", () => {
      Async.it(
        "should leave PreferredCandidate unchanged when it points to a release-managed path",
        async () => {
          let storageUri = await createStorageUri("agda-delete-memento-release-preferred")
          let state = createTestStateWithStorage(storageUri)
          let releaseManagedPath = VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
          ])->VSCode.Uri.fsPath

          await Memento.PreferredCandidate.set(state.memento, Some(releaseManagedPath))
          await Config.Connection.setAgdaPaths(
            state.channels.log,
            ["/usr/bin/agda", releaseManagedPath],
          )

          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(releaseManagedPath))
          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["/usr/bin/agda"])

          let _ = await FS.deleteRecursive(storageUri)
        },
      )

      Async.it(
        "should leave PreferredCandidate unchanged when it points to a user-managed path",
        async () => {
          let storageUri = await createStorageUri("agda-delete-memento-user-preferred")
          let state = createTestStateWithStorage(storageUri)
          let userPath = "/usr/local/bin/agda"
          let releaseManagedPath = VSCode.Uri.joinPath(storageUri, [
            "releases", "dev", "als-dev-Agda-2.8.0-macos-arm64", "als",
          ])->VSCode.Uri.fsPath

          await Memento.PreferredCandidate.set(state.memento, Some(userPath))
          await Config.Connection.setAgdaPaths(
            state.channels.log,
            [userPath, releaseManagedPath],
          )

          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(userPath))
          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [userPath])

          let _ = await FS.deleteRecursive(storageUri)
        },
      )
    })

    describe("in-flight files", () => {
      Async.it(
        "should delete orphaned in-flight.download and in-flight.download.zip files",
        async () => {
          let storageUri = await createStorageUri("agda-switch-delete-inflight")
          let state = createTestStateWithStorage(storageUri)
          let inFlightUri = VSCode.Uri.joinPath(storageUri, ["in-flight.download"])
          let inFlightZipUri = VSCode.Uri.joinPath(storageUri, ["in-flight.download.zip"])
          NodeJs.Fs.writeFileSync(inFlightUri->VSCode.Uri.fsPath, NodeJs.Buffer.fromString("partial download"))
          NodeJs.Fs.writeFileSync(inFlightZipUri->VSCode.Uri.fsPath, NodeJs.Buffer.fromString("partial zip"))

          await invokeDeleteDownloads(state)

          Assert.deepStrictEqual((await FS.stat(inFlightUri))->Result.isError, true)
          Assert.deepStrictEqual((await FS.stat(inFlightZipUri))->Result.isError, true)

          let _ = await FS.deleteRecursive(storageUri)
        },
      )
    })
  })
})
