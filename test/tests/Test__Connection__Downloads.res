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

  let makeDeleteDownloadsItem = (state: State.t) =>
    State__SwitchVersion.Item.fromItemData(
      State__SwitchVersion.ItemData.DeleteDownloads,
      state.extensionUri,
    )

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
    let view = State__SwitchVersion.View.make(state.channels.log)
    let manager = State__SwitchVersion.SwitchVersionManager.make(state)
    let selectedItem = makeDeleteDownloadsItem(state)

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
      ref([Connection__Download.Channel.Hardcoded]),
      ref(Connection__Download.Channel.Hardcoded),
      _downloadInfo => Promise.resolve(),
      view,
      [selectedItem],
    )

    await onSelectionCompleted
    view->State__SwitchVersion.View.destroy
  }

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
      "should resolve Web downloads via Hardcoded channel",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let resolvedChannel = ref(None)
        let checkedDownload = ref(false)

        module MockWebPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)
          let resolveDownloadChannel = (channel, _useCache) => {
            resolvedChannel := Some(channel)
            async (_memento, _globalStorageUri, _platform) =>
              Ok(
                Connection__Download.Source.FromURL(
                  Hardcoded,
                  Connection__Hardcoded.wasmUrl,
                  "hardcoded-als",
                ),
              )
          }
          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => {
            checkedDownload := true
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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
        Assert.deepStrictEqual(resolvedChannel.contents, Some(Connection__Download.Channel.Hardcoded))
        Assert.deepStrictEqual(checkedDownload.contents, true)
      },
    )

    Async.it(
      "should clamp stale DevALS memento to Hardcoded on Web",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let resolvedChannel = ref(None)

        module MockWebPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)
          let resolveDownloadChannel = (channel, _useCache) => {
            resolvedChannel := Some(channel)
            async (_memento, _globalStorageUri, _platform) =>
              Ok(
                Connection__Download.Source.FromURL(
                  Hardcoded,
                  Connection__Hardcoded.wasmUrl,
                  "hardcoded-als",
                ),
              )
          }
          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

        let mockPlatformDeps: Platform.t = module(MockWebPlatform)
        let memento = Memento.make(None)
        await Memento.SelectedChannel.set(memento, "DevALS")
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let _result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        Assert.deepStrictEqual(resolvedChannel.contents, Some(Connection__Download.Channel.Hardcoded))
      },
    )

    Async.it(
      "should resolve Desktop downloads via Hardcoded channel",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let resolvedChannel = ref(None)
        let checkedDownload = ref(false)

        module MockDesktopPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)
          let resolveDownloadChannel = (channel, _useCache) => {
            resolvedChannel := Some(channel)
            async (_memento, _globalStorageUri, _platform) =>
              Ok(
                Connection__Download.Source.FromURL(
                  Hardcoded,
                  "https://example.invalid/hardcoded-native",
                  "hardcoded-als",
                ),
              )
          }
          let download = (_globalStorageUri, _downloadDescriptor, ~trace as _=Connection__Download__Trace.noop) => {
            checkedDownload := true
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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
        Assert.deepStrictEqual(resolvedChannel.contents, Some(Connection__Download.Channel.Hardcoded))
        Assert.deepStrictEqual(checkedDownload.contents, true)
      },
    )

    Async.it(
      "should retry Hardcoded download with WASM source when native download fails",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let downloadedMock = switch agdaMockEndpoint.contents {
        | Some(path) => path
        | None => failwith("Unable to access Agda mock candidate")
        }
        let checkedCache = ref(false)
        let checkedNativeDownload = ref(false)
        let checkedWasmDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithHardcodedNativeFailureAndWASMSuccess(
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
      "should retry Hardcoded download with WASM source when Hardcoded channel resolution fails",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)

        let downloadedMock = switch agdaMockEndpoint.contents {
        | Some(path) => path
        | None => failwith("Unable to access Agda mock candidate")
        }
        let checkedResolve = ref(false)
        let checkedWasmDownload = ref(false)

        module MockDesktopResolveFailurePlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
          let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)
          let resolveDownloadChannel = (_channel, _useCache) =>
            async (_memento, _globalStorageUri, _platform) => {
              checkedResolve := true
              Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
            }
          let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) =>
            switch source {
            | Connection__Download.Source.FromURL(Hardcoded, url, _)
              if url == Connection__Hardcoded.wasmUrl =>
              checkedWasmDownload := true
              Promise.resolve(Ok(downloadedMock))
            | _ => Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
            }
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }

        let mockPlatformDeps: Platform.t = module(MockDesktopResolveFailurePlatform)
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

        Assert.deepStrictEqual(checkedResolve.contents, true)
        Assert.deepStrictEqual(checkedWasmDownload.contents, true)
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

          let nativeUrl = switch Connection__Hardcoded.nativeUrlForPlatform(Ubuntu) {
          | Some(url) => url
          | None => failwith("Expected hardcoded native URL for Ubuntu")
          }

          module MockDesktopOrderPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.Ubuntu)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)
            let resolveDownloadChannel = (_channel, _useCache) =>
              async (_memento, _globalStorageUri, _platform) =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.Hardcoded,
                    nativeUrl,
                    "hardcoded-als",
                  ),
                )
            let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) =>
              switch source {
              | Connection__Download.Source.FromURL(_, url, _) if url == nativeUrl =>
                downloadAttempts := Array.concat(downloadAttempts.contents, ["native"])
                Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
              | Connection__Download.Source.FromURL(_, url, _)
                if url == Connection__Hardcoded.wasmUrl =>
                downloadAttempts := Array.concat(downloadAttempts.contents, ["wasm"])
                Promise.resolve(Ok(downloadedMock))
              | _ =>
                Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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
          let resolvedChannels: ref<array<Connection__Download.Channel.t>> = ref([])

          let nativeUrl = "https://example.invalid/native-should-not-be-used"

          module MockWebOrderPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _channel) => Promise.resolve(None)
            let resolveDownloadChannel = (channel, _useCache) =>
              async (_memento, _globalStorageUri, _platform) => {
                resolvedChannels :=
                  Array.concat(resolvedChannels.contents, [channel])
                switch channel {
                | Connection__Download.Channel.Hardcoded =>
                  Ok(
                    Connection__Download.Source.FromURL(
                      Connection__Download.Channel.Hardcoded,
                      Connection__Hardcoded.wasmUrl,
                      "hardcoded-als",
                    ),
                  )
                | _ =>
                  Ok(
                    Connection__Download.Source.FromURL(
                      channel,
                      nativeUrl,
                      "other-als",
                    ),
                  )
                }
              }
            let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) =>
              switch source {
              | Connection__Download.Source.FromURL(_, url, _)
                if url == Connection__Hardcoded.wasmUrl =>
                downloadAttempts := Array.concat(downloadAttempts.contents, ["wasm"])
                Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
              | Connection__Download.Source.FromURL(_, _, _) =>
                downloadAttempts := Array.concat(downloadAttempts.contents, ["native"])
                Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
              | _ =>
                Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
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
            [Connection__Download.Channel.Hardcoded],
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
            | Failed(Connection__Download.Error.PlatformNotSupported(_)) => ()
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
            | Failed(Connection__Download.Error.CannotFindCompatibleALSRelease) => ()
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
      "should fall back to WASM when Hardcoded native download fails",
      async () => {
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let agdaMockPath = await Test__Util.Candidate.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-hardcoded-wasm-fallback",
        )

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedNativeDownload = ref(false)
        let checkedWasmDownload = ref(false)

        let mockPlatformDeps = Mock.Platform.makeWithHardcodedNativeFailureAndWASMSuccess(
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
  })
})
