open Mocha
open Test__Util

describe("Connection Downloads", () => {
  This.timeout(10000)

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
        await Memento.PickedConnection.set(Memento.make(None), None)

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
          let download = (_globalStorageUri, _downloadDescriptor) => {
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
          let download = (_globalStorageUri, _downloadDescriptor) =>
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
          let download = (_globalStorageUri, _downloadDescriptor) => {
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
          let download = (_globalStorageUri, source) =>
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
            let download = (_globalStorageUri, source) =>
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
            let download = (_globalStorageUri, source) =>
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
})
