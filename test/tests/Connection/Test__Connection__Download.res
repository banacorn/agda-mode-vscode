open Mocha

module GitHub = Connection__Download__GitHub
module Unzip = Connection__Download__Unzip

// Generic stream event emitters for fake-stream tests
@send external emitEvent: ('stream, string) => bool = "emit"
@send external emitEventWithArg: ('stream, string, Js.Exn.t) => bool = "emit"
@module("node:fs/promises") external mkdtemp: string => Js.Promise.t<string> = "mkdtemp"

let withTempStorage = async (prefix, f) => {
  let tempDir = await mkdtemp(NodeJs.Path.join([NodeJs.Os.tmpdir(), prefix]))
  let globalStorageUri = VSCode.Uri.file(tempDir)
  let cleanup = async () => {
    let _ = await FS.deleteRecursive(globalStorageUri)
  }
  try {
    await f(tempDir, globalStorageUri)
    await cleanup()
  } catch {
  | exn =>
    await cleanup()
    raise(exn)
  }
}

let normalizeLocalPathForAssert = (path: string): string =>
  if OS.onUnix {
    path
  } else {
    let rec replaceBackslashes = (index, acc) =>
      if index >= String.length(path) {
        acc
      } else {
        let ch = String.charAt(path, index)
        replaceBackslashes(index + 1, acc ++ (if ch == "\\" { "/" } else { ch }))
      }
    let normalized = replaceBackslashes(0, "")->String.toLowerCase
    if String.length(normalized) >= 4 &&
        String.charAt(normalized, 0) == "/" &&
        String.charAt(normalized, 2) == ":" &&
        String.charAt(normalized, 3) == "/" {
      String.sliceToEnd(normalized, ~start=1)
    } else {
      normalized
    }
  }

let assertLocalPathEqual = (actual: string, expected: string) =>
  Assert.deepStrictEqual(
    normalizeLocalPathForAssert(actual),
    normalizeLocalPathForAssert(expected),
  )

describe("Download", () => {
  This.timeout(10000)

  describe("DownloadArtifact", () => {
    let expectParsed = (raw, releaseTag, agdaVersion, platform) =>
      switch Connection__Download__DownloadArtifact.parseName(raw) {
      | Some(parsed) =>
        Assert.deepStrictEqual(parsed.releaseTag, releaseTag)
        Assert.deepStrictEqual(parsed.agdaVersion, agdaVersion)
        Assert.deepStrictEqual(parsed.platform, platform)
      | None => Assert.fail("expected DownloadArtifact to parse: " ++ raw)
      }

    let expectRejected = raw =>
      Assert.deepStrictEqual(Connection__Download__DownloadArtifact.parseName(raw), None)

    it("should parse canonical WASM artifact filenames with typed platform", () => {
      expectParsed(
        "als-dev-Agda-2.8.0-wasm.wasm",
        "dev",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.Wasm,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-wasm.wasm",
        "v6",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.Wasm,
      )
    })

    it("should parse canonical native artifact filenames with typed platform", () => {
      expectParsed(
        "als-v6-Agda-2.8.0-ubuntu.zip",
        "v6",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.Ubuntu,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-macos-arm64.zip",
        "v6",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.MacOSArm64,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-macos-x64.zip",
        "v6",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.MacOSX64,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-windows.zip",
        "v6",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.Windows,
      )
    })

    it("should parse canonical artifact directory names without extension", () => {
      expectParsed(
        "als-dev-Agda-2.8.0-wasm",
        "dev",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.Wasm,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-macos-arm64",
        "v6",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.MacOSArm64,
      )
    })

    it("should allow dotted release tags", () => {
      expectParsed(
        "als-v6.1-Agda-2.8.0-wasm.wasm",
        "v6.1",
        "2.8.0",
        Connection__Download__DownloadArtifact.Platform.Wasm,
      )
    })

    it("should reject non-canonical names", () => {
      expectRejected("als-v6-agda-2.8.0-wasm.wasm")
      expectRejected("als-v6-Agda-2.8.0-freebsd.zip")
      expectRejected("als-v6-Agda-2.8.0-wasm.zip")
      expectRejected("als-v6-Agda-2.8.0-ubuntu.wasm")
      expectRejected("/tmp/releases/v6/als-v6-Agda-2.8.0-wasm/als.wasm")
      expectRejected("als-v6-beta-Agda-2.8.0-wasm.wasm")
      expectRejected("als-v6/dev-Agda-2.8.0-wasm.wasm")
    })

    it("should derive the artifact directory name from the artifact identity", () => {
      let artifact = switch Connection__Download__DownloadArtifact.parseName(
        "als-v6.1-Agda-2.8.0-macos-arm64.zip",
      ) {
      | Some(artifact) => artifact
      | None => raise(Failure("expected artifact to parse"))
      }

      Assert.deepStrictEqual(
        Connection__Download__DownloadArtifact.cacheName(artifact),
        "als-v6.1-Agda-2.8.0-macos-arm64",
      )
    })

    it("should construct release-based managed executable URIs", () => {
      let globalStorageUri = VSCode.Uri.file("/tmp/agda-mode-global-storage")
      let wasmArtifact = switch Connection__Download__DownloadArtifact.parseName(
        "als-v6-Agda-2.8.0-wasm.wasm",
      ) {
      | Some(artifact) => artifact
      | None => raise(Failure("expected artifact to parse"))
      }
      let nativeArtifact = switch Connection__Download__DownloadArtifact.parseName(
        "als-v6-Agda-2.8.0-macos-arm64.zip",
      ) {
      | Some(artifact) => artifact
      | None => raise(Failure("expected artifact to parse"))
      }

      Assert.deepStrictEqual(
        VSCode.Uri.fsPath(
          Connection__Download__DownloadArtifact.managedExecutableUri(
            globalStorageUri,
            wasmArtifact,
          ),
        ),
        NodeJs.Path.join([
          "/tmp/agda-mode-global-storage",
          "releases",
          "v6",
          "als-v6-Agda-2.8.0-wasm",
          "als.wasm",
        ]),
      )
      Assert.deepStrictEqual(
        VSCode.Uri.fsPath(
          Connection__Download__DownloadArtifact.managedExecutableUri(
            globalStorageUri,
            nativeArtifact,
          ),
        ),
        NodeJs.Path.join([
          "/tmp/agda-mode-global-storage",
          "releases",
          "v6",
          "als-v6-Agda-2.8.0-macos-arm64",
          "als",
        ]),
      )
    })
  })

  describe("Source.toVersionString with release artifacts", () => {
    it("should use the concrete release tag for LatestALS sources", () => {
      let asset: GitHub.Asset.t = {
        url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-wasm.wasm",
        id: 0,
        node_id: "",
        name: "als-v6-Agda-2.8.0-wasm.wasm",
        label: None,
        content_type: "application/wasm",
        state: "uploaded",
        size: 0,
        created_at: "",
        updated_at: "",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/als-v6-Agda-2.8.0-wasm.wasm",
      }
      let release: GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "https://github.com/agda/agda-language-server/releases/tag/v6",
        id: 0,
        node_id: "",
        tag_name: "v6",
        target_commitish: "",
        name: "v6",
        draft: false,
        prerelease: false,
        created_at: "",
        published_at: "",
        assets: [asset],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }
      let descriptor: GitHub.DownloadDescriptor.t = {
        release,
        asset,
        saveAsFileName: "latest-als",
      }

      Assert.deepStrictEqual(
        Connection__Download__Source.toVersionString(
          Connection__Download__Source.FromGitHub(LatestALS, descriptor),
        ),
        "Agda v2.8.0 Language Server v6",
      )
    })
  })

  describe("expectedPathForSource with release artifacts", () => {
    let makeSource = (
      ~channel: Connection__Download__Channel.t,
      ~releaseTag,
      ~assetName,
    ) => {
      let asset: GitHub.Asset.t = {
        url: "https://github.com/agda/agda-language-server/releases/download/" ++
        releaseTag ++ "/" ++ assetName,
        id: 0,
        node_id: "",
        name: assetName,
        label: None,
        content_type: "",
        state: "uploaded",
        size: 0,
        created_at: "",
        updated_at: "",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/" ++
        releaseTag ++ "/" ++ assetName,
      }
      let release: GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "https://github.com/agda/agda-language-server/releases/tag/" ++ releaseTag,
        id: 0,
        node_id: "",
        tag_name: releaseTag,
        target_commitish: "",
        name: releaseTag,
        draft: false,
        prerelease: false,
        created_at: "",
        published_at: "",
        assets: [asset],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }
      let descriptor: GitHub.DownloadDescriptor.t = {
        release,
        asset,
        saveAsFileName: switch channel {
        | DevALS => "dev-als"
        | LatestALS => "latest-als"
        },
      }

      Connection__Download__Source.FromGitHub(channel, descriptor)
    }

    it("should use release-based managed storage for DevALS native artifacts", () => {
      let globalStorageUri = VSCode.Uri.file("/tmp/agda-mode-global-storage")
      let source = makeSource(
        ~channel=DevALS,
        ~releaseTag="dev",
        ~assetName="als-dev-Agda-2.8.0-macos-arm64.zip",
      )

      Assert.deepStrictEqual(
        Connection__Download.expectedPathForSource(globalStorageUri, source),
        NodeJs.Path.join([
          "/tmp/agda-mode-global-storage",
          "releases",
          "dev",
          "als-dev-Agda-2.8.0-macos-arm64",
          "als",
        ]),
      )
    })

    it("should use release-based managed storage for DevALS WASM artifacts", () => {
      let globalStorageUri = VSCode.Uri.file("/tmp/agda-mode-global-storage")
      let source = makeSource(
        ~channel=DevALS,
        ~releaseTag="dev",
        ~assetName="als-dev-Agda-2.8.0-wasm.wasm",
      )

      Assert.deepStrictEqual(
        Connection__Download.expectedPathForSource(globalStorageUri, source),
        NodeJs.Path.join([
          "/tmp/agda-mode-global-storage",
          "releases",
          "dev",
          "als-dev-Agda-2.8.0-wasm",
          "als.wasm",
        ]),
      )
    })
  })

  describe("Availability — DevALS", () => {
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

    let makeDevRelease = (): Connection__Download__GitHub.Release.t => {
      url: "https://api.github.com/repos/agda/agda-language-server/releases/dev",
      assets_url: "https://api.github.com/repos/agda/agda-language-server/releases/dev/assets",
      upload_url: "https://uploads.github.com/repos/agda/agda-language-server/releases/dev/assets",
      html_url: "https://github.com/agda/agda-language-server/releases/tag/dev",
      id: 1,
      node_id: "dev",
      tag_name: "dev",
      target_commitish: "main",
      name: "dev",
      draft: false,
      prerelease: true,
      created_at: "2024-01-01T00:00:00Z",
      published_at: "2024-01-01T00:00:00Z",
      assets: [
        makeDevAsset("als-dev-Agda-2.8.0-macos-arm64.zip"),
        makeDevAsset("als-dev-Agda-2.8.0-macos-x64.zip"),
        makeDevAsset("als-dev-Agda-2.8.0-ubuntu.zip"),
        makeDevAsset("als-dev-Agda-2.8.0-windows.zip"),
        makeDevAsset("als-dev-Agda-2.8.0-wasm.wasm"),
        makeDevAsset("als-dev-Agda-2.7.0.1-macos-arm64.zip"),
        makeDevAsset("als-dev-Agda-2.7.0.1-macos-x64.zip"),
        makeDevAsset("als-dev-Agda-2.7.0.1-ubuntu.zip"),
        makeDevAsset("als-dev-Agda-2.7.0.1-windows.zip"),
        makeDevAsset("als-dev-Agda-2.7.0.1-wasm.wasm"),
        makeDevAsset("als-dev-Agda-2.6.4.3-macos-arm64.zip"),
        makeDevAsset("als-dev-Agda-2.6.4.3-macos-x64.zip"),
        makeDevAsset("als-dev-Agda-2.6.4.3-ubuntu.zip"),
        makeDevAsset("als-dev-Agda-2.6.4.3-windows.zip"),
        makeDevAsset("als-dev-Agda-2.6.4.3-wasm.wasm"),
      ],
      tarball_url: "https://api.github.com/repos/agda/agda-language-server/tarball/dev",
      zipball_url: "https://api.github.com/repos/agda/agda-language-server/zipball/dev",
      body: Some("Dev build"),
    }

    let makeDevDescriptor = (): Connection__Download__GitHub.DownloadDescriptor.t => {
      let release = makeDevRelease()
      {
        Connection__Download__GitHub.DownloadDescriptor.asset: makeDevAsset(
          "als-dev-Agda-2.8.0-macos-arm64.zip",
        ),
        release,
        saveAsFileName: "dev-als",
      }
    }

    let makeDevSource = (assetName: string): Connection__Download__Source.t => {
      let release = makeDevRelease()
      Connection__Download__Source.FromGitHub(Connection__Download__Channel.DevALS, {
        Connection__Download__GitHub.DownloadDescriptor.asset: makeDevAsset(assetName),
        release,
        saveAsFileName: "dev-als",
      })
    }

    let makeDevALSPlatform = (): Platform.t => {
      let devDescriptor = makeDevDescriptor()
      {
        module MockDevALSPlatform = {
          let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
          let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
          let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
          let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
            switch channel {
            | Connection__Download__Channel.DevALS =>
              Ok(
                Connection__Download__Source.FromGitHub(
                  Connection__Download__Channel.DevALS,
                  devDescriptor,
                ),
              )
            | _ => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
            }
          )
          let download = (_globalStorageUri, _, ~trace as _trace=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
        }
        module(MockDevALSPlatform)
      }
    }

    let fileParentUri = (fileUri: VSCode.Uri.t): VSCode.Uri.t =>
      VSCode.Uri.file(NodeJs.Path.dirname(VSCode.Uri.fsPath(fileUri)))

    let getDevALSAvailability = async (storageUri, platform, ~configPaths=[]) =>
      await Connection__Download__Availability.getAll(
        Memento.make(None),
        storageUri,
        platform,
        configPaths,
        ~channel=Connection__Download__Channel.DevALS,
        ~downloadUnavailable="Not available for this platform",
      )

    Async.it(
      "should return 6 items (3 native + 3 wasm) for MacOS_Arm with 3 Agda versions",
      async () => {
        await withTempStorage("agda-devals-availability-", async (_tempDir, storageUri) => {
          let platform = makeDevALSPlatform()
          let downloadItems = await getDevALSAvailability(storageUri, platform)

          Assert.deepStrictEqual(downloadItems, [
            {
              downloaded: false,
              versionString: "Agda v2.8.0 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.8.0 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
          ])
        })
      },
    )

    Async.it(
      "should mark only the matching DevALS native source as downloaded",
      async () => {
        await withTempStorage("agda-devals-source-downloaded-", async (_tempDir, storageUri) => {
          let platform = makeDevALSPlatform()
          let downloadedSource = makeDevSource("als-dev-Agda-2.8.0-macos-arm64.zip")
          let otherSource = makeDevSource("als-dev-Agda-2.7.0.1-macos-arm64.zip")
          let downloadedNative = Connection__Download.expectedUriForSource(storageUri, downloadedSource)
          let downloadedDir = fileParentUri(downloadedNative)
          Assert.notDeepStrictEqual(
            Connection__Download.expectedPathForSource(storageUri, downloadedSource),
            Connection__Download.expectedPathForSource(storageUri, otherSource),
          )
          let _ = await FS.createDirectory(downloadedDir)
          let _ = await FS.writeFile(downloadedNative, Uint8Array.fromLength(0))

          let downloadItems = await getDevALSAvailability(storageUri, platform)

          Assert.deepStrictEqual(downloadItems, [
            {
              downloaded: true,
              versionString: "Agda v2.8.0 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.8.0 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
          ])
        })
      },
    )

    Async.it(
      "should suppress only the configured matching DevALS native source",
      async () => {
        await withTempStorage("agda-devals-source-suppress-", async (_tempDir, storageUri) => {
          let platform = makeDevALSPlatform()
          let downloadedSource = makeDevSource("als-dev-Agda-2.8.0-macos-arm64.zip")
          let downloadedNative = Connection__Download.expectedUriForSource(storageUri, downloadedSource)
          let downloadedDir = fileParentUri(downloadedNative)
          let _ = await FS.createDirectory(downloadedDir)
          let _ = await FS.writeFile(downloadedNative, Uint8Array.fromLength(0))

          let downloadItems = await getDevALSAvailability(
            storageUri,
            platform,
            ~configPaths=[Connection__Download.expectedPathForSource(storageUri, downloadedSource)],
          )

          Assert.deepStrictEqual(downloadItems, [
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            },
            {
              downloaded: false,
              versionString: "Agda v2.8.0 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
          ])
        })
      },
    )

    it("Connection__Download__Channel.Dev.toDownloadOrder should pick the wasm asset for Web platform", () => {
      let releases = [makeDevRelease()]
      let result = Connection__Download__Channel.Dev.toDownloadOrder(releases, Connection__Download__Platform.Web)
      switch result {
      | Ok(descriptor) =>
        Assert.deepStrictEqual(descriptor.asset.name, "als-dev-Agda-2.8.0-wasm.wasm")
      | Error(_) => Assert.fail("expected Ok result, got Error")
      }
    })

    Async.it(
      "should return 3 wasm items for Web platform with DevALS",
      async () => {
        await withTempStorage("agda-devals-web-availability-", async (_tempDir, storageUri) => {
          let releases = [makeDevRelease()]
          let platform: Platform.t = {
            module MockWebDevALSPlatform = {
              let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
              let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
              let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
              let resolveDownloadChannel = (channel, _useCache) =>
                async (_memento, _storageUri, downloadPlatform) =>
                  switch channel {
                  | Connection__Download__Channel.DevALS =>
                    Connection__Download__Channel.Dev.toDownloadOrder(releases, downloadPlatform)
                    ->Result.map(descriptor =>
                      Connection__Download__Source.FromGitHub(
                        Connection__Download__Channel.DevALS,
                        descriptor,
                      )
                    )
                  | _ => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
                  }
              let download = (
                _globalStorageUri,
                _,
                ~trace as _trace=Connection__Download__Trace.noop,
              ) =>
                Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
              let findCommand = (_command, ~timeout as _timeout=1000) =>
                Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
            module(MockWebDevALSPlatform)
          }

          let downloadItems = await getDevALSAvailability(storageUri, platform)

          Assert.deepStrictEqual(downloadItems, [
            {
              downloaded: false,
              versionString: "Agda v2.8.0 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server (dev build)",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
          ])
        })
      },
    )
  })

  describe("Availability — LatestALS", () => {
    let makeLatestAsset = (name): Connection__Download__GitHub.Asset.t => {
      url: "https://github.com/agda/agda-language-server/releases/download/v6/" ++ name,
      id: 0,
      node_id: "",
      name,
      label: Some(""),
      content_type: name->String.endsWith(".wasm") ? "application/wasm" : "application/zip",
      state: "uploaded",
      size: 1000000,
      created_at: "2025-01-01T00:00:00Z",
      updated_at: "2025-01-01T00:00:00Z",
      browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/" ++ name,
    }

    let makeLatestRelease = (): Connection__Download__GitHub.Release.t => {
      url: "https://api.github.com/repos/agda/agda-language-server/releases/v6",
      assets_url: "https://api.github.com/repos/agda/agda-language-server/releases/v6/assets",
      upload_url: "https://uploads.github.com/repos/agda/agda-language-server/releases/v6/assets",
      html_url: "https://github.com/agda/agda-language-server/releases/tag/v6",
      id: 2,
      node_id: "v6",
      tag_name: "v6",
      target_commitish: "main",
      name: "v6",
      draft: false,
      prerelease: false,
      created_at: "2025-01-01T00:00:00Z",
      published_at: "2025-01-01T00:00:00Z",
      assets: [
        makeLatestAsset("als-v6-Agda-2.8.0-macos-arm64.zip"),
        makeLatestAsset("als-v6-Agda-2.8.0-macos-x64.zip"),
        makeLatestAsset("als-v6-Agda-2.8.0-ubuntu.zip"),
        makeLatestAsset("als-v6-Agda-2.8.0-windows.zip"),
        makeLatestAsset("als-v6-Agda-2.8.0-wasm.wasm"),
        makeLatestAsset("als-v6-Agda-2.7.0.1-macos-arm64.zip"),
        makeLatestAsset("als-v6-Agda-2.7.0.1-macos-x64.zip"),
        makeLatestAsset("als-v6-Agda-2.7.0.1-ubuntu.zip"),
        makeLatestAsset("als-v6-Agda-2.7.0.1-windows.zip"),
        makeLatestAsset("als-v6-Agda-2.7.0.1-wasm.wasm"),
        makeLatestAsset("als-v6-Agda-2.6.4.3-macos-arm64.zip"),
        makeLatestAsset("als-v6-Agda-2.6.4.3-macos-x64.zip"),
        makeLatestAsset("als-v6-Agda-2.6.4.3-ubuntu.zip"),
        makeLatestAsset("als-v6-Agda-2.6.4.3-windows.zip"),
        makeLatestAsset("als-v6-Agda-2.6.4.3-wasm.wasm"),
      ],
      tarball_url: "https://api.github.com/repos/agda/agda-language-server/tarball/v6",
      zipball_url: "https://api.github.com/repos/agda/agda-language-server/zipball/v6",
      body: Some("Latest stable build"),
    }

    it("Connection__Download__Channel.Latest.toDownloadOrder should pick the wasm asset for Web platform", () => {
      let releases = [makeLatestRelease()]
      let result = Connection__Download__Channel.Latest.toDownloadOrder(releases, Connection__Download__Platform.Web)
      switch result {
      | Ok(descriptor) =>
        Assert.deepStrictEqual(descriptor.asset.name, "als-v6-Agda-2.8.0-wasm.wasm")
      | Error(_) => Assert.fail("expected Ok result, got Error")
      }
    })

    it("should not pin v0.2.7.0.1.5 when v6 exists", () => {
      let makeAsset = (name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/v6/" ++ name,
        id: 1,
        node_id: "",
        name,
        label: None,
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2025-01-01T00:00:00Z",
        updated_at: "2025-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/v6/" ++ name,
      }

      let makeRelease = (name, assets): Connection__Download__GitHub.Release.t => {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 1,
        node_id: "",
        tag_name: name,
        target_commitish: "main",
        name,
        draft: false,
        prerelease: false,
        created_at: "2025-01-01T00:00:00Z",
        published_at: "2025-01-01T00:00:00Z",
        assets,
        tarball_url: "",
        zipball_url: "",
        body: None,
      }

      let pinnedRelease = makeRelease("v0.2.7.0.1.5", [
        makeAsset("als-v0.2.7.0.1.5-Agda-2.6.3-windows.zip"),
      ])
      let v6Release = makeRelease("v6", [
        makeAsset("als-v6-Agda-2.8.0-windows.zip"),
      ])

      let releases = [pinnedRelease, v6Release]

      let result = Connection__Download__Channel.Latest.toDownloadOrder(releases, Connection__Download__Platform.Windows)

      switch result {
      | Error(_) => Assert.fail("expected Ok but got Error")
      | Ok(descriptor) =>
        Assert.deepStrictEqual(descriptor.release.name, "v6")
        Assert.deepStrictEqual(descriptor.asset.name, "als-v6-Agda-2.8.0-windows.zip")
      }
    })

    it("should fall back to older release when newest lacks a compatible asset", () => {
      let makeAsset = (tag, name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
        id: 1,
        node_id: "",
        name,
        label: None,
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2025-01-01T00:00:00Z",
        updated_at: "2025-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
      }

      let makeRelease = (tag, published_at, assets): Connection__Download__GitHub.Release.t => {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 1,
        node_id: "",
        tag_name: tag,
        target_commitish: "main",
        name: tag,
        draft: false,
        prerelease: false,
        created_at: published_at,
        published_at,
        assets,
        tarball_url: "",
        zipball_url: "",
        body: None,
      }

      let v7Release = makeRelease("v7", "2025-06-01T00:00:00Z", [
        makeAsset("v7", "als-v7-Agda-2.9.0-ubuntu.zip"),
      ])
      let v6Release = makeRelease("v6", "2025-01-01T00:00:00Z", [
        makeAsset("v6", "als-v6-Agda-2.8.0-windows.zip"),
      ])

      let releases = [v7Release, v6Release]

      let result = Connection__Download__Channel.Latest.toDownloadOrder(releases, Connection__Download__Platform.Windows)

      switch result {
      | Error(_) => Assert.fail("expected Ok but got Error")
      | Ok(descriptor) =>
        Assert.deepStrictEqual(descriptor.release.name, "v6")
        Assert.deepStrictEqual(descriptor.asset.name, "als-v6-Agda-2.8.0-windows.zip")
      }
    })

    it("should skip prerelease/draft releases and pick stable v6", () => {
      let makeAsset = (tag, name): Connection__Download__GitHub.Asset.t => {
        url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
        id: 1,
        node_id: "",
        name,
        label: None,
        content_type: "application/zip",
        state: "uploaded",
        size: 1000000,
        created_at: "2025-01-01T00:00:00Z",
        updated_at: "2025-01-01T00:00:00Z",
        browser_download_url: "https://github.com/agda/agda-language-server/releases/download/" ++ tag ++ "/" ++ name,
      }

      let v8DraftRelease: Connection__Download__GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 3,
        node_id: "",
        tag_name: "v8-draft",
        target_commitish: "main",
        name: "v8-draft",
        draft: true,
        prerelease: false,
        created_at: "2025-09-01T00:00:00Z",
        published_at: "2025-09-01T00:00:00Z",
        assets: [makeAsset("v8-draft", "als-v8-draft-Agda-3.0.0-windows.zip")],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }
      let v7PreRelease: Connection__Download__GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 2,
        node_id: "",
        tag_name: "v7-rc1",
        target_commitish: "main",
        name: "v7-rc1",
        draft: false,
        prerelease: true,
        created_at: "2025-06-01T00:00:00Z",
        published_at: "2025-06-01T00:00:00Z",
        assets: [makeAsset("v7-rc1", "als-v7-rc1-Agda-2.9.0-windows.zip")],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }
      let v6Release: Connection__Download__GitHub.Release.t = {
        url: "",
        assets_url: "",
        upload_url: "",
        html_url: "",
        id: 1,
        node_id: "",
        tag_name: "v6",
        target_commitish: "main",
        name: "v6",
        draft: false,
        prerelease: false,
        created_at: "2025-01-01T00:00:00Z",
        published_at: "2025-01-01T00:00:00Z",
        assets: [makeAsset("v6", "als-v6-Agda-2.8.0-windows.zip")],
        tarball_url: "",
        zipball_url: "",
        body: None,
      }

      let releases = [v8DraftRelease, v7PreRelease, v6Release]

      let result = Connection__Download__Channel.Latest.toDownloadOrder(releases, Connection__Download__Platform.Windows)

      switch result {
      | Error(_) => Assert.fail("expected Ok but got Error")
      | Ok(descriptor) =>
        Assert.deepStrictEqual(descriptor.release.name, "v6")
        Assert.deepStrictEqual(descriptor.asset.name, "als-v6-Agda-2.8.0-windows.zip")
      }
    })

    Async.it(
      "should return 3 wasm items for Web platform with LatestALS",
      async () => {
        await withTempStorage("agda-latestals-web-availability-", async (_tempDir, storageUri) => {
          let releases = [makeLatestRelease()]
          let platform: Platform.t = {
            module MockWebLatestALSPlatform = {
              let determinePlatform = async () => Ok(Connection__Download__Platform.Web)
              let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
              let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
              let resolveDownloadChannel = (channel, _useCache) =>
                async (_memento, _storageUri, downloadPlatform) =>
                  switch channel {
                  | Connection__Download__Channel.LatestALS =>
                    Connection__Download__Channel.Latest.toDownloadOrder(releases, downloadPlatform)
                    ->Result.map(descriptor =>
                      Connection__Download__Source.FromGitHub(
                        Connection__Download__Channel.LatestALS,
                        descriptor,
                      )
                    )
                  | _ => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
                  }
              let download = (
                _globalStorageUri,
                _,
                ~trace as _trace=Connection__Download__Trace.noop,
              ) =>
                Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
              let findCommand = (_command, ~timeout as _timeout=1000) =>
                Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
            module(MockWebLatestALSPlatform)
          }

          let downloadItems = await Connection__Download__Availability.getAll(
            Memento.make(None),
            storageUri,
            platform,
            [],
            ~channel=Connection__Download__Channel.LatestALS,
            ~downloadUnavailable="Not available for this platform",
          )

          Assert.deepStrictEqual(downloadItems, [
            {
              downloaded: false,
              versionString: "Agda v2.8.0 Language Server v6",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.7.0.1 Language Server v6",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
            {
              downloaded: false,
              versionString: "Agda v2.6.4.3 Language Server v6",
              platform: Connection__Download__DownloadArtifact.Platform.Wasm,
            },
          ])
        })
      },
    )
  })

  describe("Flow.sourceForSelection", () => {
    let makeAsset = (name): GitHub.Asset.t => {
      url: "https://github.com/agda/agda-language-server/releases/download/test/" ++ name,
      id: 0,
      node_id: "",
      name,
      label: Some(""),
      content_type: name->String.endsWith(".wasm") ? "application/wasm" : "application/zip",
      state: "uploaded",
      size: 1000000,
      created_at: "2024-01-01T00:00:00Z",
      updated_at: "2024-01-01T00:00:00Z",
      browser_download_url: "https://github.com/agda/agda-language-server/releases/download/test/" ++ name,
    }

    let makeRelease = (tagName, assets): GitHub.Release.t => {
      url: "https://api.github.com/repos/agda/agda-language-server/releases/" ++ tagName,
      assets_url: "https://api.github.com/repos/agda/agda-language-server/releases/" ++ tagName ++ "/assets",
      upload_url: "https://uploads.github.com/repos/agda/agda-language-server/releases/" ++ tagName ++ "/assets",
      html_url: "https://github.com/agda/agda-language-server/releases/tag/" ++ tagName,
      id: 1,
      node_id: tagName,
      tag_name: tagName,
      target_commitish: "main",
      name: tagName,
      draft: false,
      prerelease: tagName == "dev",
      created_at: "2024-01-01T00:00:00Z",
      published_at: "2024-01-01T00:00:00Z",
      assets,
      tarball_url: "",
      zipball_url: "",
      body: Some(""),
    }

    let makeGitHubSource = (channel, release, asset, saveAsFileName) =>
      Connection__Download__Source.FromGitHub(channel, {
        GitHub.DownloadDescriptor.asset,
        release,
        saveAsFileName,
      })

    let makePlatform = (downloadPlatform, resolvedSource): Platform.t => {
      module MockFlowPlatform = {
        let determinePlatform = async () => Ok(downloadPlatform)
        let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
        let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
        let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(_channel => Ok(resolvedSource))
        let download = (_globalStorageUri, _, ~trace as _trace=Connection__Download__Trace.noop) =>
          Promise.resolve(Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
        let findCommand = (_command, ~timeout as _timeout=1000) =>
          Promise.resolve(Error(Connection__Command.Error.NotFound))
      }
      module(MockFlowPlatform)
    }

    let expectGitHubAsset = (result, expectedChannel, expectedAssetName) =>
      switch result {
      | Ok(Connection__Download__Source.FromGitHub(channel, descriptor)) =>
        Assert.deepStrictEqual(channel, expectedChannel)
        Assert.deepStrictEqual(descriptor.asset.name, expectedAssetName)
      | Ok(Connection__Download__Source.FromURL(_, _, _)) =>
        Assert.fail("expected FromGitHub source")
      | Error(error) =>
        Assert.fail("expected Ok source, got " ++ Connection__Download__Error.toString(error))
      }

    Async.it(
      "should return the matching DevALS native source for the selected version string",
      async () => {
        let nativeAsset = makeAsset("als-dev-Agda-2.8.0-macos-arm64.zip")
        let release = makeRelease("dev", [
          nativeAsset,
          makeAsset("als-dev-Agda-2.7.0.1-macos-arm64.zip"),
          makeAsset("als-dev-Agda-2.8.0-wasm.wasm"),
        ])
        let platform = makePlatform(
          Connection__Download__Platform.MacOS_Arm,
          makeGitHubSource(Connection__Download__Channel.DevALS, release, nativeAsset, "dev-als"),
        )

        let result = await Connection__Download__Flow.sourceForSelection(
          Memento.make(None),
          VSCode.Uri.file("/tmp/agda-flow-dev-native"),
          platform,
          ~channel=Connection__Download__Channel.DevALS,
          ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
          ~versionString="Agda v2.8.0 Language Server (dev build)",
        )

        expectGitHubAsset(
          result,
          Connection__Download__Channel.DevALS,
          "als-dev-Agda-2.8.0-macos-arm64.zip",
        )
      },
    )

    Async.it(
      "should return the matching DevALS WASM source for the selected version string",
      async () => {
        let nativeAsset = makeAsset("als-dev-Agda-2.8.0-macos-arm64.zip")
        let wasmAsset = makeAsset("als-dev-Agda-2.8.0-wasm.wasm")
        let release = makeRelease("dev", [
          nativeAsset,
          wasmAsset,
          makeAsset("als-dev-Agda-2.7.0.1-wasm.wasm"),
        ])
        let platform = makePlatform(
          Connection__Download__Platform.MacOS_Arm,
          makeGitHubSource(Connection__Download__Channel.DevALS, release, nativeAsset, "dev-als"),
        )

        let result = await Connection__Download__Flow.sourceForSelection(
          Memento.make(None),
          VSCode.Uri.file("/tmp/agda-flow-dev-wasm"),
          platform,
          ~channel=Connection__Download__Channel.DevALS,
          ~platform=Connection__Download__DownloadArtifact.Platform.Wasm,
          ~versionString="Agda v2.8.0 Language Server (dev build)",
        )

        expectGitHubAsset(
          result,
          Connection__Download__Channel.DevALS,
          "als-dev-Agda-2.8.0-wasm.wasm",
        )
      },
    )

    Async.it("should return CannotFindCompatibleALSRelease when no source matches", async () => {
      let nativeAsset = makeAsset("als-dev-Agda-2.8.0-macos-arm64.zip")
      let release = makeRelease("dev", [nativeAsset])
      let platform = makePlatform(
        Connection__Download__Platform.MacOS_Arm,
        makeGitHubSource(Connection__Download__Channel.DevALS, release, nativeAsset, "dev-als"),
      )

      let result = await Connection__Download__Flow.sourceForSelection(
        Memento.make(None),
        VSCode.Uri.file("/tmp/agda-flow-no-match"),
        platform,
        ~channel=Connection__Download__Channel.DevALS,
        ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
        ~versionString="Agda v9.9.9 Language Server (dev build)",
      )

      Assert.deepStrictEqual(result, Error(Connection__Download__Error.CannotFindCompatibleALSRelease))
    })

    Async.it("should return FromURL sources directly", async () => {
      let source = Connection__Download__Source.FromURL(
        Connection__Download__Channel.DevALS,
        "https://example.invalid/als.wasm",
        "dev-als",
      )
      let platform = makePlatform(Connection__Download__Platform.Web, source)

      let result = await Connection__Download__Flow.sourceForSelection(
        Memento.make(None),
        VSCode.Uri.file("/tmp/agda-flow-url"),
        platform,
        ~channel=Connection__Download__Channel.DevALS,
        ~platform=Connection__Download__DownloadArtifact.Platform.Wasm,
        ~versionString="irrelevant for URL sources",
      )

      Assert.deepStrictEqual(result, Ok(source))
    })

    Async.it("should use canonical LatestALS release assets", async () => {
      let latestAsset = makeAsset("als-v6-Agda-2.8.0-macos-arm64.zip")
      let release = makeRelease("v6", [
        latestAsset,
        makeAsset("als-v6-Agda-2.8.0-wasm.wasm"),
        makeAsset("als-v5-Agda-2.7.0.1-macos-arm64.zip"),
      ])
      let platform = makePlatform(
        Connection__Download__Platform.MacOS_Arm,
        makeGitHubSource(Connection__Download__Channel.LatestALS, release, latestAsset, "latest-als"),
      )

      let result = await Connection__Download__Flow.sourceForSelection(
        Memento.make(None),
        VSCode.Uri.file("/tmp/agda-flow-latest"),
        platform,
        ~channel=Connection__Download__Channel.LatestALS,
        ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
        ~versionString="Agda v2.8.0 Language Server v6",
      )

      expectGitHubAsset(
        result,
        Connection__Download__Channel.LatestALS,
        "als-v6-Agda-2.8.0-macos-arm64.zip",
      )
    })
  })

  describe("ManagedStorage.findAnyWasmDownloaded", () => {
    Async.it(
      "should return Some(uri) when WASM artifact exists in release-managed storage",
      async () => {
        await withTempStorage("wasm-found-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
          ])
          let wasmFile = NodeJs.Path.join([artifactDir, "als.wasm"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download__ManagedStorage.findAnyWasmDownloaded(globalStorageUri)

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(wasmFile)->VSCode.Uri.toString))
        })
      },
    )

    Async.it(
      "should return None when no WASM artifact exists",
      async () => {
        await withTempStorage("wasm-missing-", async (_tempDir, globalStorageUri) => {
          let result = await Connection__Download__ManagedStorage.findAnyWasmDownloaded(globalStorageUri)
          Assert.deepStrictEqual(result, None)
        })
      },
    )
  })

  describe("ManagedStorage.findAnyDownloadedForPlatform", () => {
    Async.it(
      "should prefer native artifact over WASM for MacOS_Arm platform",
      async () => {
        await withTempStorage("platform-devals-both-", async (tempDir, globalStorageUri) => {
          let nativeDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let wasmDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
          ])
          let nativeExecutable = NodeJs.Path.join([nativeDir, "als"])
          let wasmFile = NodeJs.Path.join([wasmDir, "als.wasm"])

          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          await NodeJs.Fs.mkdir(wasmDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativeExecutable, NodeJs.Buffer.fromString("mock executable"))
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download__ManagedStorage.findAnyDownloadedForPlatform(
            globalStorageUri,
            Connection__Download__Platform.MacOS_Arm,
          )

          Assert.deepStrictEqual(
            result,
            Some(VSCode.Uri.file(nativeExecutable)->VSCode.Uri.fsPath),
          )
        })
      },
    )

    Async.it(
      "should fall back to WASM when no native artifact matches the platform",
      async () => {
        await withTempStorage("platform-devals-wasm-only-", async (tempDir, globalStorageUri) => {
          let wasmDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
          ])
          let wasmFile = NodeJs.Path.join([wasmDir, "als.wasm"])

          await NodeJs.Fs.mkdir(wasmDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download__ManagedStorage.findAnyDownloadedForPlatform(
            globalStorageUri,
            Connection__Download__Platform.MacOS_Arm,
          )

          let wasmUri = VSCode.Uri.joinPath(globalStorageUri, [
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
            "als.wasm",
          ])
          Assert.deepStrictEqual(result, Some(VSCode.Uri.toString(wasmUri)))
        })
      },
    )

    Async.it(
      "should return Some(path) when only native artifact exists for the platform",
      async () => {
        await withTempStorage("platform-native-only-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findAnyDownloadedForPlatform(
            globalStorageUri,
            Connection__Download__Platform.MacOS_Arm,
          )

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "should discover artifacts across all releases regardless of channel",
      async () => {
        await withTempStorage("platform-cross-channel-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findAnyDownloadedForPlatform(
            globalStorageUri,
            Connection__Download__Platform.MacOS_Arm,
          )

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "should ignore artifacts whose release directory does not match the artifact release tag",
      async () => {
        await withTempStorage("platform-mismatch-", async (tempDir, globalStorageUri) => {
          let mismatchedDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let mismatchedExecutable = NodeJs.Path.join([mismatchedDir, "als"])

          await NodeJs.Fs.mkdir(mismatchedDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(mismatchedExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findAnyDownloadedForPlatform(
            globalStorageUri,
            Connection__Download__Platform.MacOS_Arm,
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "should return None when releases directory does not exist",
      async () => {
        await withTempStorage("platform-no-releases-", async (_tempDir, globalStorageUri) => {
          let result = await Connection__Download__ManagedStorage.findAnyDownloadedForPlatform(
            globalStorageUri,
            Connection__Download__Platform.MacOS_Arm,
          )
          Assert.deepStrictEqual(result, None)
        })
      },
    )
  })

  describe("ManagedStorage.findWasmForManagedPath", () => {
    Async.it(
      "should return same-release WASM path for canonical native path",
      async () => {
        await withTempStorage("managed-wasm-for-native-", async (tempDir, globalStorageUri) => {
          let nativeDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let wasmDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-wasm",
          ])
          let nativeExecutable = NodeJs.Path.join([nativeDir, "als"])
          let wasmExecutable = NodeJs.Path.join([wasmDir, "als.wasm"])

          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          await NodeJs.Fs.mkdir(wasmDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativeExecutable, NodeJs.Buffer.fromString("mock native"))
          NodeJs.Fs.writeFileSync(wasmExecutable, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            nativeExecutable,
          )

          Assert.deepStrictEqual(
            result,
            Some(VSCode.Uri.file(wasmExecutable)->VSCode.Uri.toString),
          )
        })
      },
    )

    Async.it(
      "should return None for WASM input path",
      async () => {
        await withTempStorage("managed-wasm-input-", async (tempDir, globalStorageUri) => {
          let wasmDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-wasm",
          ])
          let wasmExecutable = NodeJs.Path.join([wasmDir, "als.wasm"])

          await NodeJs.Fs.mkdir(wasmDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmExecutable, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            wasmExecutable,
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "should return None when release directory and artifact tag mismatch",
      async () => {
        await withTempStorage("managed-wasm-mismatch-", async (tempDir, globalStorageUri) => {
          let nativeDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v7",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let nativeExecutable = NodeJs.Path.join([nativeDir, "als"])

          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativeExecutable, NodeJs.Buffer.fromString("mock native"))

          let result = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            nativeExecutable,
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "should return None when same-release WASM executable is missing",
      async () => {
        await withTempStorage("managed-wasm-missing-", async (tempDir, globalStorageUri) => {
          let nativeDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let nativeExecutable = NodeJs.Path.join([nativeDir, "als"])

          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativeExecutable, NodeJs.Buffer.fromString("mock native"))

          let result = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            nativeExecutable,
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "should return None for malformed, non-managed, or extra-segment paths",
      async () => {
        await withTempStorage("managed-wasm-malformed-", async (tempDir, globalStorageUri) => {
          let nativeDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let nativeExecutable = NodeJs.Path.join([nativeDir, "als"])
          let extraSegment = NodeJs.Path.join([nativeExecutable, "extra"])
          let nonManaged = NodeJs.Path.join([tempDir, "als"])

          await NodeJs.Fs.mkdir(nativeDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(nativeExecutable, NodeJs.Buffer.fromString("mock native"))

          let malformedResult = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            NodeJs.Path.join([tempDir, "releases", "dev", "not-an-artifact", "als"]),
          )
          let extraResult = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            extraSegment,
          )
          let nonManagedResult = await Connection__Download__ManagedStorage.findWasmForManagedPath(
            globalStorageUri,
            nonManaged,
          )

          Assert.deepStrictEqual(malformedResult, None)
          Assert.deepStrictEqual(extraResult, None)
          Assert.deepStrictEqual(nonManagedResult, None)
        })
      },
    )
  })

  describe("isDownloading", () => {
    Async.it(
      "should return false and create directory when global storage directory doesn't exist",
      async () => {
        // Create a unique test directory that doesn't exist
        let nonExistentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Verify it doesn't exist initially
        Assert.ok(!NodeJs.Fs.existsSync(nonExistentDir))

        let globalStorageUri = VSCode.Uri.file(nonExistentDir)
        let result = await GitHub.isDownloading(globalStorageUri)

        // Should return false (no download in progress)
        Assert.ok(!result)

        // Should have created the directory
        Assert.ok(NodeJs.Fs.existsSync(nonExistentDir))

        // Cleanup
        NodeJs.Fs.rmdirSync(nonExistentDir)
      },
    )

    Async.it(
      "should return false when directory exists but no in-flight download file is present",
      async () => {
        // Create a temporary directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-exists-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create the directory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        // Verify directory exists but no in-flight file
        Assert.ok(NodeJs.Fs.existsSync(tempDir))
        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])
        Assert.ok(!NodeJs.Fs.existsSync(inFlightFile))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await GitHub.isDownloading(globalStorageUri)

        // Should return false (no download in progress)
        Assert.ok(!result)

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return true when in-flight download file exists",
      async () => {
        // Create a temporary directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-inflight-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])

        // Create directory and in-flight file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(inFlightFile, NodeJs.Buffer.fromString("downloading..."))

        // Verify both directory and in-flight file exist
        Assert.ok(NodeJs.Fs.existsSync(tempDir))
        Assert.ok(NodeJs.Fs.existsSync(inFlightFile))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await GitHub.isDownloading(globalStorageUri)

        // Should return true (download in progress)
        Assert.ok(result)

        // Cleanup
        NodeJs.Fs.unlinkSync(inFlightFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return false when directory exists with other files but no in-flight download file",
      async () => {
        // Create a temporary directory with other files
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-otherfiles-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let otherFile = NodeJs.Path.join([tempDir, "some-other-file.txt"])
        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])

        // Create directory and other files, but NOT the in-flight file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(otherFile, NodeJs.Buffer.fromString("other content"))

        // Verify directory and other file exist, but no in-flight file
        Assert.ok(NodeJs.Fs.existsSync(tempDir))
        Assert.ok(NodeJs.Fs.existsSync(otherFile))
        Assert.ok(!NodeJs.Fs.existsSync(inFlightFile))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await GitHub.isDownloading(globalStorageUri)

        // Should return false (no download in progress)
        Assert.ok(!result)

        // Cleanup
        NodeJs.Fs.unlinkSync(otherFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should handle multiple calls consistently when no download is in progress",
      async () => {
        // Create a temporary directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-multiple-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create the directory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let globalStorageUri = VSCode.Uri.file(tempDir)

        // Call isDownloading multiple times
        let result1 = await GitHub.isDownloading(globalStorageUri)
        let result2 = await GitHub.isDownloading(globalStorageUri)
        let result3 = await GitHub.isDownloading(globalStorageUri)

        // All should return false consistently
        Assert.ok(!result1)
        Assert.ok(!result2)
        Assert.ok(!result3)

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should handle multiple calls consistently when download is in progress",
      async () => {
        // Create a temporary directory with in-flight file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-multiple-progress-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])

        // Create directory and in-flight file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(inFlightFile, NodeJs.Buffer.fromString("downloading..."))

        let globalStorageUri = VSCode.Uri.file(tempDir)

        // Call isDownloading multiple times
        let result1 = await GitHub.isDownloading(globalStorageUri)
        let result2 = await GitHub.isDownloading(globalStorageUri)
        let result3 = await GitHub.isDownloading(globalStorageUri)

        // All should return true consistently
        Assert.ok(result1)
        Assert.ok(result2)
        Assert.ok(result3)

        // Cleanup
        NodeJs.Fs.unlinkSync(inFlightFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should transition from not downloading to downloading when in-flight file is created",
      async () => {
        // Create a temporary directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-transition-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])

        // Create the directory without in-flight file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let globalStorageUri = VSCode.Uri.file(tempDir)

        // Initially should return false
        let result1 = await GitHub.isDownloading(globalStorageUri)
        Assert.ok(!result1)

        // Create the in-flight file
        NodeJs.Fs.writeFileSync(inFlightFile, NodeJs.Buffer.fromString("downloading..."))

        // Now should return true
        let result2 = await GitHub.isDownloading(globalStorageUri)
        Assert.ok(result2)

        // Remove the in-flight file
        NodeJs.Fs.unlinkSync(inFlightFile)

        // Should return false again
        let result3 = await GitHub.isDownloading(globalStorageUri)
        Assert.ok(!result3)

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should handle edge case with empty in-flight file",
      async () => {
        // Create a temporary directory with empty in-flight file
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-empty-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])

        // Create directory and empty in-flight file
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(inFlightFile, NodeJs.Buffer.fromString(""))

        // Verify files exist
        Assert.ok(NodeJs.Fs.existsSync(tempDir))
        Assert.ok(NodeJs.Fs.existsSync(inFlightFile))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await GitHub.isDownloading(globalStorageUri)

        // Should still return true (file exists, even if empty)
        Assert.ok(result)

        // Cleanup
        NodeJs.Fs.unlinkSync(inFlightFile)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should create directory with proper permissions when it doesn't exist",
      async () => {
        // Create a unique test directory that doesn't exist
        let nonExistentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-isdownloading-permissions-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Verify it doesn't exist initially
        Assert.ok(!NodeJs.Fs.existsSync(nonExistentDir))

        let globalStorageUri = VSCode.Uri.file(nonExistentDir)
        let result = await GitHub.isDownloading(globalStorageUri)

        // Should return false (no download in progress)
        Assert.ok(!result)

        // Should have created the directory and it should be accessible
        Assert.ok(NodeJs.Fs.existsSync(nonExistentDir))

        // Verify we can write to the directory by creating a test file
        let testFile = NodeJs.Path.join([nonExistentDir, "test.txt"])
        NodeJs.Fs.writeFileSync(testFile, NodeJs.Buffer.fromString("test"))
        Assert.ok(NodeJs.Fs.existsSync(testFile))

        // Cleanup
        NodeJs.Fs.unlinkSync(testFile)
        NodeJs.Fs.rmdirSync(nonExistentDir)
      },
    )
  })

  describe("chmodExecutable", () => {
    Async.it(
      "should set the executable bit on a file",
      async () => {
        let tempFile = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "chmod-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString("mock binary"))
        await NodeJs.Fs.chmod(tempFile, ~mode=0o644)

        let statsBefore = NodeJs.Fs.lstatSync(#String(tempFile))
        Assert.deepStrictEqual(land(statsBefore.mode, 0o111), 0)

        let result = await GitHub.chmodExecutable(tempFile)
        Assert.deepStrictEqual(result, Ok())

        let statsAfter = NodeJs.Fs.lstatSync(#String(tempFile))
        Assert.ok(land(statsAfter.mode, 0o111) != 0)

        NodeJs.Fs.unlinkSync(tempFile)
      },
    )
  })

  describe("download — directory vs file cache check", () => {
    Async.it(
      "should NOT treat existing directory as cached when binary file is missing",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-dl-cache-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        // Create dev-als directory but NOT the als binary (simulating failed previous download)
        let devAlsDir = NodeJs.Path.join([tempDir, "dev-als"])
        await NodeJs.Fs.mkdir(devAlsDir, {recursive: true, mode: 0o777})

        let globalStorageUri = VSCode.Uri.file(tempDir)

        let fakeAsset: GitHub.Asset.t = {
          url: "https://invalid.example.com/fake-asset.zip",
          id: 0,
          node_id: "",
          name: "als-dev-Agda-2.8.0-macos-arm64.zip",
          label: None,
          content_type: "application/zip",
          state: "uploaded",
          size: 0,
          created_at: "",
          updated_at: "",
          browser_download_url: "https://invalid.example.com/fake-asset.zip",
        }
        let fakeRelease: GitHub.Release.t = {
          url: "",
          assets_url: "",
          upload_url: "",
          html_url: "",
          id: 0,
          node_id: "",
          tag_name: "dev",
          target_commitish: "",
          name: "dev",
          draft: false,
          prerelease: true,
          created_at: "",
          published_at: "",
          assets: [fakeAsset],
          tarball_url: "",
          zipball_url: "",
          body: None,
        }
        let descriptor: GitHub.DownloadDescriptor.t = {
          release: fakeRelease,
          asset: fakeAsset,
          saveAsFileName: "dev-als",
        }

        let result = await GitHub.download(descriptor, globalStorageUri, _ => ())

        // Should NOT return Ok(true) (cached) — als binary doesn't exist, only the directory
        // Should return Error(...) because the download attempt fails (invalid URL)
        Assert.deepStrictEqual(Result.isOk(result), false)

        // Cleanup
        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )
  })

  let makeFakeDescriptor = (name): (GitHub.Asset.t, GitHub.Release.t, GitHub.DownloadDescriptor.t) => {
    let asset: GitHub.Asset.t = {
      url: "https://invalid.example.com/fake.zip",
      id: 0, node_id: "", name,
      label: None, content_type: "application/zip", state: "uploaded",
      size: 0, created_at: "", updated_at: "",
      browser_download_url: "https://invalid.example.com/fake.zip",
    }
    let release: GitHub.Release.t = {
      url: "", assets_url: "", upload_url: "", html_url: "",
      id: 0, node_id: "", tag_name: "v1", target_commitish: "",
      name: "v1", draft: false, prerelease: false,
      created_at: "", published_at: "",
      assets: [asset], tarball_url: "", zipball_url: "", body: None,
    }
    let descriptor: GitHub.DownloadDescriptor.t = {
      release, asset, saveAsFileName: "test-als",
    }
    (asset, release, descriptor)
  }

  describe("download — in-flight sentinel ordering (Fix 3)", () => {
    Async.it(
      "should abort with CannotWriteInFlightFile when claiming in-flight sentinel fails",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-sentinel-write-fail-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-arm64.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        // Seam: sentinel write always fails
        let mockWriteSentinel = (_uri: VSCode.Uri.t) =>
          Promise.resolve(Error("simulated sentinel write failure"))

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~writeInFlightSentinel=mockWriteSentinel,
        )

        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(result, Error(GitHub.Error.CannotWriteInFlightFile("simulated sentinel write failure")))
      },
    )

    Async.it(
      "should write then delete sentinel when binary is already cached",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-sentinel-cache-hit-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let binaryDir = NodeJs.Path.join([tempDir, "test-als"])
        await NodeJs.Fs.mkdir(binaryDir, {recursive: true, mode: 0o777})
        // Binary is NOT pre-created — it appears only inside mockWriteSentinel.
        // This proves the cache check runs after sentinel write: if the cache
        // check ran first it would find no binary and proceed to download instead
        // of returning Ok(true).

        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-arm64.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        // Seams record operation sequence
        let ops = ref([])
        let mockWriteSentinel = (_uri: VSCode.Uri.t) => {
          ops := ops.contents->Array.concat(["write"])
          // Make the cached binary appear now, after the sentinel is claimed
          NodeJs.Fs.writeFileSync(NodeJs.Path.join([binaryDir, "als"]), NodeJs.Buffer.fromString("mock binary"))
          Promise.resolve(Ok())
        }
        let mockDeleteSentinel = (_uri: VSCode.Uri.t) => {
          ops := ops.contents->Array.concat(["delete"])
          Promise.resolve(Ok())
        }

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~writeInFlightSentinel=mockWriteSentinel,
          ~deleteInFlightSentinel=mockDeleteSentinel,
        )

        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(result, Ok(true))
        Assert.deepStrictEqual(ops.contents, ["write", "delete"])
      },
    )

    Async.it(
      "should return CannotDeleteFile when cached-binary path cannot release in-flight sentinel",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-sentinel-delete-fail-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let binaryDir = NodeJs.Path.join([tempDir, "test-als"])
        await NodeJs.Fs.mkdir(binaryDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(NodeJs.Path.join([binaryDir, "als"]), NodeJs.Buffer.fromString("mock binary"))

        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-arm64.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        let mockWriteSentinel = (_uri: VSCode.Uri.t) => Promise.resolve(Ok())
        let mockDeleteSentinel = (_uri: VSCode.Uri.t) =>
          Promise.resolve(Error("simulated sentinel delete failure"))

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~writeInFlightSentinel=mockWriteSentinel,
          ~deleteInFlightSentinel=mockDeleteSentinel,
        )

        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(result, Error(GitHub.Error.CannotDeleteFile("simulated sentinel delete failure")))
      },
    )
  })

  describe("download — post-unzip binary verification (Fix 4)", () => {
    Async.it(
      "should return BinaryMissingAfterExtraction when unzip succeeds but binary is absent",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-fix4-missing-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-arm64.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        // fetchFile: pretend download succeeded by writing the downloaded file at the given URI
        let mockFetchFile = (uri: VSCode.Uri.t, ~trace as _=Connection__Download__Trace.noop) => {
          NodeJs.Fs.writeFileSync(VSCode.Uri.fsPath(uri), NodeJs.Buffer.fromString("mock download"))
          Promise.resolve(Ok())
        }

        // unzip: succeed without creating destPath/als
        let mockUnzip = (_zipUri: VSCode.Uri.t, _destUri: VSCode.Uri.t) => Promise.resolve()

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~fetchFile=Some(mockFetchFile),
          ~unzip=Some(mockUnzip),
        )

        let _ = await FS.deleteRecursive(globalStorageUri)

        let expectedPath = NodeJs.Path.join([tempDir, "test-als", "als"])
        switch result {
        | Error(GitHub.Error.BinaryMissingAfterExtraction(path)) =>
          assertLocalPathEqual(path, expectedPath)
        | _ => Assert.fail("Expected BinaryMissingAfterExtraction error")
        }
      },
    )

    Async.it(
      "should return Ok(false) and delete zip when unzip succeeds and binary exists",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-fix4-exists-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-arm64.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        let mockFetchFile = (uri: VSCode.Uri.t, ~trace as _=Connection__Download__Trace.noop) => {
          NodeJs.Fs.writeFileSync(VSCode.Uri.fsPath(uri), NodeJs.Buffer.fromString("mock download"))
          Promise.resolve(Ok())
        }

        // unzip: succeed and create destPath/als
        let mockUnzip = (_zipUri: VSCode.Uri.t, destUri: VSCode.Uri.t) => {
          let binaryDir = VSCode.Uri.fsPath(destUri)
          NodeJs.Fs.mkdirSyncWith(binaryDir, {recursive: true})
          NodeJs.Fs.writeFileSync(
            NodeJs.Path.join([binaryDir, "als"]),
            NodeJs.Buffer.fromString("mock binary"),
          )
          Promise.resolve()
        }

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~fetchFile=Some(mockFetchFile),
          ~unzip=Some(mockUnzip),
        )

        let zipPath = NodeJs.Path.join([tempDir, "in-flight.download.zip"])
        let zipExistsAfter = NodeJs.Fs.existsSync(zipPath)
        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(result, Ok(false))
        Assert.deepStrictEqual(zipExistsAfter, false)
      },
    )

    Async.it(
      "should return CannotDeleteFile when zip cleanup fails after successful unzip and binary stat",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-fix4-deletefail-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-arm64.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        let mockFetchFile = (uri: VSCode.Uri.t, ~trace as _=Connection__Download__Trace.noop) => {
          NodeJs.Fs.writeFileSync(VSCode.Uri.fsPath(uri), NodeJs.Buffer.fromString("mock download"))
          Promise.resolve(Ok())
        }

        let mockUnzip = (_zipUri: VSCode.Uri.t, destUri: VSCode.Uri.t) => {
          let binaryDir = VSCode.Uri.fsPath(destUri)
          NodeJs.Fs.mkdirSyncWith(binaryDir, {recursive: true})
          NodeJs.Fs.writeFileSync(
            NodeJs.Path.join([binaryDir, "als"]),
            NodeJs.Buffer.fromString("mock binary"),
          )
          Promise.resolve()
        }

        let mockDeleteZip = (_uri: VSCode.Uri.t) =>
          Promise.resolve(Error("simulated zip delete failure"))

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~fetchFile=Some(mockFetchFile),
          ~unzip=Some(mockUnzip),
          ~deleteZip=Some(mockDeleteZip),
        )

        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(result, Error(GitHub.Error.CannotDeleteFile("simulated zip delete failure")))
      },
    )

    Async.it(
      "should return Ok(false) for WASM asset without requiring destPath/als",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-fix4-wasm-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        // WASM asset name — must not trigger native ZIP verification
        let (_, _, descriptor) = makeFakeDescriptor("als-Agda-2.8.0-macos-wasm.zip")
        let globalStorageUri = VSCode.Uri.file(tempDir)

        let mockFetchFile = (uri: VSCode.Uri.t, ~trace as _=Connection__Download__Trace.noop) => {
          NodeJs.Fs.writeFileSync(VSCode.Uri.fsPath(uri), NodeJs.Buffer.fromString("mock download"))
          Promise.resolve(Ok())
        }

        // If unzip is ever called for WASM this test fails, proving no regression
        let shouldNotCallUnzip = (_zipUri: VSCode.Uri.t, _destUri: VSCode.Uri.t) => {
          Assert.fail("unzip must not be called for WASM downloads")
          Promise.resolve()
        }

        let result = await GitHub.download(
          descriptor, globalStorageUri, _ => (),
          ~fetchFile=Some(mockFetchFile),
          ~unzip=Some(shouldNotCallUnzip),
        )

        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(result, Ok(false))
      },
    )
  })

  describe("download — trace wiring", () => {
    module Trace = Connection__Download__Trace
    module Download = Connection__Download

    let invalidUrl = "https://invalid.example.com/fake.zip"

    let makeFakeDescriptor = (): GitHub.DownloadDescriptor.t => {
      let asset: GitHub.Asset.t = {
        url: invalidUrl,
        id: 0, node_id: "", name: "als-Agda-2.8.0-macos-arm64.zip",
        label: None, content_type: "application/zip", state: "uploaded",
        size: 0, created_at: "", updated_at: "",
        browser_download_url: invalidUrl,
      }
      let release: GitHub.Release.t = {
        url: "", assets_url: "", upload_url: "", html_url: "",
        id: 0, node_id: "", tag_name: "v1", target_commitish: "",
        name: "v1", draft: false, prerelease: false,
        created_at: "", published_at: "",
        assets: [asset], tarball_url: "", zipball_url: "", body: None,
      }
      { release, asset, saveAsFileName: "dev-als" }
    }

    // Deterministic fetchFile mock: calls trace(FetchStarted) via the injected ~trace and returns
    // immediately without a network call, proving ~trace was forwarded from download to asFile
    let mockFetchFile = (
      _uri: VSCode.Uri.t,
      ~trace=Connection__Download__Trace.noop,
    ) => {
      trace(Trace.FetchStarted(invalidUrl))
      Promise.resolve(Error(Connection__Download__Util.Error.NoRedirectLocation))
    }

    // Ensures temp dir is deleted even if the test body throws
    let withTempDir = async (prefix, f) => {
      let tempDir = NodeJs.Path.join([
        NodeJs.Os.tmpdir(),
        prefix ++ string_of_int(int_of_float(Js.Date.now())),
      ])
      await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
      let uri = VSCode.Uri.file(tempDir)
      let cleanup = async () => {
        let _ = await FS.deleteRecursive(uri)
      }
      try {
        await f(uri)
        await cleanup()
      } catch {
      | exn =>
        await cleanup()
        raise(exn)
      }
    }

    Async.it(
      "GitHub.download should forward trace to asFile",
      async () => {
        await withTempDir("agda-trace-wire-", async globalStorageUri => {
          let traces: ref<array<Trace.t>> = ref([])
          let onTrace = event => { traces := traces.contents->Array.concat([event]) }

          let descriptor = makeFakeDescriptor()
          let _ = await GitHub.download(
            descriptor, globalStorageUri, _ => (),
            ~trace=onTrace,
            ~fetchFile=Some(mockFetchFile),
          )

          Assert.deepStrictEqual(
            traces.contents->Array.filter(t =>
              switch t {
              | Trace.FetchStarted(_) => true
              | _ => false
              }
            ),
            [Trace.FetchStarted(invalidUrl)],
          )
        })
      },
    )

    Async.it(
      "Connection__Download.download should forward trace to GitHub.download",
      async () => {
        await withTempDir("agda-trace-wire2-", async globalStorageUri => {
          let descriptor = makeFakeDescriptor()
          let source = Connection__Download__Source.FromGitHub(
            Connection__Download__Channel.DevALS,
            descriptor,
          )

          let traces: ref<array<Trace.t>> = ref([])
          let onTrace = event => { traces := traces.contents->Array.concat([event]) }

          let _ = await Download.download(globalStorageUri, source, ~trace=onTrace, ~fetchFile=Some(mockFetchFile))

          Assert.deepStrictEqual(
            traces.contents->Array.filter(t =>
              switch t {
              | Trace.FetchStarted(_) => true
              | _ => false
              }
            ),
            [Trace.FetchStarted(invalidUrl)],
          )
        })
      },
    )

    Async.it(
      "Connection__Download.download should forward trace to downloadFromURL (FromURL path)",
      async () => {
        await withTempDir("agda-trace-wire3-", async globalStorageUri => {
          let fromUrlUrl = "https://trace-test.example/trace-check.zip"
          let source = Connection__Download__Source.FromURL(
            Connection__Download__Channel.DevALS,
            fromUrlUrl,
            "trace-url-test",
          )

          let traces: ref<array<Trace.t>> = ref([])
          let onTrace = event => { traces := traces.contents->Array.concat([event]) }
          let mockFetch = (_url, _headers) =>
            Promise.resolve(Error(Connection__Download__Util.Error.NoRedirectLocation))

          let _ = await Download.download(globalStorageUri, source, ~trace=onTrace, ~fetch=mockFetch)

          Assert.deepStrictEqual(
            traces.contents->Array.filter(t =>
              switch t {
              | Trace.FetchStarted(_) => true
              | _ => false
              }
            ),
            [Trace.FetchStarted(fromUrlUrl)],
          )
        })
      },
    )

    Async.it(
      "handleDownload should emit DownloadTrace to state.channels.log",
      async () => {
        await withTempDir("agda-trace-log-", async storageUri => {
          let channels: State.channels = {
            inputMethod: Chan.make(),
            responseHandled: Chan.make(),
            commandHandled: Chan.make(),
            log: Chan.make(),
          }
          let mockEditor: VSCode.TextEditor.t = %raw(`{ document: { fileName: "test.agda" } }`)
          let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))
          let state = State.make(
            "test-id",
            Mock.Platform.makeBasic(),
            channels,
            storageUri,
            mockExtensionUri,
            Memento.make(None),
            mockEditor,
            None,
          )

          let logEvents: ref<array<Log.t>> = ref([])
          let _ = Chan.on(state.channels.log, event => {
            logEvents := logEvents.contents->Array.concat([event])
          })

          let platform: Platform.t = {
            module MockPlatform = {
              let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
              let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
              let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
              let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(_ =>
                Ok(Connection__Download__Source.FromURL(
                  Connection__Download__Channel.DevALS,
                  "mock://trace-test",
                  "dev-als",
                ))
              )
              let download = (_globalStorageUri, _source, ~trace=Connection__Download__Trace.noop) => {
                trace(Trace.FetchStarted("mock://trace-test"))
                Promise.resolve(Ok("/mock/path"))
              }
              let findCommand = (_command, ~timeout as _timeout=1000) =>
                Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
            module(MockPlatform)
          }

          await Connection__UI__Handlers.handleDownload(
            state,
            platform,
            Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            false,
            "ALS vTest",
            ~channel=Connection__Download__Channel.DevALS,
          )

          let hasDownloadTrace = logEvents.contents->Array.some(e =>
            switch e {
            | Log.DownloadTrace(Trace.FetchStarted("mock://trace-test")) => true
            | _ => false
            }
          )
          Assert.deepStrictEqual(hasDownloadTrace, true)
        })
      },
    )

    it("Log.toString renders DownloadTrace with stage and URL", () => {
      let event = Trace.FetchStarted("https://example.com/test.zip")
      Assert.deepStrictEqual(
        Log.toString(Log.DownloadTrace(event)),
        "[ DownloadTrace    ] FetchStarted: https://example.com/test.zip",
      )
    })

    it("Log.toString renders DownloadTrace Failed with stage and message", () => {
      let event = Trace.Failed("fetch", "connection refused")
      Assert.deepStrictEqual(
        Log.toString(Log.DownloadTrace(event)),
        "[ DownloadTrace    ] Failed(fetch): connection refused",
      )
    })
  })

  describe("Unzip.run", () => {
    // Race a promise against a timeout; clears the timer whether the tested promise wins or loses
    let withTimeout = (ms, p) => {
      let timerId: ref<option<Js.Global.timeoutId>> = ref(None)
      let clearTimer = () => timerId.contents->Option.forEach(id => Js.Global.clearTimeout(id))
      // Wrap p so clearTimer fires on resolution
      let wrappedP = p->Promise.then(v => {
        clearTimer()
        Promise.resolve(v)
      })
      // Also clear on rejection (fire-and-forget side chain)
      wrappedP->Promise.catch(_ => {
        clearTimer()
        Promise.resolve()
      })->ignore
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

    // Assert that a promise rejects; fails fast with a clear message if it hangs instead.
    // Distinguishes a genuine rejection from a timeout rejection so regressions are not masked.
    let expectToReject = async p => {
      let didReject = ref(false)
      try {
        await withTimeout(1000, p)
      } catch {
      | Js.Exn.Error(e) =>
        if Js.Exn.message(e)->Option.getOr("")->String.startsWith("timed out") {
          Assert.fail("expected rejection but timed out — error event listener not wired")
        } else {
          didReject := true
        }
      | _ => didReject := true
      }
      Assert.deepStrictEqual(didReject.contents, true)
    }

    Async.it(
      "attaches finish handler before piping",
      async () => {
        let fakeInput: NodeJs.Fs.ReadStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic
        let fakeOutput: NodeJs.Fs.WriteStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic

        // ~doPipe emits finish synchronously. If handlers were attached after doPipe,
        // the event fires before onFinishOnce is wired → p never resolves → withTimeout fails.
        let p = Unzip.run(
          VSCode.Uri.file("/fake/src"),
          VSCode.Uri.file("/fake/dest"),
          ~makeStreams=Some((_, _) => (fakeInput, fakeOutput)),
          ~doPipe=Some((_, e) => e->emitEvent("finish")->ignore),
        )
        await withTimeout(1000, p)
      },
    )

    Async.it(
      "should not resolve on input stream close, but should resolve on extractor finish",
      async () => {
        let fakeInput: NodeJs.Fs.ReadStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic
        let fakeOutput: NodeJs.Fs.WriteStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic

        let resolved = ref(false)
        let p = Unzip.run(
          VSCode.Uri.file("/fake/src"),
          VSCode.Uri.file("/fake/dest"),
          ~makeStreams=Some((_, _) => (fakeInput, fakeOutput)),
        )
        p->Promise.then(() => {
          resolved := true
          Promise.resolve()
        })->ignore

        // Emit input close — must NOT resolve (tests that we listen on extractor, not input)
        fakeInput->emitEvent("close")->ignore
        await Promise.resolve()
        Assert.deepStrictEqual(resolved.contents, false)

        // Emit extractor finish — MUST resolve; timeout guard ensures fast failure if not wired
        fakeOutput->emitEvent("finish")->ignore
        await withTimeout(1000, p)
        Assert.deepStrictEqual(resolved.contents, true)
      },
    )

    Async.it(
      "should reject when input stream emits error",
      async () => {
        let fakeInput: NodeJs.Fs.ReadStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic
        let fakeOutput: NodeJs.Fs.WriteStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic

        let p = Unzip.run(
          VSCode.Uri.file("/fake/src"),
          VSCode.Uri.file("/fake/dest"),
          ~makeStreams=Some((_, _) => (fakeInput, fakeOutput)),
        )

        let fakeErr: Js.Exn.t = %raw(`new Error("input stream error")`)
        fakeInput->emitEventWithArg("error", fakeErr)->ignore
        await expectToReject(p)
      },
    )

    Async.it(
      "should reject when extraction stream emits error",
      async () => {
        let fakeInput: NodeJs.Fs.ReadStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic
        let fakeOutput: NodeJs.Fs.WriteStream.t = NodeJs.Stream.PassThrough.make()->Obj.magic

        let p = Unzip.run(
          VSCode.Uri.file("/fake/src"),
          VSCode.Uri.file("/fake/dest"),
          ~makeStreams=Some((_, _) => (fakeInput, fakeOutput)),
        )

        let fakeErr: Js.Exn.t = %raw(`new Error("extraction stream error")`)
        fakeOutput->emitEventWithArg("error", fakeErr)->ignore
        await expectToReject(p)
      },
    )
  })

  describe("cleanupInFlightFiles", () => {
    Async.it(
      "should delete both in-flight files when they exist",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "cleanup-inflight-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])
        let inFlightZipFile = NodeJs.Path.join([tempDir, "in-flight.download.zip"])
        NodeJs.Fs.writeFileSync(inFlightFile, NodeJs.Buffer.fromString("downloading..."))
        NodeJs.Fs.writeFileSync(inFlightZipFile, NodeJs.Buffer.fromString("downloading..."))
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightFile), true)
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightZipFile), true)

        let globalStorageUri = VSCode.Uri.file(tempDir)
        await GitHub.cleanupInFlightFiles(globalStorageUri)

        let inFlightGone = !NodeJs.Fs.existsSync(inFlightFile)
        let inFlightZipGone = !NodeJs.Fs.existsSync(inFlightZipFile)
        let _ = await FS.deleteRecursive(globalStorageUri)

        Assert.deepStrictEqual(inFlightGone, true)
        Assert.deepStrictEqual(inFlightZipGone, true)
      },
    )

    Async.it(
      "should succeed when neither in-flight file exists",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "cleanup-inflight-empty-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let inFlightFile = NodeJs.Path.join([tempDir, "in-flight.download"])
        let inFlightZipFile = NodeJs.Path.join([tempDir, "in-flight.download.zip"])
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightFile), false)
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightZipFile), false)

        // Should not throw
        let globalStorageUri = VSCode.Uri.file(tempDir)
        await GitHub.cleanupInFlightFiles(globalStorageUri)

        Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightFile), false)
        Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightZipFile), false)

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )
  })

  describe("Integration Tests", () => {
    // will get HTTP 403 on GitHub CI macOS runners :(
    describe_skip(
      "`getReleaseManifestFromGitHub` without cache",
      () => {
        Async.it(
          "should fetch release from GitHub API",
          async () => {
            let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")
            let repo = Connection__Download__Channel.Latest.makeRepo(globalStorageUri)
            let memento = Memento.make(None)
            let releaseResult = await Connection__Download.getReleaseManifestFromGitHub(
              memento,
              repo,
              ~useCache=false,
            )

            switch releaseResult {
            | Error(e) =>
              Assert.fail("Failed to fetch releases: " ++ Connection__Download__Error.toString(e))
            | Ok(releases) =>
              // Verify we got releases from GitHub API
              Assert.ok(Array.length(releases) > 0)
            }
          },
        )
      },
    )
  })

  describe("Channel", () => {
    it("toString(LatestALS) == \"latest\"", () => {
      Assert.deepStrictEqual(Connection__Download__Channel.toString(Connection__Download__Channel.LatestALS), "latest")
    })

    it("toString(DevALS) == \"dev\"", () => {
      Assert.deepStrictEqual(Connection__Download__Channel.toString(Connection__Download__Channel.DevALS), "dev")
    })

    it("fromString(\"latest\") == Some(LatestALS)", () => {
      Assert.deepStrictEqual(Connection__Download__Channel.fromString("latest"), Some(Connection__Download__Channel.LatestALS))
    })

    it("fromString(\"dev\") == Some(DevALS)", () => {
      Assert.deepStrictEqual(Connection__Download__Channel.fromString("dev"), Some(Connection__Download__Channel.DevALS))
    })

    it("all is [DevALS, LatestALS]", () => {
      Assert.deepStrictEqual(Connection__Download__Channel.all, [Connection__Download__Channel.DevALS, Connection__Download__Channel.LatestALS])
    })
  })


  describe("ManagedStorage.findCandidateForSelection", () => {
    Async.it(
      "DevALS native: returns Some(path) when artifact exists and version matches",
      async () => {
        await withTempStorage("managed-find-devals-native-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.DevALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            ~versionString="Agda v2.8.0 Language Server (dev build)",
          )

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "DevALS WASM on desktop: returns Some(path) when WASM artifact exists and version matches",
      async () => {
        await withTempStorage("managed-find-devals-wasm-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
          ])
          let wasmFile = NodeJs.Path.join([artifactDir, "als.wasm"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.DevALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.Wasm,
            ~versionString="Agda v2.8.0 Language Server (dev build)",
          )

          Assert.deepStrictEqual(
            result,
            Some(Connection__Download.uriToPath(VSCode.Uri.file(wasmFile))),
          )
        })
      },
    )

    Async.it(
      "LatestALS native: returns Some(path) when v6 artifact exists and version matches",
      async () => {
        await withTempStorage("managed-find-latest-native-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.LatestALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            ~versionString="Agda v2.8.0 Language Server v6",
          )

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "Malformed artifact directory: returns None",
      async () => {
        await withTempStorage("managed-find-malformed-", async (tempDir, globalStorageUri) => {
          let malformedDir = NodeJs.Path.join([tempDir, "releases", "dev", "not-a-valid-artifact"])
          let executable = NodeJs.Path.join([malformedDir, "als"])

          await NodeJs.Fs.mkdir(malformedDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(executable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.DevALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            ~versionString="Agda v2.8.0 Language Server (dev build)",
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "Missing executable: returns None when directory exists but executable does not",
      async () => {
        await withTempStorage("managed-find-missing-exe-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.DevALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            ~versionString="Agda v2.8.0 Language Server (dev build)",
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "Non-matching version: returns None when versionString does not match artifact",
      async () => {
        await withTempStorage("managed-find-ver-mismatch-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.DevALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            ~versionString="Agda v9.9.9 Language Server (dev build)",
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "Channel mismatch: returns None when artifact channel does not match requested channel",
      async () => {
        await withTempStorage("managed-find-chan-mismatch-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download__ManagedStorage.findCandidateForSelection(
            globalStorageUri,
            ~channel=Connection__Download__Channel.LatestALS,
            ~platform=Connection__Download__DownloadArtifact.Platform.MacOSArm64,
            ~versionString="Agda v2.8.0 Language Server (dev build)",
          )

          Assert.deepStrictEqual(result, None)
        })
      },
    )
  })

  describe("managedDeleteRoots", () => {
    it("should return only <globalStorage>/releases", () => {
      let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
      let roots = Connection__Download.managedDeleteRoots(globalStorageUri)
      Assert.deepStrictEqual(
        roots->Array.map(uri => VSCode.Uri.toString(uri)),
        ["file:///tmp/test-storage/releases"],
      )
    })

    it("should not include legacy flat dirs (latest-als, dev-als, dev-wasm-als)", () => {
      let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
      let roots = Connection__Download.managedDeleteRoots(globalStorageUri)
      let rootStrings = roots->Array.map(uri => VSCode.Uri.toString(uri))
      Assert.deepStrictEqual(
        rootStrings->Array.some(s => String.includes(s, "latest-als")),
        false,
      )
      Assert.deepStrictEqual(
        rootStrings->Array.some(s => String.includes(s, "dev-als")),
        false,
      )
      Assert.deepStrictEqual(
        rootStrings->Array.some(s => String.includes(s, "dev-wasm-als")),
        false,
      )
    })

    it("should not include sibling or migration globalStorage roots", () => {
      let globalStorageUri = VSCode.Uri.file(
        "/tmp/Library/Application Support/Code/User/globalStorage/banacorn.agda-mode",
      )
      let roots = Connection__Download.managedDeleteRoots(globalStorageUri)
      Assert.deepStrictEqual(Array.length(roots), 1)
      Assert.deepStrictEqual(
        roots->Array.map(uri => VSCode.Uri.toString(uri)),
        [
          "file:///tmp/Library/Application%20Support/Code/User/globalStorage/banacorn.agda-mode/releases",
        ],
      )
    })
  })
})
