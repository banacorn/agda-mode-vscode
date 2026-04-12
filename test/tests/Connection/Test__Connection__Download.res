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

describe("Download", () => {
  This.timeout(10000)

  describe("DownloadArtifact", () => {
    let expectParsed = (raw, releaseTag, agdaVersion, platform) =>
      switch Connection__Download.DownloadArtifact.parseName(raw) {
      | Some(parsed) =>
        Assert.deepStrictEqual(parsed.releaseTag, releaseTag)
        Assert.deepStrictEqual(parsed.agdaVersion, agdaVersion)
        Assert.deepStrictEqual(parsed.platform, platform)
      | None => Assert.fail("expected DownloadArtifact to parse: " ++ raw)
      }

    let expectRejected = raw =>
      Assert.deepStrictEqual(Connection__Download.DownloadArtifact.parseName(raw), None)

    it("should parse canonical WASM artifact filenames with typed platform", () => {
      expectParsed(
        "als-dev-Agda-2.8.0-wasm.wasm",
        "dev",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.Wasm,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-wasm.wasm",
        "v6",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.Wasm,
      )
    })

    it("should parse canonical native artifact filenames with typed platform", () => {
      expectParsed(
        "als-v6-Agda-2.8.0-ubuntu.zip",
        "v6",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.Ubuntu,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-macos-arm64.zip",
        "v6",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.MacOSArm64,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-macos-x64.zip",
        "v6",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.MacOSX64,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-windows.zip",
        "v6",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.Windows,
      )
    })

    it("should parse canonical artifact directory names without extension", () => {
      expectParsed(
        "als-dev-Agda-2.8.0-wasm",
        "dev",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.Wasm,
      )
      expectParsed(
        "als-v6-Agda-2.8.0-macos-arm64",
        "v6",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.MacOSArm64,
      )
    })

    it("should allow dotted release tags", () => {
      expectParsed(
        "als-v6.1-Agda-2.8.0-wasm.wasm",
        "v6.1",
        "2.8.0",
        Connection__Download.DownloadArtifact.Platform.Wasm,
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
      let artifact = switch Connection__Download.DownloadArtifact.parseName(
        "als-v6.1-Agda-2.8.0-macos-arm64.zip",
      ) {
      | Some(artifact) => artifact
      | None => raise(Failure("expected artifact to parse"))
      }

      Assert.deepStrictEqual(
        Connection__Download.DownloadArtifact.cacheName(artifact),
        "als-v6.1-Agda-2.8.0-macos-arm64",
      )
    })

    it("should construct release-based managed executable URIs", () => {
      let globalStorageUri = VSCode.Uri.file("/tmp/agda-mode-global-storage")
      let wasmArtifact = switch Connection__Download.DownloadArtifact.parseName(
        "als-v6-Agda-2.8.0-wasm.wasm",
      ) {
      | Some(artifact) => artifact
      | None => raise(Failure("expected artifact to parse"))
      }
      let nativeArtifact = switch Connection__Download.DownloadArtifact.parseName(
        "als-v6-Agda-2.8.0-macos-arm64.zip",
      ) {
      | Some(artifact) => artifact
      | None => raise(Failure("expected artifact to parse"))
      }

      Assert.deepStrictEqual(
        VSCode.Uri.fsPath(
          Connection__Download.DownloadArtifact.managedExecutableUri(
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
          Connection__Download.DownloadArtifact.managedExecutableUri(
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
        Connection__Download.Source.toVersionString(
          Connection__Download.Source.FromGitHub(LatestALS, descriptor),
        ),
        "Agda v2.8.0 Language Server v6",
      )
    })
  })

  describe("expectedPathForSource with release artifacts", () => {
    let makeSource = (
      ~channel: Connection__Download.Channel.t,
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
        | Hardcoded => "hardcoded-als"
        },
      }

      Connection__Download.Source.FromGitHub(channel, descriptor)
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

  describe("alreadyDownloaded", () => {
    Async.it(
      "should return Some(path) when DevALS native artifact exists in release-managed storage",
      async () => {
        await withTempStorage("devals-release-native-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "should return Some(path) when DevALS WASM artifact exists in release-managed storage",
      async () => {
        await withTempStorage("devals-release-wasm-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
          ])
          let wasmFile = NodeJs.Path.join([artifactDir, "als.wasm"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(wasmFile)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "should ignore malformed DevALS artifacts in release-managed storage",
      async () => {
        await withTempStorage("devals-release-malformed-", async (tempDir, globalStorageUri) => {
          let malformedDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-freebsd",
          ])
          let malformedExecutable = NodeJs.Path.join([malformedDir, "als"])

          await NodeJs.Fs.mkdir(malformedDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(malformedExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "should discover release-managed artifacts globally without limiting to the selected channel",
      async () => {
        await withTempStorage("devals-release-global-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "v6",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
        })
      },
    )

    Async.it(
      "should ignore artifacts whose release directory does not match the artifact release tag",
      async () => {
        await withTempStorage("devals-release-mismatch-", async (tempDir, globalStorageUri) => {
          let mismatchedDir = NodeJs.Path.join([
            tempDir,
            "releases",
            "dev",
            "als-v6-Agda-2.8.0-macos-arm64",
          ])
          let mismatchedExecutable = NodeJs.Path.join([mismatchedDir, "als"])

          await NodeJs.Fs.mkdir(mismatchedDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(mismatchedExecutable, NodeJs.Buffer.fromString("mock executable"))

          let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

          Assert.deepStrictEqual(result, None)
        })
      },
    )

    Async.it(
      "should return None when dev ALS not downloaded",
      async () => {
        let nonExistentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-nonexistent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        let globalStorageUri = VSCode.Uri.file(nonExistentDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

        Assert.deepStrictEqual(result, None)
      },
    )

    Async.it(
      "should return Some(path) when dev ALS executable exists",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-exists-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devAlsDir = NodeJs.Path.join([tempDir, "dev-als"])
        let alsExecutable = NodeJs.Path.join([devAlsDir, "als"])

        // Create directory structure and als executable
        await NodeJs.Fs.mkdir(devAlsDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

        Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))

        // Cleanup
        NodeJs.Fs.unlinkSync(alsExecutable)
        NodeJs.Fs.rmdirSync(devAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return None when dev-als directory exists but als executable missing",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-noals-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devAlsDir = NodeJs.Path.join([tempDir, "dev-als"])

        // Create dev-als directory but not the als executable
        await NodeJs.Fs.mkdir(devAlsDir, {recursive: true, mode: 0o777})

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(devAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return None when global storage exists but no dev-als directory",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-nostorage-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        // Create global storage directory but no dev-als subdirectory
        await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, DevALS)

        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("alreadyDownloaded with Hardcoded", () => {
    Async.it(
      "should return Some(path) when native binary exists at hardcoded-als/als",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "hardcoded-als-native-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let hardcodedAlsDir = NodeJs.Path.join([tempDir, "hardcoded-als"])
        let alsExecutable = NodeJs.Path.join([hardcodedAlsDir, "als"])

        // Create directory structure and als executable
        await NodeJs.Fs.mkdir(hardcodedAlsDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, Hardcoded)

        Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))

        // Cleanup
        NodeJs.Fs.unlinkSync(alsExecutable)
        NodeJs.Fs.rmdirSync(hardcodedAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Some(uri) when WASM exists at hardcoded-als/als.wasm",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "hardcoded-als-wasm-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let hardcodedAlsDir = NodeJs.Path.join([tempDir, "hardcoded-als"])
        let wasmFile = NodeJs.Path.join([hardcodedAlsDir, "als.wasm"])

        // Create directory structure and als.wasm file
        await NodeJs.Fs.mkdir(hardcodedAlsDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, Hardcoded)

        let wasmUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als.wasm"])
        Assert.deepStrictEqual(result, Some(VSCode.Uri.toString(wasmUri)))

        // Cleanup
        NodeJs.Fs.unlinkSync(wasmFile)
        NodeJs.Fs.rmdirSync(hardcodedAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return None when nothing exists at hardcoded-als",
      async () => {
        let nonExistentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "hardcoded-als-test-nonexistent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        let globalStorageUri = VSCode.Uri.file(nonExistentDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, Hardcoded)

        Assert.deepStrictEqual(result, None)
      },
    )

    Async.it(
      "should prefer native binary over WASM when both exist",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "hardcoded-als-both-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let hardcodedAlsDir = NodeJs.Path.join([tempDir, "hardcoded-als"])
        let alsExecutable = NodeJs.Path.join([hardcodedAlsDir, "als"])
        let wasmFile = NodeJs.Path.join([hardcodedAlsDir, "als.wasm"])

        // Create both native and WASM files
        await NodeJs.Fs.mkdir(hardcodedAlsDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))
        NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__Download.alreadyDownloaded(globalStorageUri, Hardcoded)

        // Should prefer native binary
        Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))

        // Cleanup
        NodeJs.Fs.unlinkSync(alsExecutable)
        NodeJs.Fs.unlinkSync(wasmFile)
        NodeJs.Fs.rmdirSync(hardcodedAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
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
      release, asset, saveAsFileName: "hardcoded-als",
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
        let binaryDir = NodeJs.Path.join([tempDir, "hardcoded-als"])
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
        let binaryDir = NodeJs.Path.join([tempDir, "hardcoded-als"])
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

        let expectedPath = NodeJs.Path.join([tempDir, "hardcoded-als", "als"])
        Assert.deepStrictEqual(result, Error(GitHub.Error.BinaryMissingAfterExtraction(expectedPath)))
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
      { release, asset, saveAsFileName: "hardcoded-als" }
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
          let source = Connection__Download.Source.FromGitHub(
            Connection__Download.Channel.Hardcoded,
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
          let source = Connection__Download.Source.FromURL(
            Connection__Download.Channel.Hardcoded,
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
              let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
              let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(_ =>
                Ok(Connection__Download.Source.FromURL(
                  Connection__Download.Channel.Hardcoded,
                  "mock://trace-test",
                  "hardcoded-als",
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

          await State__SwitchVersion.Handler.handleDownload(
            state, platform,
            State__SwitchVersion.Download.Native,
            false, "ALS vTest",
            ~channel=Connection__Download.Channel.Hardcoded,
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
            let repo = Connection__LatestALS.makeRepo(globalStorageUri)
            let memento = Memento.make(None)
            let releaseResult = await Connection__Download.getReleaseManifestFromGitHub(
              memento,
              repo,
              ~useCache=false,
            )

            switch releaseResult {
            | Error(e) =>
              Assert.fail("Failed to fetch releases: " ++ Connection__Download.Error.toString(e))
            | Ok(releases) =>
              // Verify we got releases from GitHub API
              Assert.ok(Array.length(releases) > 0)
            }
          },
        )
      },
    )
  })
})
