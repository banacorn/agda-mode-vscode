open Mocha

module GitHub = Connection__Download__GitHub

let logReleases = %raw(`
  function(releases) {
    console.log("All releases:", JSON.stringify(releases, null, 2));
  }
`)

let logAssets = %raw(`
  function(assets) {
    console.log("Dev release assets:", JSON.stringify(assets, null, 2));
  }
`)

let logFetchSpec = %raw(`
  function(fetchSpec) {
    console.log("SUCCESS - fetchSpec:", JSON.stringify(fetchSpec, null, 2));
  }
`)

let logError = %raw(`
  function(error) {
    console.log("ERROR:", JSON.stringify(error, null, 2));
  }
`)

describe("Connection DevALS", () => {
  This.timeout(10000)

  describe("makeAgdaLanguageServerRepo", () => {
    it(
      "should create repo config targeting banacorn/agda-language-server",
      () => {
        let mockMemento = Memento.make(None)
        let mockGlobalStorageUri = VSCode.Uri.file("/mock/storage")

        let repo = Connection__DevALS.makeAgdaLanguageServerRepo(mockMemento, mockGlobalStorageUri)

        Assert.deepStrictEqual(repo.username, "banacorn")
        Assert.deepStrictEqual(repo.repository, "agda-language-server")
        Assert.deepStrictEqual(repo.userAgent, "banacorn/agda-mode-vscode")
        Assert.deepStrictEqual(repo.cacheInvalidateExpirationSecs, 86400)
      },
    )
  })

  describe("chooseAssetByPlatform", () => {
    let mockRelease = {
      Connection__Download__GitHub.Release.url: "https://api.github.com/repos/banacorn/agda-language-server/releases/123456",
      assets_url: "https://api.github.com/repos/banacorn/agda-language-server/releases/123456/assets",
      upload_url: "https://uploads.github.com/repos/banacorn/agda-language-server/releases/123456/assets{?name,label}",
      html_url: "https://github.com/banacorn/agda-language-server/releases/tag/dev",
      id: 123456,
      node_id: "RE_mock_node_id",
      tag_name: "dev",
      target_commitish: "main",
      name: "dev",
      draft: false,
      prerelease: true,
      created_at: "2024-01-01T00:00:00Z",
      published_at: "2024-01-01T00:00:00Z",
      assets: [
        {
          Connection__Download__GitHub.Asset.url: "https://example.com/darwin-arm64.zip",
          id: 123456,
          node_id: "mock_node_id_1",
          name: "als-dev-Agda-2.8.0-macos-arm64.zip",
          label: "",
          content_type: "application/zip",
          state: "uploaded",
          size: 1000000,
          created_at: "2024-01-01T00:00:00Z",
          updated_at: "2024-01-01T00:00:00Z",
          browser_download_url: "https://example.com/darwin-arm64.zip",
        },
        {
          Connection__Download__GitHub.Asset.url: "https://example.com/linux-x64.zip",
          id: 123457,
          node_id: "mock_node_id_2",
          name: "als-dev-Agda-2.8.0-ubuntu.zip",
          label: "",
          content_type: "application/zip",
          state: "uploaded",
          size: 1000000,
          created_at: "2024-01-01T00:00:00Z",
          updated_at: "2024-01-01T00:00:00Z",
          browser_download_url: "https://example.com/linux-x64.zip",
        },
        {
          Connection__Download__GitHub.Asset.url: "https://example.com/win32-x64.zip",
          id: 123458,
          node_id: "mock_node_id_3",
          name: "als-dev-Agda-2.8.0-windows.zip",
          label: "",
          content_type: "application/zip",
          state: "uploaded",
          size: 1000000,
          created_at: "2024-01-01T00:00:00Z",
          updated_at: "2024-01-01T00:00:00Z",
          browser_download_url: "https://example.com/win32-x64.zip",
        },
      ],
      tarball_url: "https://api.github.com/repos/banacorn/agda-language-server/tarball/dev",
      zipball_url: "https://api.github.com/repos/banacorn/agda-language-server/zipball/dev",
      body: Some("Development release"),
    }

    Async.it(
      "should filter assets by darwin-arm64 platform",
      async () => {
        let platform: Connection__Download__Platform.t = MacOS_Arm

        let assets = await Connection__DevALS.chooseAssetByPlatform(mockRelease, platform)

        Assert.deepStrictEqual(Array.length(assets), 1)
        Assert.deepStrictEqual(
          (assets[0]->Option.getExn).name,
          "als-dev-Agda-2.8.0-macos-arm64.zip",
        )
      },
    )

    Async.it(
      "should filter assets by linux-x64 platform",
      async () => {
        let platform: Connection__Download__Platform.t = Ubuntu

        let assets = await Connection__DevALS.chooseAssetByPlatform(mockRelease, platform)

        Assert.deepStrictEqual(Array.length(assets), 1)
        Assert.deepStrictEqual((assets[0]->Option.getExn).name, "als-dev-Agda-2.8.0-ubuntu.zip")
      },
    )

    Async.it(
      "should filter assets by win32-x64 platform",
      async () => {
        let platform: Connection__Download__Platform.t = Windows

        let assets = await Connection__DevALS.chooseAssetByPlatform(mockRelease, platform)

        Assert.deepStrictEqual(Array.length(assets), 1)
        Assert.deepStrictEqual((assets[0]->Option.getExn).name, "als-dev-Agda-2.8.0-windows.zip")
      },
    )

    Async.it(
      "should return empty array when no matching assets",
      async () => {
        // Create a release with no matching assets for MacOS_Arm
        let releaseWithDifferentAssets = {
          Connection__Download__GitHub.Release.url: "https://api.github.com/repos/banacorn/agda-language-server/releases/123457",
          assets_url: "https://api.github.com/repos/banacorn/agda-language-server/releases/123457/assets",
          upload_url: "https://uploads.github.com/repos/banacorn/agda-language-server/releases/123457/assets{?name,label}",
          html_url: "https://github.com/banacorn/agda-language-server/releases/tag/dev",
          id: 123457,
          node_id: "RE_mock_node_id_2",
          tag_name: "dev",
          target_commitish: "main",
          name: "dev",
          draft: false,
          prerelease: true,
          created_at: "2024-01-01T00:00:00Z",
          published_at: "2024-01-01T00:00:00Z",
          assets: [
            {
              Connection__Download__GitHub.Asset.url: "https://example.com/other.zip",
              id: 123456,
              node_id: "mock_node_id_1",
              name: "other-file-unsupported.zip", // This won't match any platform
              label: "",
              content_type: "application/zip",
              state: "uploaded",
              size: 1000000,
              created_at: "2024-01-01T00:00:00Z",
              updated_at: "2024-01-01T00:00:00Z",
              browser_download_url: "https://example.com/other.zip",
            },
          ],
          tarball_url: "https://api.github.com/repos/banacorn/agda-language-server/tarball/dev",
          zipball_url: "https://api.github.com/repos/banacorn/agda-language-server/zipball/dev",
          body: Some("Development release without matching assets"),
        }

        let assets = await Connection__DevALS.chooseAssetByPlatform(
          releaseWithDifferentAssets,
          MacOS_Arm,
        )

        Assert.deepStrictEqual(Array.length(assets), 0)
      },
    )
  })

  describe("alreadyDownloaded", () => {
    Async.it(
      "should return None when dev-als directory doesn't exist",
      async () => {
        let nonExistentDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-nonexistent-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])

        let globalStorageUri = VSCode.Uri.file(nonExistentDir)
        let result = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

        Assert.deepStrictEqual(result, None)
      },
    )

    Async.it(
      "should return None when dev-als directory exists but als executable doesn't",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-noals-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devAlsDir = NodeJs.Path.join([tempDir, "dev-als"])

        // Create dev-als directory but not the als executable
        await NodeJs.Fs.mkdir(devAlsDir, {recursive: true, mode: 0o777})

        let globalStorageUri = VSCode.Uri.file(tempDir)
        let result = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(devAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return Some(path) when dev-als/als exists",
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
        let result = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

        Assert.deepStrictEqual(result, Some(alsExecutable))

        // Cleanup
        NodeJs.Fs.unlinkSync(alsExecutable)
        NodeJs.Fs.rmdirSync(devAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should handle multiple calls consistently",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "devals-test-multiple-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let devAlsDir = NodeJs.Path.join([tempDir, "dev-als"])
        let alsExecutable = NodeJs.Path.join([devAlsDir, "als"])

        // Create directory structure and als executable
        await NodeJs.Fs.mkdir(devAlsDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

        let globalStorageUri = VSCode.Uri.file(tempDir)

        // Call multiple times
        let result1 = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()
        let result2 = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()
        let result3 = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

        // All should return the same result
        Assert.deepStrictEqual(result1, Some(alsExecutable))
        Assert.deepStrictEqual(result2, Some(alsExecutable))
        Assert.deepStrictEqual(result3, Some(alsExecutable))

        // Cleanup
        NodeJs.Fs.unlinkSync(alsExecutable)
        NodeJs.Fs.rmdirSync(devAlsDir)
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("getALSReleaseManifest integration", () => {
    Async.it(
      "should fetch ALS release manifest from GitHub API",
      async () => {
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        let releaseResult = await Connection__DevALS.getALSReleaseManifest(
          memento,
          globalStorageUri,
        )

        switch releaseResult {
        | Error(e) =>
          Assert.fail("Failed to fetch releases: " ++ Connection__Download.Error.toString(e))
        | Ok(releases) =>
          logReleases(releases)->ignore

          // Verify we got at least one release
          Assert.ok(Array.length(releases) > 0)

          // Verify the dev release exists
          let devRelease = releases->Array.find(release => release.tag_name == "dev")
          switch devRelease {
          | None => Assert.fail("No dev release found in manifest")
          | Some(devRelease) =>
            // Verify dev release structure
            Assert.deepStrictEqual(devRelease.tag_name, "dev")
            Assert.deepStrictEqual(devRelease.name, "Development Release (dev)")
            Assert.ok(devRelease.prerelease == true)
            Assert.ok(Array.length(devRelease.assets) > 0)

            // Verify assets contain expected platforms
            let assetNames = devRelease.assets->Array.map(asset => asset.name)
            Assert.ok(assetNames->Array.some(name => name->String.includes("ubuntu")))
            Assert.ok(assetNames->Array.some(name => name->String.includes("windows")))
            Assert.ok(assetNames->Array.some(name => name->String.includes("macos")))
          }
        }
      },
    )
  })

  describe("getFetchSpec integration", () => {
    Async.it(
      "should fetch dev release spec from GitHub API",
      async () => {
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        // Try with Ubuntu platform since we know it should exist
        let platform = Connection__Download__Platform.Ubuntu
        let result = await Connection__DevALS.getFetchSpec(memento, globalStorageUri, platform)

        switch result {
        | Ok(fetchSpec) =>
          logFetchSpec(fetchSpec)->ignore
          // Verify we got the dev release
          Assert.deepStrictEqual(fetchSpec.release.tag_name, "dev")
          // Verify the saveAsFileName is correct
          Assert.deepStrictEqual(fetchSpec.saveAsFileName, "dev-als")
          // Verify it's a dev asset
          Assert.ok(fetchSpec.asset.name->String.includes("als-dev-Agda"))
        | Error(error) =>
          logError(error)->ignore
          Assert.fail(
            "Expected success but got error: " ++ Connection__Download.Error.toString(error),
          )
        }
      },
    )
  })
})
