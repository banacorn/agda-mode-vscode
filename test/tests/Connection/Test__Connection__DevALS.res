open Mocha

module GitHub = Connection__Download__GitHub

describe("Connection DevALS", () => {
  This.timeout(10000)

  describe("alreadyDownloaded", () => {
    Async.it(
      "should return None when dev ALS not downloaded",
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
        let result = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

        Assert.deepStrictEqual(result, Some(alsExecutable))

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
        let result = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

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
        let result = await Connection__DevALS.alreadyDownloaded(globalStorageUri)()

        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("GitHub API integration", () => {
    Async.it(
      "should fetch dev release from GitHub API",
      async () => {
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        let releaseResult = await Connection__DevALS.getALSReleaseManifestWithoutCache(
          globalStorageUri,
        )

        switch releaseResult {
        | Error(e) =>
          Assert.fail("Failed to fetch releases: " ++ Connection__Download.Error.toString(e))
        | Ok(releases) =>
          // Should find dev release
          let devRelease = releases->Array.find(release => release.tag_name == "dev")
          switch devRelease {
          | None => Assert.fail("Dev release not found")
          | Some(release) => Assert.deepStrictEqual(release.tag_name, "dev")
          }
        }
      },
    )
  })

  describe("getDownloadDescriptor integration", () => {
    Async.it(
      "should fetch dev release spec from GitHub API",
      async () => {
        let globalStorageUri = VSCode.Uri.file("/tmp/test-dev-als")

        // Try with Ubuntu platform since we know it should exist
        let platform = Connection__Download__Platform.Ubuntu
        let result = await Connection__DevALS.getDownloadDescriptor(globalStorageUri, platform)

        switch result {
        | Ok(downloadDescriptor) =>
          // Verify we got the dev release
          Assert.deepStrictEqual(downloadDescriptor.release.tag_name, "dev")
          // Verify the saveAsFileName is correct
          Assert.deepStrictEqual(downloadDescriptor.saveAsFileName, "dev-als")
          // Verify it's a dev asset for Ubuntu platform
          Assert.deepStrictEqual(downloadDescriptor.asset.name, "als-dev-Agda-2.8.0-ubuntu.zip")
        | Error(error) =>
          Assert.fail(
            "Expected success but got error: " ++ Connection__Download.Error.toString(error),
          )
        }
      },
    )
  })
})
