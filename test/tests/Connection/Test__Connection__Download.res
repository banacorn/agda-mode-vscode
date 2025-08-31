open Mocha

module GitHub = Connection__Download__GitHub

describe("Download", () => {
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

  describe("Integration Tests", () => {
    describe(
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
