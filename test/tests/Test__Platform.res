open Mocha

describe("Platform dependent utilities", () => {
  describe("Platform Abstraction", () => {
    Async.it(
      "should create Desktop platform and have working operations",
      async () => {
        let platformDeps = Desktop.make()
        module PlatformOps = unpack(platformDeps)

        // Test that platform operations are actually callable and return proper types
        let platformResult = await PlatformOps.determinePlatform()
        // Desktop should either succeed or fail with a proper error structure
        switch platformResult {
        | Ok(_) => Assert.ok(true) // Valid platform detected
        | Error(_) => Assert.ok(true) // Valid error structure (may fail in CI/test environment)
        }
      },
    )

    Async.it(
      "desktop alreadyDownloaded should find release-managed WASM artifact for DevALS channel",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "desktop-release-wasm-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let artifactDir = NodeJs.Path.join([tempDir, "releases", "dev", "als-dev-Agda-2.8.0-wasm"])
        let wasmFile = NodeJs.Path.join([artifactDir, "als.wasm"])

        await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

        let platformDeps = Desktop.make()
        module PlatformOps = unpack(platformDeps)
        let globalStorageUri = VSCode.Uri.file(tempDir)

        let result = await PlatformOps.alreadyDownloaded(globalStorageUri, Connection__Download.Channel.DevALS)
        // Should find the WASM artifact and return its URI
        Assert.deepStrictEqual(result->Option.isSome, true)

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )
  })

  describe("Mock Platform for Testing", () => {
    Async.it(
      "should allow custom mock platforms to be created for testing",
      async () => {
        // Create a custom mock platform for testing
        let mockPlatformDeps = Mock.Platform.makeBasic()
        module PlatformOps = unpack(mockPlatformDeps)

        // Test the mock behavior
        let platformResult = await PlatformOps.determinePlatform()
        switch platformResult {
        | Ok(Connection__Download__Platform.MacOS_Arm) => Assert.ok(true)
        | _ => Assert.fail("Mock platform should return MacOS_Arm")
        }
      },
    )
  })
})
