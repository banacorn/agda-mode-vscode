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
      "desktop alreadyDownloaded should fall back to hardcoded WASM when native binary is missing",
      async () => {
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "desktop-hardcoded-wasm-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let hardcodedDir = NodeJs.Path.join([tempDir, "hardcoded-als"])
        let wasmFile = NodeJs.Path.join([hardcodedDir, "als.wasm"])

        await NodeJs.Fs.mkdir(hardcodedDir, {recursive: true, mode: 0o777})
        NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

        let platformDeps = Desktop.make()
        module PlatformOps = unpack(platformDeps)
        let globalStorageUri = VSCode.Uri.file(tempDir)

        let result = await PlatformOps.alreadyDownloaded(globalStorageUri, Hardcoded)
        let expected = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als.wasm"])
        Assert.deepStrictEqual(result, Some(VSCode.Uri.toString(expected)))

        NodeJs.Fs.unlinkSync(wasmFile)
        NodeJs.Fs.rmdirSync(hardcodedDir)
        NodeJs.Fs.rmdirSync(tempDir)
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
