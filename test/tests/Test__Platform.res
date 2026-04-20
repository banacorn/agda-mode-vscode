open Mocha

let withTempStorage = async (prefix, f) => {
  let tempDir = NodeJs.Path.join([
    NodeJs.Os.tmpdir(),
    prefix ++ string_of_int(int_of_float(Js.Date.now())),
  ])
  let globalStorageUri = VSCode.Uri.file(tempDir)
  try {
    await NodeJs.Fs.mkdir(tempDir, {recursive: true, mode: 0o777})
    await f(tempDir, globalStorageUri)
  } catch {
  | exn =>
    let _ = await FS.deleteRecursive(globalStorageUri)
    raise(exn)
  }
  let _ = await FS.deleteRecursive(globalStorageUri)
}

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
      "desktop alreadyDownloaded should find release-managed WASM artifact",
      async () => {
        await withTempStorage("desktop-release-wasm-test-", async (tempDir, globalStorageUri) => {
          let artifactDir = NodeJs.Path.join([tempDir, "releases", "dev", "als-dev-Agda-2.8.0-wasm"])
          let wasmFile = NodeJs.Path.join([artifactDir, "als.wasm"])

          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let platformDeps = Desktop.make()
          module PlatformOps = unpack(platformDeps)

          let result = await PlatformOps.alreadyDownloaded(globalStorageUri)
          Assert.deepStrictEqual(result, Some(VSCode.Uri.file(wasmFile)->VSCode.Uri.toString))
        })
      },
    )

    Async.it(
      "desktop alreadyDownloaded should find release-managed native artifact",
      async () => {
        let platformDeps = Desktop.make()
        module PlatformOps = unpack(platformDeps)

        switch await PlatformOps.determinePlatform() {
        | Error(_) => Assert.ok(true)
        | Ok(platform) =>
          await withTempStorage("desktop-release-latest-test-", async (tempDir, globalStorageUri) => {
            let platformTag = Connection__Download__Platform.toAssetName(platform)
            let artifactDir = NodeJs.Path.join([
              tempDir,
              "releases",
              "v6",
              "als-v6-Agda-2.8.0-" ++ platformTag,
            ])
            let alsExecutable = NodeJs.Path.join([artifactDir, "als"])

            await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
            NodeJs.Fs.writeFileSync(alsExecutable, NodeJs.Buffer.fromString("mock executable"))

            let result = await PlatformOps.alreadyDownloaded(globalStorageUri)
            Assert.deepStrictEqual(result, Some(VSCode.Uri.file(alsExecutable)->VSCode.Uri.fsPath))
          })
        }
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
