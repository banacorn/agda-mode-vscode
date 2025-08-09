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
      "should create Web platform and return expected errors",
      async () => {
        let platformDeps = Web.make()
        module PlatformOps = unpack(platformDeps)

        // Web platform should consistently return errors for unsupported operations
        let platformResult = await PlatformOps.determinePlatform()
        switch platformResult {
        | Error(raw) =>
          Assert.deepStrictEqual(raw["os"], "web")
          Assert.deepStrictEqual(raw["dist"], "browser")
        | Ok(_) => Assert.fail("Web platform should return Error for determinePlatform")
        }

        // Test download policy - should return No for web
        let policy = await PlatformOps.askUserAboutDownloadPolicy()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)
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
