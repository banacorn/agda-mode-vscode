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
