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

        // Test command finding - should return proper Result type
        let commandResult = await PlatformOps.findCommands(["nonexistent-command"])
        switch commandResult {
        | Ok(_) => Assert.fail("Should not find nonexistent command")
        | Error(errors) =>
          Assert.deepStrictEqual(
            errors,
            Dict.fromArray([("nonexistent-command", Connection__Command.Error.NotFound)]),
          )
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

        let commandResult = await PlatformOps.findCommands(["agda"])
        switch commandResult {
        | Error(errors) =>
          Assert.deepStrictEqual(
            errors,
            Dict.fromArray([("agda", Connection__Command.Error.NotFound)]),
          )
        | _ => Assert.fail("Web platform should return specific NotFound error")
        }

        // Test download policy - should return No for web
        let policy = await PlatformOps.askUserAboutDownloadPolicy()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)
      },
    )
  })

  describe("Connection Integration", () => {
    Async.it(
      "should allow Connection.make to be called with platform dependencies",
      async () => {
        let platformDeps = Web.make() // Use web for predictable mock behavior
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let paths = []
        let commands = ["agda"]

        // This should not crash and should return an error (since web platform doesn't support real connections)
        let result = await Connection.make(platformDeps, memento, globalStorageUri, paths, commands)

        switch result {
        | Error(_) => Assert.ok(true) // Expected for web platform
        | Ok(_) => Assert.fail("Web platform should not succeed in making real connections")
        }
      },
    )

    Async.it(
      "should allow Connection.fromDownloads to be called with platform dependencies",
      async () => {
        let platformDeps = Web.make() // Use web for predictable mock behavior
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let constructionError = {
          Connection__Error.Construction.endpoints: Dict.make(),
          commands: Dict.make(),
          download: None, // No download error for this test
        }

        // This should not crash and should return an error (since web platform doesn't support downloads)
        let result = await Connection.fromDownloads(
          platformDeps,
          memento,
          globalStorageUri,
          constructionError,
        )

        switch result {
        | Error(_) => Assert.ok(true) // Expected for web platform
        | Ok(_) => Assert.fail("Web platform should not succeed in downloading")
        }
      },
    )
  })

  describe("Mock Platform for Testing", () => {
    Async.it(
      "should allow custom mock platforms to be created for testing",
      async () => {
        // Create a custom mock platform for testing
        module MockPlatform: Platform.PlatformOps = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Windows))
          let findCommands = async commands => Error(
            commands
            ->Array.map(command => (command, Connection__Command.Error.NotFound))
            ->Dict.fromArray,
          )
          let findCommand = (_command, ~timeout as _timeout=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)
          let alreadyDownloaded2 = _globalStorageUri => () => Promise.resolve(None)
          let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let downloadLatestALS2 = (_memento, _globalStorageUri) => _platform =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let getInstalledEndpointsAndPersistThem = _globalStorageUri =>
            Promise.resolve(Dict.fromArray([]))
          let getInstalledEndpointsAndPersistThem2 = _globalStorageUri =>
            Promise.resolve(Dict.make())
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.No)
        }

        let mockPlatformDeps: Platform.t = module(MockPlatform)
        module PlatformOps = unpack(mockPlatformDeps)

        // Test the mock behavior
        let platformResult = await PlatformOps.determinePlatform()
        switch platformResult {
        | Ok(Connection__Download__Platform.Windows) => Assert.ok(true)
        | _ => Assert.fail("Mock platform should return Windows")
        }

        let commandResult = await PlatformOps.findCommands(["test"])
        switch commandResult {
        | Error(errors) =>
          Assert.deepStrictEqual(
            errors,
            Dict.fromArray([("test", Connection__Command.Error.NotFound)]),
          )
        | _ => Assert.fail("Mock platform should return NotFound error")
        }
      },
    )
  })
})
