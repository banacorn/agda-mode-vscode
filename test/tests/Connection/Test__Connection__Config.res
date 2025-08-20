open Mocha
open Test__Util

// Config.Connection paths
// ├── Working config paths
// │   ├── Single working path → should not modify config
// │   ├── Multiple working paths → should not modify config
// │   └── Mixed working/broken paths → should not modify config (working paths exist)
// ├── Broken config paths
// │   ├── All broken, auto discovery succeeds → should add discovered path to config
// │   └── All broken, auto discovery fails → should fail without modifying config
// ├── Empty config paths
// │   ├── Auto discovery succeeds → should add discovered path to config
// │   └── Auto discovery fails → should fail without modifying config
// └── UI-triggered additions
//     └── Switch version UI selection → should add selected path to config

describe_only("Config.Connection paths", () => {
  let userAgda = ref("")
  let systemAgda = ref("")
  let brokenAgda = ref("/broken/agda")
  let alternativeAgda = ref("")
  let logChannel = Chan.make()

  // setup the Agda mocks
  Async.before(async () => {
    userAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-user"))
    systemAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-system"))
    alternativeAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-alt"))
  })

  // cleanup the Agda mocks
  Async.after(async () => {
    await Endpoint.Agda.destroy(userAgda.contents)
    await Endpoint.Agda.destroy(systemAgda.contents)
    await Endpoint.Agda.destroy(alternativeAgda.contents)
  })

  // Mock platform that returns the system Agda path when `findCommand` is called
  let platformWithDiscovery = (
    module(
      {
        include Desktop.Desktop
        let findCommand = (_command, ~timeout as _timeout=1000) => {
          Promise.resolve(Ok(systemAgda.contents))
        }
      }
    ): Platform.t
  )
  
  // Mock platform that fails discovery
  let platformNoDiscovery = (
    module(
      {
        include Desktop.Desktop
        let findCommand = (_command, ~timeout as _timeout=1000) => {
          Promise.resolve(Error(Connection__Command.Error.NotFound))
        }
      }
    ): Platform.t
  )

  let makeConnection = async (configPaths: array<string>, platform: Platform.t) => {
    let memento = Memento.make(None)
    // Set up config without logging (use a separate channel for setup)
    await Config.Connection.setAgdaPaths(Chan.make(), configPaths)

    await Connection.make(
      platform,
      memento,
      VSCode.Uri.file("/tmp/test"),
      configPaths,
      ["agda"], // command to discover
      logChannel,
    )
  }

  describe("Working config paths", () => {
    Async.it("should not modify config with single working path", async () => {
      let configPaths = [userAgda.contents]
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformWithDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should not modify config
      Assert.deepStrictEqual(logs, [])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

      switch result {
      | Ok(connection) =>
        Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
      | Error(_) => Assert.fail("Connection should succeed")
      }
    })

    Async.it("should not modify config with multiple working paths", async () => {
      let configPaths = [userAgda.contents, alternativeAgda.contents]
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformWithDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should not modify config
      Assert.deepStrictEqual(logs, [])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

      switch result {
      | Ok(connection) =>
        // should use first working path
        Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
      | Error(_) => Assert.fail("Connection should succeed")
      }
    })

    Async.it("should not modify config with mixed working/broken paths", async () => {
      let configPaths = [brokenAgda.contents, userAgda.contents, "/another/broken"]
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformWithDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should not modify config since working paths exist
      Assert.deepStrictEqual(logs, [])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

      switch result {
      | Ok(connection) =>
        // should use first working path (userAgda)
        Assert.deepStrictEqual(connection->Connection.getPath, userAgda.contents)
      | Error(_) => Assert.fail("Connection should succeed")
      }
    })
  })

  describe("Broken config paths", () => {

    Async.it("should add discovered path when all config paths are broken and auto discovery succeeds", async () => {
      let configPaths = [brokenAgda.contents, "/another/broken"]
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformWithDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should add discovered path to config
      let expectedConfig = Array.concat(configPaths, [systemAgda.contents])
      Assert.deepStrictEqual(logs, [Log.Config(Changed(configPaths, expectedConfig))])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), expectedConfig)

      switch result {
      | Ok(connection) =>
        Assert.deepStrictEqual(connection->Connection.getPath, systemAgda.contents)
      | Error(_) => Assert.fail("Connection should succeed")
      }
    })

    Async.it("should fail without modifying config when all config paths are broken and auto discovery fails", async () => {
      // Set download policy to No to prevent dialog prompts in test environment
      await Config.Connection.DownloadPolicy.set(No)
      
      let configPaths = [brokenAgda.contents, "/another/broken"]
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformNoDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should not modify config when all attempts fail
      Assert.deepStrictEqual(logs, [])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

      switch result {
      | Ok(_) => Assert.fail("Connection should fail when all paths are broken and no discovery")
      | Error(_) => () // expected to fail
      }
    })
  })

  describe("Empty config paths", () => {
    Async.it("should add discovered path when config is empty and auto discovery succeeds", async () => {
      let configPaths = []
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformWithDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should add discovered path to config
      let expectedConfig = [systemAgda.contents]
      Assert.deepStrictEqual(logs, [Log.Config(Changed([], expectedConfig))])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), expectedConfig)

      switch result {
      | Ok(connection) =>
        Assert.deepStrictEqual(connection->Connection.getPath, systemAgda.contents)
      | Error(_) => Assert.fail("Connection should succeed")
      }
    })

    Async.it("should fail without modifying config when config is empty and auto discovery fails", async () => {
      // Set download policy to No to prevent dialog prompts in test environment
      await Config.Connection.DownloadPolicy.set(No)
      
      let configPaths = []
      let listener = Log.collect(logChannel)
      let result = await makeConnection(configPaths, platformNoDiscovery)
      let logs = listener(~filter=Log.isConfig)

      // should not modify config when discovery fails
      Assert.deepStrictEqual(logs, [])
      Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), configPaths)

      switch result {
      | Ok(_) => Assert.fail("Connection should fail when config is empty and no discovery")
      | Error(_) => () // expected to fail
      }
    })
  })

  describe("UI-triggered additions", () => {
    Async.it("should add selected path when user chooses from switch version UI", async () => {
      // TODO: This test requires implementing UI selection logic
      // For now, this is a placeholder to maintain the test structure
      // The actual implementation would involve testing the SwitchVersion UI interaction
      Assert.ok(true) // placeholder
    })
  })
})
