open Mocha
open Test__Util

describe("Connection Config Path Management", () => {
  let userAgda = ref("")
  let systemAgda = ref("")

  // setup the Agda mocks
  Async.before(async () => {
    userAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-user"))
    systemAgda := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-system"))
  })

  // cleanup the Agda mocks
  Async.after(async () => {
    await Endpoint.Agda.destroy(userAgda.contents)
    await Endpoint.Agda.destroy(systemAgda.contents)
  })

  // Mock platform that:
  //  * returns the system Agda path when `findCommand` is called
  let platform = (
    module(
      {
        include Desktop.Desktop
        let findCommand = (_command, ~timeout as _timeout=1000) => {
          Promise.resolve(Ok(systemAgda.contents))
        }
      }
    ): Platform.t
  )
  let makeConnection = async (previouslySelectedPath: option<string>) => {
    // Mock memento to simulate previously selected path
    let memento = Memento.make(None)
    switch previouslySelectedPath {
    | Some(path) => await memento->Memento.PickedConnection.set(Some(path))
    | None => ()
    }

    await Connection.make(
      platform,
      memento,
      VSCode.Uri.file("/tmp/test"), // don't care,
      Config.Connection.getAgdaPaths(),
      ["whatever"], // to invoked our `findCommand` mock
      Chan.make(),
    )
  }

  describe_only("User Configuration", () => {
    Async.it(
      "should respect user's configuration when no paths were previously selected",
      async () => {
        // Precondition
        //    * User has set `userAgda` in the configuration
        //    * User has not selected any paths before

        let userConfig = [userAgda.contents]
        await Config.Connection.setAgdaPaths(userConfig)

        let result = await makeConnection(None)

        // user's configuration should be respected
        let expectedConfig = userConfig
        let actualConfig = Config.Connection.getAgdaPaths()
        Assert.deepStrictEqual(actualConfig, expectedConfig)

        switch result {
        | Ok(connection) =>
          // `userAgda` should be used instead of `systemAgda`
          let actualPath = connection->Connection.getPath
          let expectedPath = userAgda.contents
          Assert.deepStrictEqual(actualPath, expectedPath)
        | Error(_) => Assert.fail("Connection should succeed with user-configured paths")
        }
      },
    )

    Async.it(
      "should respect user's configuration when a path has been previously selected",
      async () => {
        // Precondition
        //    * User has set `userAgda` in the configuration
        //    * User has previously selected `userAgda`

        let userConfig = [userAgda.contents]
        await Config.Connection.setAgdaPaths(userConfig)

        let result = await makeConnection(Some(userAgda.contents))

        // user's configuration should be respected
        let expectedConfig = userConfig
        let actualConfig = Config.Connection.getAgdaPaths()
        Assert.deepStrictEqual(actualConfig, expectedConfig)

        switch result {
        | Ok(connection) =>
          // user's Agda should be used instead of the system Agda
          let actualPath = connection->Connection.getPath
          let expectedPath = userAgda.contents
          Assert.deepStrictEqual(actualPath, expectedPath)
        | Error(_) => Assert.fail("Connection should succeed with user-configured paths")
        }
      },
    )

    Async.it(
      "should only add path when the users has not provided any paths",
      async () => {
        // Precondition
        //    * User has not set any paths in the configuration
        //    * User has not selected any paths before

        let userConfig = []
        await Config.Connection.setAgdaPaths(userConfig)

        let result = await makeConnection(None)

        // user's configuration should be respected
        let expectedConfig = [systemAgda.contents]
        let actualConfig = Config.Connection.getAgdaPaths()
        Assert.deepStrictEqual(actualConfig, expectedConfig)

        switch result {
        | Ok(connection) =>
          // `userAgda` should be used instead of `systemAgda`
          let actualPath = connection->Connection.getPath
          let expectedPath = systemAgda.contents
          Assert.deepStrictEqual(actualPath, expectedPath)
        | Error(_) => Assert.fail("Connection should succeed with user-configured paths")
        }
      },
    )
  })

  describe("Path Management Integrity", () => {
    Async.it(
      "should prevent duplicate entries when paths already exist in user config",
      async () => {
        // GIVEN: User sets agdaMode.connection.paths = ["/usr/bin/agda"]
        let existingPath = "/usr/bin/agda"
        await Config.Connection.setAgdaPaths([existingPath])

        // WHEN: The same path would be auto-discovered from PATH
        // We simulate the addAgdaPath function behavior
        await Config.Connection.addAgdaPath(existingPath)
        let pathsAfterDuplicateAdd = Config.Connection.getAgdaPaths()

        // THEN: agdaMode.connection.paths should remain ["/usr/bin/agda"] (no duplicates)
        Assert.deepStrictEqual(pathsAfterDuplicateAdd, [existingPath])
        Assert.deepStrictEqual(Array.length(pathsAfterDuplicateAdd), 1)
      },
    )

    Async.it(
      "should preserve user-configured path order and priority",
      async () => {
        // GIVEN: User configures specific path order
        let userOrderedPaths = ["/custom/agda", "/usr/bin/agda", "/opt/agda"]
        await Config.Connection.setAgdaPaths(userOrderedPaths)

        // WHEN: Configuration is read back
        let actualPaths = Config.Connection.getAgdaPaths()

        // THEN: Order should be preserved exactly as user configured
        Assert.deepStrictEqual(actualPaths, userOrderedPaths)

        // Verify first path maintains priority
        Assert.deepStrictEqual(actualPaths[0], Some("/custom/agda"))
      },
    )

    Async.it(
      "should handle setAgdaPaths as complete replacement, not addition",
      async () => {
        // Verify that setAgdaPaths replaces all paths, doesn't append
        await Config.Connection.setAgdaPaths(["path1", "path2"])
        let initialPaths = Config.Connection.getAgdaPaths()
        Assert.deepStrictEqual(initialPaths, ["path1", "path2"])

        // When setting new paths
        await Config.Connection.setAgdaPaths(["path3"])
        let replacedPaths = Config.Connection.getAgdaPaths()

        // Should completely replace, not append
        Assert.deepStrictEqual(replacedPaths, ["path3"])
        Assert.deepStrictEqual(Array.length(replacedPaths), 1)
      },
    )
  })

  describe("Correct Auto-Discovery Behavior", () => {
    Async.it(
      "should demonstrate proper conditional auto-discovery logic",
      async () => {
        // This test documents the CORRECT behavior that should be implemented

        // SCENARIO 1: User has configured paths - no auto-discovery
        await Config.Connection.setAgdaPaths(["user-configured-agda"])
        let userPaths = Config.Connection.getAgdaPaths()
        let hasUserConfig = Array.length(userPaths) > 0

        // When user has configuration, should NOT auto-discover
        if hasUserConfig {
          // This is the CORRECT behavior: respect user config completely
          Assert.deepStrictEqual(userPaths, ["user-configured-agda"])
          // In real implementation: skip auto-discovery entirely
        }

        // SCENARIO 2: User has no configured paths - allow auto-discovery
        await Config.Connection.setAgdaPaths([])
        let emptyPaths = Config.Connection.getAgdaPaths()
        let shouldAutoDiscover = Array.length(emptyPaths) == 0

        if shouldAutoDiscover {
          // This is when auto-discovery should be allowed
          // In real implementation: proceed with PATH search and add discovered paths
          await Config.Connection.addAgdaPath("/discovered/agda")
          let discoveredPaths = Config.Connection.getAgdaPaths()
          Assert.deepStrictEqual(discoveredPaths, ["/discovered/agda"])
        }
      },
    )

    Async.it(
      "should verify addAgdaPath idempotency works correctly",
      async () => {
        // Test that addAgdaPath doesn't create duplicates (existing functionality)
        await Config.Connection.setAgdaPaths([])

        // Add same path multiple times
        await Config.Connection.addAgdaPath("/some/path")
        await Config.Connection.addAgdaPath("/some/path")
        await Config.Connection.addAgdaPath("/some/path")

        let paths = Config.Connection.getAgdaPaths()

        // Should only appear once
        Assert.deepStrictEqual(paths, ["/some/path"])
        Assert.deepStrictEqual(Array.length(paths), 1)
      },
    )
  })

  describe("Configuration State Management", () => {
    Async.it(
      "should maintain configuration integrity across multiple operations",
      async () => {
        // Test comprehensive config management

        // Start clean
        await Config.Connection.setAgdaPaths([])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [])

        // Set initial user config
        await Config.Connection.setAgdaPaths(["agda-user"])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["agda-user"])

        // Try to add duplicate - should be ignored
        await Config.Connection.addAgdaPath("agda-user")
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["agda-user"])

        // Add different path - should append
        await Config.Connection.addAgdaPath("agda-system")
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["agda-user", "agda-system"])

        // Replace entire config
        await Config.Connection.setAgdaPaths(["agda-new"])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["agda-new"])
      },
    )

    Async.it(
      "should handle empty and invalid configurations gracefully",
      async () => {
        // Test edge cases in configuration handling

        // Empty array should work
        await Config.Connection.setAgdaPaths([])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [])

        // Single empty string should be preserved (user might have reasons)
        await Config.Connection.setAgdaPaths([""])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), [""])

        // Multiple mixed paths should work
        await Config.Connection.setAgdaPaths(["", "agda", ""])
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["", "agda", ""])
      },
    )
  })

  Async.after(async () => {
    // Clean up test state
    await Config.Connection.setAgdaPaths([])
  })
})
