open Mocha
open Test__Util

describe("Connection Config Path Management", () => {
  let userAgda = ref("")
  let systemAgda = ref("")
  let logChannel = Chan.make()

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
      logChannel,
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
        await Config.Connection.setAgdaPaths(logChannel, userConfig)

        let listener = Log.collect(logChannel)
        let result = await makeConnection(None)
        let logs = listener(~filter=Log.isConfig)

        // make sure that no edits were made to the configuration
        Assert.deepStrictEqual(logs, [])

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
        await Config.Connection.setAgdaPaths(logChannel, userConfig)

        let listener = Log.collect(logChannel)
        let result = await makeConnection(Some(userAgda.contents))
        let logs = listener(~filter=Log.isConfig)

        // make sure that no edits were made to the configuration
        Assert.deepStrictEqual(logs, [])

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
      "should update previously selected path when it doesn't exist in user config but user config works",
      async () => {
        // Precondition
        //    * User has set `userAgda` in the configuration (and it works)
        //    * User has previously selected `systemAgda` (not in config)

        let userConfig = [userAgda.contents]
        await Config.Connection.setAgdaPaths(logChannel, userConfig)

        let listener = Log.collect(logChannel)
        let result = await makeConnection(Some(systemAgda.contents))
        let logs = listener(~filter=Log.isConfig)

        // make sure that the config was NOT modified
        Assert.deepStrictEqual(logs, [])

        // user's configuration should remain unchanged
        let expectedConfig = [userAgda.contents]
        let actualConfig = Config.Connection.getAgdaPaths()
        Assert.deepStrictEqual(actualConfig, expectedConfig)

        switch result {
        | Ok(connection) =>
          // user's working path should be used instead of the previously selected non-config path
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
        await Config.Connection.setAgdaPaths(logChannel, userConfig)

        let listener = Log.collect(logChannel)
        let result = await makeConnection(None)
        let logs = listener(~filter=Log.isConfig)

        // make sure that the path was added
        Assert.deepStrictEqual(logs, [Log.Config(Changed([], [systemAgda.contents]))])

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
})
