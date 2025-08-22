open Mocha
open Test__Util

// Memento.PickedConnection
// ├── Memento path exists in config → should prioritize over other config paths
// ├── Memento path not in config → should ignore memento, use config as-is
// └── Memento always updated → should set to working connection path (regardless of source)

describe("Memento.PickedConnection", () => {
  let userAgda = ref("")
  let systemAgda = ref("")
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

  // Mock platform for command discovery
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

  let makeConnection = async (
    previouslySelectedPath: option<string>,
    configPaths: array<string>,
  ) => {
    // Mock memento to simulate previously selected path
    let memento = Memento.make(None)
    switch previouslySelectedPath {
    | Some(path) => await memento->Memento.PickedConnection.set(Some(path))
    | None => ()
    }

    // Set config paths
    await Config.Connection.setAgdaPaths(logChannel, configPaths)

    await Connection.makeWithFallback(
      platform,
      memento,
      VSCode.Uri.file("/tmp/test"),
      configPaths,
      ["whatever"], // to invoke our `findCommand` mock
      logChannel,
    )
  }

  describe("Memento path exists in config", () => {
    Async.it(
      "should prioritize memento path over other config paths",
      async () => {
        // Config: [alternativeAgda, userAgda]
        // Memento: userAgda
        // Expected: userAgda should be used (prioritized)

        let configPaths = [alternativeAgda.contents, userAgda.contents]
        let result = await makeConnection(Some(userAgda.contents), configPaths)

        switch result {
        | Ok(connection) =>
          let actualPath = connection->Connection.getPath
          Assert.deepStrictEqual(actualPath, userAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )
  })

  describe("Memento path not in config", () => {
    Async.it(
      "should ignore memento and use config as-is",
      async () => {
        // Config: [userAgda]
        // Memento: systemAgda (not in config)
        // Expected: userAgda should be used (from config)

        let configPaths = [userAgda.contents]
        let result = await makeConnection(Some(systemAgda.contents), configPaths)

        switch result {
        | Ok(connection) =>
          let actualPath = connection->Connection.getPath
          Assert.deepStrictEqual(actualPath, userAgda.contents)
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )
  })

  describe("Memento always updated", () => {
    Async.it(
      "should set memento to working connection path from config",
      async () => {
        let memento = Memento.make(None)
        await Config.Connection.setAgdaPaths(logChannel, [userAgda.contents])

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test"),
          [userAgda.contents],
          ["whatever"],
          logChannel,
        )

        switch result {
        | Ok(connection) =>
          let actualPath = connection->Connection.getPath
          let mementoPath = Memento.PickedConnection.get(memento)
          Assert.deepStrictEqual(actualPath, userAgda.contents)
          Assert.deepStrictEqual(mementoPath, Some(userAgda.contents))
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )

    Async.it(
      "should set memento to working connection path from auto discovery",
      async () => {
        let memento = Memento.make(None)
        await Config.Connection.setAgdaPaths(logChannel, []) // empty config

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test"),
          [],
          ["whatever"], // will trigger auto discovery via findCommand
          logChannel,
        )

        switch result {
        | Ok(connection) =>
          let actualPath = connection->Connection.getPath
          let mementoPath = Memento.PickedConnection.get(memento)
          Assert.deepStrictEqual(actualPath, systemAgda.contents)
          Assert.deepStrictEqual(mementoPath, Some(systemAgda.contents))
        | Error(_) => Assert.fail("Connection should succeed")
        }
      },
    )
  })
})
