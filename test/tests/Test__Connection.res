open Mocha
open Test__Util

describe("Connection", () => {
  describe("URI.parse", () => {
    Async.it(
      "should be able to parse URIs with lsp: as the protocol",
      async () => {
        let actual = Connection__URI.parse("lsp://path/to/als")
        let expected = Connection__URI.URL(NodeJs.Url.make("lsp://path/to/als"))
        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should be able to parse file paths",
      async () => {
        let actual = Connection__URI.parse("path/to/als")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("path/to/als")
        } else {
          Connection__URI.Filepath("path\\to\\als")
        }
        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should be able to parse convert \"/c/path/to/agda\" to \"c:/path/to/agda\" on Windows",
      async () => {
        let actual = Connection__URI.parse("/c/path/to/agda")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("/c/path/to/agda")
        } else {
          Connection__URI.Filepath("c:\\path\\to\\agda")
        }

        Assert.deepEqual(actual, expected)

        let actual = Connection__URI.parse("/d/path/to/agda")
        let expected = if OS.onUnix {
          Connection__URI.Filepath("/d/path/to/agda")
        } else {
          Connection__URI.Filepath("d:\\path\\to\\agda")
        }

        Assert.deepEqual(actual, expected)
      },
    )
  })

  describe("Target", () => {
    let agdaMockPath = ref("")
    let agdaMockTarget = ref(None)

    Async.before(
      async () => {
        // setup the Agda mock
        agdaMockPath := (await Target.Agda.mock(~version="2.7.0.1", ~name="agda-mock"))

        switch await Connection.Target.fromRawPath(agdaMockPath.contents) {
        | Ok(target) => agdaMockTarget := Some(target)
        | Error(_) => failwith("Got error when trying to construct a mock for Agda ")
        }
      },
    )

    Async.it(
      "should return the previously picked connection",
      async () => {
        // access the Agda mock
        let agdaMockTarget = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the momento
        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))

        let paths = [agdaMockPath.contents, "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Ok(agdaMockTarget)

        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when there's no previously picked connection",
      async () => {
        // setup the momento
        let memento = State__Memento.make(None)
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = if OS.onUnix {
          Error([
            Connection__Target.Error.ValidationError("path/to/agda", NotFound("path/to/agda")),
            Connection__Target.Error.ValidationError("path/to/als", NotFound("path/to/als")),
          ])
        } else {
          Error([
            Connection__Target.Error.ValidationError("path\\to\\agda", NotFound("path\\to\\agda")),
            Connection__Target.Error.ValidationError("path\\to\\als", NotFound("path\\to\\als")),
          ])
        }

        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should return nothing when the previously picked connection is not in the supplied paths",
      async () => {
        // access the Agda mock
        let agdaMockTarget = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the momento
        let memento = State__Memento.make(None)
        await Connection.Target.setPicked(memento, Some(agdaMockTarget))
        let paths = ["path/to/agda", "path/to/als"]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = if OS.onUnix {
          Error([
            Connection__Target.Error.ValidationError("path/to/agda", NotFound("path/to/agda")),
            Connection__Target.Error.ValidationError("path/to/als", NotFound("path/to/als")),
          ])
        } else {
          Error([
            Connection__Target.Error.ValidationError("path\\to\\agda", NotFound("path\\to\\agda")),
            Connection__Target.Error.ValidationError("path\\to\\als", NotFound("path\\to\\als")),
          ])
        }

        Assert.deepEqual(actual, expected)
      },
    )

    Async.it(
      "should return the first usable connection target when the previously picked connection is invalid or not in the supplied paths",
      async () => {
        // access the Agda mock
        let agdaMockTarget = switch agdaMockTarget.contents {
        | Some(target) => target
        | None => failwith("Unable to access the Agda mock target")
        }

        // setup the momento
        let memento = State__Memento.make(None)
        let paths =
          [
            "path/to/non-existent-agda",
            agdaMockPath.contents,
            "path/to/non-existent-als",
          ]->Array.map(Connection__URI.parse)

        let actual = await Connection__Target.getPicked(memento, paths)
        let expected = Ok(agdaMockTarget)

        Assert.deepEqual(actual, expected)
      },
    )

    Async.after(
      async () => {
        // cleanup the Agda mock
        switch agdaMockTarget.contents {
        | Some(target) =>
          Target.Agda.destroy(target)
          agdaMockTarget := None
        | None => ()
        }
      },
    )
  })

  describe("Command searching", () => {
    Async.it(
      "should be able to find itself (`which` or `where`)",
      async () => {
        if OS.onUnix {
          switch await Connection__Command__Search.search("which") {
          | Ok(_output) => ()
          | Error(_) => failwith("expected to find `which`")
          }
        } else {
          switch await Connection__Command__Search.search("where") {
          | Ok(_output) => ()
          | Error(_) => failwith("expected to find `where`")
          }
        }
      },
    )
    Async.it(
      "should return an error when the command is not found",
      async () => {
        switch await Connection__Command__Search.search("non-existent-command") {
        | Ok(_output) => failwith("expected to not find `non-existent-command`")
        | Error(_) => ()
        }
      },
    )
  })

  describe("findCommands", () => {
    Async.it(
      "should return the connection when a command is found",
      async () => {
        let commands = ["agda", "als"]
        switch await Connection.findCommands(commands) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }
      },
    )

    Async.it(
      "should return an error when the command is not found",
      async () => {
        let commands = ["non-existent-command"]
        switch await Connection.findCommands(commands) {
        | Ok(_) => failwith("expected to not find `non-existent-command`")
        | Error(_) => ()
        }
      },
    )
  })

  describe("make", () => {
    Async.it(
      "should return a connection and add it to the Memento when the command is found",
      async () => {
        // get the path of `agda` first
        let path = switch await Connection.findCommands(["agda"]) {
        | Ok(path) => path->Connection.Target.toURI->Connection__Target.URI.toString
        | Error(_) => failwith("expected to find `agda`")
        }

        // remove all paths in the config
        await Config.Connection.setAgdaPaths([])
        let paths = Config.Connection.getAgdaPaths()

        let memento = State__Memento.make(None)
        let commands = ["agda", "als"]
        let platform = await Connection__Download__Platform.determine()
        switch await Connection.make(memento, paths, commands, platform) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }

        Assert.deepEqual(Config.Connection.getAgdaPaths(), [path]->Array.map(Connection__URI.parse))

        let pathIsNowInConfig =
          Config.Connection.getAgdaPaths()->Util.Array.includes(path->Connection__URI.parse)
        Assert.ok(pathIsNowInConfig)

        switch await Connection.Target.getPicked(memento, Config.Connection.getAgdaPaths()) {
        | Error(_) => failwith("expected to find the picked connection")
        | Ok(picked) =>
          Assert.deepStrictEqual(
            picked->Connection.Target.toURI->Connection__Target.URI.toString,
            path,
          )
        }
      },
    )

    Async.it(
      "should return a connection and add it to the Memento when the command is found",
      async () => {
        // get the path of `agda` first
        let path = switch await Connection.findCommands(["agda"]) {
        | Ok(path) => path->Connection.Target.toURI
        | Error(_) => failwith("expected to find `agda`")
        }

        await Config.Connection.setAgdaPaths(["some/other/path"]->Array.map(Connection__URI.parse))
        let paths = Config.Connection.getAgdaPaths()

        let memento = State__Memento.make(None)
        let commands = ["agda", "als"]
        let platform = await Connection__Download__Platform.determine()
        switch await Connection.make(memento, paths, commands, platform) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }

        Assert.deepEqual(Config.Connection.getAgdaPaths(), [...paths, path])

        let pathIsNowInConfig = Config.Connection.getAgdaPaths()->Util.Array.includes(path)
        Assert.ok(pathIsNowInConfig)

        switch await Connection.Target.getPicked(memento, Config.Connection.getAgdaPaths()) {
        | Error(_) => failwith("expected to find the picked connection")
        | Ok(picked) => Assert.deepStrictEqual(picked->Connection.Target.toURI, path)
        }
      },
    )

    Async.it(
      "should do nothing when the command is already in the paths of the config",
      async () => {
        // get the path of `agda` first
        let path = switch await Connection.findCommands(["agda"]) {
        | Ok(path) => path->Connection.Target.toURI
        | Error(_) => failwith("expected to find `agda`")
        }

        await Config.Connection.setAgdaPaths([path, Connection__URI.parse("some/other/path")])
        let paths = Config.Connection.getAgdaPaths()

        let memento = State__Memento.make(None)
        let commands = ["agda", "als"]
        let platform = await Connection__Download__Platform.determine()
        switch await Connection.make(memento, paths, commands, platform) {
        | Ok(_) => ()
        | Error(error) =>
          let (header, body) = Connection.Error.toString(error)
          failwith("expected to find `agda` or `als`: " ++ header ++ " - " ++ body)
        }

        Assert.deepEqual(Config.Connection.getAgdaPaths(), paths)

        switch await Connection.Target.getPicked(memento, Config.Connection.getAgdaPaths()) {
        | Error(_) => failwith("expected to find the picked connection")
        | Ok(picked) => Assert.deepStrictEqual(picked->Connection.Target.toURI, path)
        }
      },
    )
  })

  // describe("State__Request.onCannotFindALSorAgdaError", () => {
  //   Async.it(
  //     "should download the latest version of ALS",
  //     async () => {
  //       switch await Connection.downloadLatestALS() {
  //       | Ok(_) => ()
  //       | Error(_) => failwith("expected to download the latest version of ALS")
  //       }
  //     },
  //   )
  // })
})
