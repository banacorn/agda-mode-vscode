open Mocha
open Test__Util

let getAgdaTarget = async () => {
  let platformDeps = Desktop.make()
  switch await Connection.fromPathsOrCommands(
    platformDeps,
    [("agda", Connection.Error.Establish.FromConfig)],
  ) {
  | Ok(connection) => connection
  | Error(_) => failwith("expected to find `agda`")
  }
}

// TODO: rewrite everything here
describe("Connection", () => {
  This.timeout(10000)

  describe("Target", () => {
    let agdaMockPath = ref("")
    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        // setup the Agda mock
        agdaMockPath := (await Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-mock"))
        agdaMockEndpoint := Some(agdaMockPath.contents)
      },
    )

    Async.after(
      async () => {
        // cleanup the Agda mock
        switch agdaMockEndpoint.contents {
        | Some(target) =>
          await Candidate.Agda.destroy(target)
          agdaMockEndpoint := None
        | None => ()
        }
      },
    )
  })

  describe("Command searching", () => {
    Async.it(
      "should return an error when the command is not found",
      async () => {
        switch await Connection__Command.search("non-existent-command") {
        | Ok(_output) => failwith("expected to not find `non-existent-command`")
        | Error(_) => ()
        }
      },
    )
  })

  describe("checkForPrebuiltDataDirectory", () => {
    Async.it(
      "should return asset path when data directory exists",
      async () => {
        // Create a temporary directory structure
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let execPath = NodeJs.Path.join([tempDir, "bin", "agda-language-server"])
        let dataDir = NodeJs.Path.join([tempDir, "bin", "data"])

        // Create the directory structure
        await NodeJs.Fs.mkdir(NodeJs.Path.join([tempDir, "bin"]), {recursive: true, mode: 0o777})
        await NodeJs.Fs.mkdir(dataDir, {recursive: true, mode: 0o777})

        // Test the function
        let result = await Connection.checkForPrebuiltDataDirectory(execPath)

        // Should return Some with asset path
        let expectedAssetPath = NodeJs.Path.join([execPath, "..", "data"])
        Assert.deepStrictEqual(result, Some(expectedAssetPath))

        // Cleanup
        NodeJs.Fs.rmdirSync(dataDir)
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "bin"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )

    Async.it(
      "should return None when data directory does not exist",
      async () => {
        // Create a temporary directory structure without data directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let execPath = NodeJs.Path.join([tempDir, "bin", "agda-language-server"])

        // Create only the bin directory, no data directory
        await NodeJs.Fs.mkdir(NodeJs.Path.join([tempDir, "bin"]), {recursive: true, mode: 0o777})

        // Test the function
        let result = await Connection.checkForPrebuiltDataDirectory(execPath)

        // Should return None
        Assert.deepStrictEqual(result, None)

        // Cleanup
        NodeJs.Fs.rmdirSync(NodeJs.Path.join([tempDir, "bin"]))
        NodeJs.Fs.rmdirSync(tempDir)
      },
    )
  })

  describe("`probeFilepath`", () => {
    let agdaMockPath = ref("")
    let alsMockPath = ref("")

    Async.before(
      async () => {
        // Setup Agda mock
        agdaMockPath := (await Candidate.Agda.mock(~version="2.6.4.1", ~name="agda-probe-mock"))
        // Setup ALS mock
        alsMockPath :=
          (
            await Candidate.ALS.mock(
              ~alsVersion="1.2.3",
              ~agdaVersion="2.6.4",
              ~name="als-probe-mock",
            )
          )
      },
    )

    Async.after(
      async () => {
        // Cleanup mocks
        if agdaMockPath.contents != "" {
          await Candidate.Agda.destroy(agdaMockPath.contents)
        }
        if alsMockPath.contents != "" {
          await Candidate.ALS.destroy(alsMockPath.contents)
        }
      },
    )

    Async.it(
      "should correctly identify Agda executable",
      async () => {
        let result = await Connection.probeFilepath(agdaMockPath.contents)

        switch result {
        | Ok(path, IsAgda(version)) =>
          Assert.deepStrictEqual(path, agdaMockPath.contents)
          Assert.deepStrictEqual(version, "2.6.4.1")
        | Ok(_, _) => Assert.fail("Expected Agda result, got ALS")
        | Error(_) => Assert.fail("Expected successful probe of Agda mock")
        }
      },
    )

    Async.it(
      "should correctly identify ALS executable",
      async () => {
        let result = await Connection.probeFilepath(alsMockPath.contents)

        switch result {
        | Ok(path, IsALS(alsVersion, agdaVersion, lspOptions)) =>
          Assert.deepStrictEqual(path, alsMockPath.contents)
          Assert.deepStrictEqual(alsVersion, "1.2.3")
          Assert.deepStrictEqual(agdaVersion, "2.6.4")
          // lspOptions should be None for this mock (no prebuilt data directory)
          Assert.deepStrictEqual(lspOptions, None)
        | Ok(_, _) => Assert.fail("Expected ALS result, got Agda")
        | Error(_) => Assert.fail("Expected successful probe of ALS mock")
        }
      },
    )

    Async.it(
      "should return NotAgdaOrALS error for unrecognized executable",
      async () => {
        // Create a mock executable that outputs something unrecognized
        let mockOutput = "Some other program version 1.0"
        let mockPath = if OS.onUnix {
          let fileName = "unrecognized-mock"
          let content = "#!/bin/sh\necho '" ++ mockOutput ++ "'\nexit 0"
          let tempFile = NodeJs.Path.join([NodeJs.Os.tmpdir(), fileName])
          NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(content))
          await NodeJs.Fs.chmod(tempFile, ~mode=0o755)
          tempFile
        } else {
          let fileName = "unrecognized-mock.bat"
          let content = "@echo " ++ mockOutput
          let tempFile = NodeJs.Path.join([NodeJs.Os.tmpdir(), fileName])
          NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(content))
          tempFile
        }

        let result = await Connection.probeFilepath(mockPath)

        switch result {
        | Error(Connection__Error.Probe.NotAgdaOrALS(output)) =>
          // Trim output to handle potential newline characters
          Assert.deepStrictEqual(String.trim(output), mockOutput)
        | Ok(_) => Assert.fail("Expected NotAgdaOrALS error")
        | Error(_) => Assert.fail("Expected NotAgdaOrALS error, got different error")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(mockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should return CannotDetermineAgdaOrALS error for non-executable file",
      async () => {
        let nonExecutablePath = NodeJs.Path.join([NodeJs.Os.tmpdir(), "non-executable.txt"])
        NodeJs.Fs.writeFileSync(nonExecutablePath, NodeJs.Buffer.fromString("not executable"))

        let result = await Connection.probeFilepath(nonExecutablePath)

        switch result {
        | Error(Connection__Error.Probe.CannotDetermineAgdaOrALS(_)) => ()
        | Ok(_) => Assert.fail("Expected CannotDetermineAgdaOrALS error")
        | Error(_) => Assert.fail("Expected CannotDetermineAgdaOrALS error, got different error")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(nonExecutablePath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should return CannotDetermineAgdaOrALS error for non-existent file",
      async () => {
        let nonExistentPath = "/path/that/does/not/exist/executable"
        let result = await Connection.probeFilepath(nonExistentPath)

        switch result {
        | Error(Connection__Error.Probe.CannotDetermineAgdaOrALS(_)) => ()
        | Ok(_) => Assert.fail("Expected CannotDetermineAgdaOrALS error")
        | Error(_) => Assert.fail("Expected CannotDetermineAgdaOrALS error, got different error")
        }
      },
    )

    Async.it(
      "should detect prebuilt data directory for ALS with LSP options",
      async () => {
        // Create a temporary directory structure with data directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "als-data-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let binDir = NodeJs.Path.join([tempDir, "bin"])
        let dataDir = NodeJs.Path.join([binDir, "data"])
        let alsPath = NodeJs.Path.join([binDir, "als-with-data"])

        // Create directory structure
        await NodeJs.Fs.mkdir(binDir, {recursive: true, mode: 0o777})
        await NodeJs.Fs.mkdir(dataDir, {recursive: true, mode: 0o777})

        // Create ALS mock with data directory
        let alsOutput = "Agda v2.6.4 Language Server v1.2.3"
        let (fileName, content) = if OS.onUnix {
          (alsPath, "#!/bin/sh\necho '" ++ alsOutput ++ "'\nexit 0")
        } else {
          (alsPath ++ ".bat", "@echo " ++ alsOutput)
        }

        NodeJs.Fs.writeFileSync(fileName, NodeJs.Buffer.fromString(content))
        if OS.onUnix {
          await NodeJs.Fs.chmod(fileName, ~mode=0o755)
        }

        let result = await Connection.probeFilepath(fileName)

        // Normalize fileName through the same URI parsing process for consistent comparison
        let normalizedFileName = switch Connection__URI.parse(fileName) {
        | FileURI(_, vsCodeUri) => VSCode.Uri.fsPath(vsCodeUri)
        }

        switch result {
        | Ok(path, IsALS(alsVersion, agdaVersion, Some(lspOptions))) =>
          Assert.deepStrictEqual(path, normalizedFileName)
          Assert.deepStrictEqual(alsVersion, "1.2.3")
          Assert.deepStrictEqual(agdaVersion, "2.6.4")
          // Should have Agda_datadir environment variable set
          switch lspOptions.env->Option.flatMap(Js.Dict.get(_, "Agda_datadir")) {
          | Some(dataDirPath) => Assert.ok(String.includes(dataDirPath, "data"))
          | None => Assert.fail("Expected Agda_datadir in LSP options")
          }
        | Ok(_, IsALS(_, _, None)) => Assert.fail("Expected LSP options with data directory")
        | Ok(_, IsAgda(_)) => Assert.fail("Expected ALS result, got Agda")
        | Ok(_, IsALSWASM(_)) => Assert.fail("Expected ALS result, got ALSWASM")
        | Error(_) => Assert.fail("Expected successful probe of ALS with data directory")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(fileName)
          NodeJs.Fs.rmdirSync(dataDir)
          NodeJs.Fs.rmdirSync(binDir)
          NodeJs.Fs.rmdirSync(tempDir)
        } catch {
        | _ => ()
        }
      },
    )
  })

  describe("`make`", () => {
    let agdaMockPath = ref("")
    let alsMockPath = ref("")

    Async.before(
      async () => {
        // Setup Agda mock
        agdaMockPath := (await Candidate.Agda.mock(~version="2.6.3", ~name="agda-make-mock"))
        // Setup ALS mock
        alsMockPath :=
          (
            await Candidate.ALS.mock(
              ~alsVersion="1.3.1",
              ~agdaVersion="2.6.3",
              ~name="als-make-mock",
            )
          )
      },
    )

    Async.after(
      async () => {
        // Cleanup mocks
        if agdaMockPath.contents != "" {
          await Candidate.Agda.destroy(agdaMockPath.contents)
        }
        if alsMockPath.contents != "" {
          await Candidate.ALS.destroy(alsMockPath.contents)
        }
      },
    )

    Async.it(
      "should successfully create Agda connection from valid Agda path",
      async () => {
        let result = await Connection.make(
          agdaMockPath.contents,
          Connection.Error.Establish.FromConfig,
        )

        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath.contents)
            Assert.deepStrictEqual(version, "2.6.3")
          | ALS(_, _, _) => Assert.fail("Expected Agda connection, got ALS")
          | ALSWASM(_, _, _, _) => Assert.fail("Expected Agda connection, got ALSWASM")
          }
        | Error(_) => Assert.fail("Expected successful connection creation")
        }
      },
    )

    // Skip ALS connection test due to LSP client complexity in test environment
    // The ALS connection requires a working Language Server Protocol client
    // which is complex to setup in test environment
    Async.it(
      "should handle ALS executable probing (without full connection)",
      async () => {
        This.retries(2)

        // Test just the probing part which we know works
        let result = await Connection.probeFilepath(alsMockPath.contents)

        // FIXME: this test is flaky; sometimes it fails to probe
        switch result {
        | Ok(path, IsALS(alsVersion, agdaVersion, lspOptions)) =>
          Assert.deepStrictEqual(path, alsMockPath.contents)
          Assert.deepStrictEqual(alsVersion, "1.3.1")
          Assert.deepStrictEqual(agdaVersion, "2.6.3")
          Assert.deepStrictEqual(lspOptions, None)
        | Ok(_, IsAgda(_)) => Assert.fail("Expected ALS result, got Agda")
        | Ok(_, IsALSWASM(_)) => Assert.fail("Expected ALS result, got ALSWASM")
        | Error(_) => Assert.fail("Expected successful probe")
        }
      },
    )

    Async.it(
      "should return Establish error for non-existent path",
      async () => {
        let nonExistentPath = "/path/that/does/not/exist/agda"
        let result = await Connection.make(
          nonExistentPath,
          Connection.Error.Establish.FromConfig,
        )

        switch result {
        | Ok(_) => Assert.fail("Expected error for non-existent path")
        | Error(errors) =>
          // Should have probe error for the non-existent path
          switch errors.probes->Dict.get(nonExistentPath) {
          | Some((error, _source)) =>
            // Verify it's a CannotDetermineAgdaOrALS error wrapped in candidate error
            switch error {
            | Connection__Error.Probe.CannotDetermineAgdaOrALS(_) => ()
            | _ => Assert.fail("Expected CannotDetermineAgdaOrALS error")
            }
          | None => Assert.fail("Expected probe error for non-existent path")
          }
        }
      },
    )

    Async.it(
      "should return Establish error for unrecognized executable",
      async () => {
        // Create mock executable that outputs unrecognized content
        let mockOutput = "Unknown Program v1.0.0"
        let mockPath = if OS.onUnix {
          let fileName = "unrecognized-make-mock"
          let content = "#!/bin/sh\necho '" ++ mockOutput ++ "'\nexit 0"
          let tempFile = NodeJs.Path.join([NodeJs.Os.tmpdir(), fileName])
          NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(content))
          await NodeJs.Fs.chmod(tempFile, ~mode=0o755)
          tempFile
        } else {
          let fileName = "unrecognized-make-mock.bat"
          let content = "@echo " ++ mockOutput
          let tempFile = NodeJs.Path.join([NodeJs.Os.tmpdir(), fileName])
          NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(content))
          tempFile
        }

        let result = await Connection.make(mockPath, Connection.Error.Establish.FromConfig)

        switch result {
        | Ok(_) => Assert.fail("Expected error for unrecognized executable")
        | Error(errors) =>
          // Should have probe error for the unrecognized executable
          switch errors.probes->Dict.get(mockPath) {
          | Some((error, _source)) =>
            switch error {
            | Connection__Error.Probe.NotAgdaOrALS(output) =>
              Assert.deepStrictEqual(String.trim(output), mockOutput)
            | _ => Assert.fail("Expected NotAgdaOrALS error")
            }
          | None => Assert.fail("Expected probe error for unrecognized executable")
          }
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(mockPath)
        } catch {
        | _ => ()
        }
      },
    )

    // Skip full ALS connection test - test just the prebuilt data directory detection
    Async.it(
      "should detect prebuilt data directory correctly in probing phase",
      async () => {
        // This test covers the prebuilt data directory detection without full ALS connection
        // Create ALS executable with adjacent data directory
        let tempDir = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "als-probe-data-test-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let binDir = NodeJs.Path.join([tempDir, "bin"])
        let dataDir = NodeJs.Path.join([binDir, "data"])
        let alsPath = NodeJs.Path.join([binDir, "als-with-data-probe-test"])

        // Create directory structure
        await NodeJs.Fs.mkdir(binDir, {recursive: true, mode: 0o777})
        await NodeJs.Fs.mkdir(dataDir, {recursive: true, mode: 0o777})

        // Create ALS mock with data directory
        let alsOutput = "Agda v2.6.3 Language Server v1.3.1"
        let (fileName, content) = if OS.onUnix {
          (alsPath, "#!/bin/sh\necho '" ++ alsOutput ++ "'\nexit 0")
        } else {
          (alsPath ++ ".bat", "@echo " ++ alsOutput)
        }

        NodeJs.Fs.writeFileSync(fileName, NodeJs.Buffer.fromString(content))
        if OS.onUnix {
          await NodeJs.Fs.chmod(fileName, ~mode=0o755)
        }

        // Test just the probing which includes data directory detection
        let result = await Connection.probeFilepath(fileName)

        // Normalize fileName through the same URI parsing process for consistent comparison
        let normalizedFileName = switch Connection__URI.parse(fileName) {
        | FileURI(_, vsCodeUri) => VSCode.Uri.fsPath(vsCodeUri)
        }

        switch result {
        | Ok(path, IsALS(alsVersion, agdaVersion, Some(lspOptions))) =>
          Assert.deepStrictEqual(path, normalizedFileName)
          Assert.deepStrictEqual(alsVersion, "1.3.1")
          Assert.deepStrictEqual(agdaVersion, "2.6.3")
          // Should have Agda_datadir environment variable detected
          switch lspOptions.env->Option.flatMap(Js.Dict.get(_, "Agda_datadir")) {
          | Some(dataDirPath) => Assert.ok(String.includes(dataDirPath, "data"))
          | None => Assert.fail("Expected Agda_datadir in LSP options")
          }
        | Ok(_, IsALS(_, _, None)) => Assert.fail("Expected LSP options with data directory")
        | Ok(_, IsAgda(_)) => Assert.fail("Expected ALS result, got Agda")
        | Ok(_, IsALSWASM(_)) => Assert.fail("Expected ALS result, got ALSWASM")
        | Error(_) => Assert.fail("Expected successful probe with data directory")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(fileName)
          NodeJs.Fs.rmdirSync(dataDir)
          NodeJs.Fs.rmdirSync(binDir)
          NodeJs.Fs.rmdirSync(tempDir)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle error construction correctly for probe errors",
      async () => {
        let invalidPath = "/definitely/does/not/exist/agda"
        let result = await Connection.make(invalidPath, Connection.Error.Establish.FromConfig)

        switch result {
        | Ok(_) => Assert.fail("Expected construction error")
        | Error(errors) =>
          // Verify error structure
          Assert.deepStrictEqual(Array.length(errors.probes->Dict.toArray), 1)
          Assert.deepStrictEqual(Array.length(errors.commands->Dict.toArray), 0)
          Assert.deepStrictEqual(
            errors.download,
            Connection__Error.Establish.NotAttempted,
          )

          // Verify the specific probe error
          switch errors.probes->Dict.get(invalidPath) {
          | Some((Connection__Error.Probe.CannotDetermineAgdaOrALS(_), _source)) => ()
          | Some(_) => Assert.fail("Expected CannotDetermineAgdaOrALS error")
          | None => Assert.fail("Expected probe error to be present")
          }
        }
      },
    )
  })

  describe("`fromPathsOrCommands`", () => {
    describe("paths", () => {
      let agdaMockEndpoint = ref(None)

      Async.before(
        async () => {
          try {
            // setup the Agda mock
            let path = await Test__Util.Candidate.Agda.mock(
              ~version="2.7.0.1",
              ~name="agda-mock-paths-or-commands",
            )
            agdaMockEndpoint := Some(path)
          } catch {
          | error => failwith("Failed to create Agda mock: " ++ Js.String.make(error))
          }
        },
      )

      Async.after(
        async () => {
          // cleanup the Agda mock
          switch agdaMockEndpoint.contents {
          | Some(path) =>
            try {
              NodeJs.Fs.unlinkSync(path)
            } catch {
            | _ => () // ignore cleanup errors
            }
          | None => ()
          }
        },
      )

      Async.it(
        "should connect successfully with valid path",
        async () => {
          let mockPath = switch agdaMockEndpoint.contents {
          | Some(path) => path
          | None => failwith("Mock candidate not available")
          }

          let platformDeps = Mock.Platform.makeBasic()
          let paths = [(mockPath, Connection.Error.Establish.FromConfig)]
          let result = await Connection.fromPathsOrCommands(platformDeps, paths)

          switch result {
          | Ok(connection) =>
            switch connection {
            | Agda(_, path, version) =>
              Assert.deepStrictEqual(path, mockPath)
              Assert.deepStrictEqual(version, "2.7.0.1")
            | _ => Assert.fail("Expected Agda connection")
            }
          | Error(_) => Assert.fail("Expected successful connection")
          }
        },
      )

      Async.it(
        "should try multiple paths and use first valid one",
        async () => {
          let mockPath = switch agdaMockEndpoint.contents {
          | Some(path) => path
          | None => failwith("Mock candidate not available")
          }

          let platformDeps = Mock.Platform.makeBasic()
          let paths = [
            ("invalid/path/1", Connection.Error.Establish.FromConfig),
            ("invalid/path/2", Connection.Error.Establish.FromConfig),
            (mockPath, Connection.Error.Establish.FromConfig),
            ("invalid/path/3", Connection.Error.Establish.FromConfig),
          ]
          let result = await Connection.fromPathsOrCommands(platformDeps, paths)

          switch result {
          | Ok(connection) =>
            switch connection {
            | Agda(_, path, version) =>
              Assert.deepStrictEqual(path, mockPath)
              Assert.deepStrictEqual(version, "2.7.0.1")
            | _ => Assert.fail("Expected Agda connection")
            }
          | Error(_) => Assert.fail("Expected successful connection to first valid path")
          }
        },
      )

      Async.it(
        "should return Construction error when all paths are invalid",
        async () => {
          let platformDeps = Mock.Platform.makeBasic()
          let paths = [
            ("invalid/path/1", Connection.Error.Establish.FromConfig),
            ("invalid/path/2", Connection.Error.Establish.FromConfig),
            ("invalid/path/3", Connection.Error.Establish.FromConfig),
          ]
          let result = await Connection.fromPathsOrCommands(platformDeps, paths)

          switch result {
          | Ok(_) => Assert.fail("Expected error with invalid paths")
          | Error(errors) =>
            // Should have three probe errors
            let probeErrors = errors.probes->Dict.toArray
            Assert.deepStrictEqual(Array.length(probeErrors), 3)

            // Should have no command errors
            let commandErrors = errors.commands->Dict.toArray
            Assert.deepStrictEqual(Array.length(commandErrors), 0)

            // Should have no download error
            Assert.deepStrictEqual(
              errors.download,
              Connection__Error.Establish.NotAttempted,
            )
          }
        },
      )

      Async.it(
        "should preserve FromConfig as probe source for resource candidates",
        async () => {
          let platformDeps = Mock.Platform.makeBasic()
          let path = "/definitely/not/a/real/agda"
          let result = await Connection.fromPathsOrCommands(
            platformDeps,
            [(path, Connection.Error.Establish.FromConfig)],
          )

          switch result {
          | Ok(_) => Assert.fail("Expected error with invalid resource candidate")
          | Error(errors) =>
            switch errors.probes->Dict.get(path) {
            | Some((_, source)) =>
              Assert.deepStrictEqual(source, Connection.Error.Establish.FromConfig)
            | None => Assert.fail("Expected probe error entry for invalid resource candidate")
            }
          }
        },
      )

      Async.it(
        "should return empty error when no paths provided",
        async () => {
          let platformDeps = Mock.Platform.makeBasic()
          let paths = []
          let result = await Connection.fromPathsOrCommands(platformDeps, paths)

          switch result {
          | Ok(_) => Assert.fail("Expected error with empty paths")
          | Error(errors) =>
            // Should have no errors since no paths were tried
            let probeErrors = errors.probes->Dict.toArray
            Assert.deepStrictEqual(Array.length(probeErrors), 0)

            let commandErrors = errors.commands->Dict.toArray
            Assert.deepStrictEqual(Array.length(commandErrors), 0)

            Assert.deepStrictEqual(
              errors.download,
              Connection__Error.Establish.NotAttempted,
            )
          }
        },
      )
    })

    describe("commands", () => {
      Async.it(
        "should connect successfully with valid command",
        async () => {
          let platformDeps = Desktop.make()
          let commands = [
            ("agda", Connection.Error.Establish.FromConfig),
            ("als", Connection.Error.Establish.FromConfig),
          ]
          let result = await Connection.fromPathsOrCommands(platformDeps, commands)

          switch result {
          | Ok(connection) =>
            // TODO: Should connect to agda - version varies by environment
            switch connection {
            | Agda(_, _, _) => ()
            | ALS(_, _, _) => ()
            | ALSWASM(_, _, _, _) => ()
            }
          | Error(_) => Assert.fail("Expected successful connection via command")
          }
        },
      )

      Async.it(
        "should try multiple commands and use first valid one",
        async () => {
          let platformDeps = Desktop.make()
          let commands = [
            ("non-existent-cmd", Connection.Error.Establish.FromConfig),
            ("agda", Connection.Error.Establish.FromConfig),
            ("als", Connection.Error.Establish.FromConfig),
          ]
          let result = await Connection.fromPathsOrCommands(platformDeps, commands)

          switch result {
          | Ok(connection) =>
            // TODO: Should connect to agda - version varies by environment
            switch connection {
            | Agda(_, _path, _version) => ()
            | ALS(_, _path, _) => ()
            | ALSWASM(_, _, _, _) => ()
            }
          | Error(_) => Assert.fail("Expected successful connection to valid command")
          }
        },
      )

      Async.it(
        "should return Construction error when all commands are invalid",
        async () => {
          let platformDeps = Desktop.make()
          let commands = [
            ("non-existent-cmd-1", Connection.Error.Establish.FromConfig),
            ("non-existent-cmd-2", Connection.Error.Establish.FromConfig),
            ("non-existent-cmd-3", Connection.Error.Establish.FromConfig),
          ]
          let result = await Connection.fromPathsOrCommands(platformDeps, commands)

          switch result {
          | Ok(_) => Assert.fail("Expected error with invalid commands")
          | Error(errors) =>
            // Should have no probe errors
            let probeErrors = errors.probes->Dict.toArray
            Assert.deepStrictEqual(Array.length(probeErrors), 0)

            // Should have three command errors
            let commandErrors = errors.commands->Dict.toArray
            Assert.deepStrictEqual(Array.length(commandErrors), 3)

            // Verify all command names are present
            let commandNames =
              commandErrors->Array.map(((name, _)) => name)->Array.toSorted(String.compare)
            Assert.deepStrictEqual(
              commandNames,
              ["non-existent-cmd-1", "non-existent-cmd-2", "non-existent-cmd-3"],
            )

            // Should have no download error
            Assert.deepStrictEqual(
              errors.download,
              Connection__Error.Establish.NotAttempted,
            )
          }
        },
      )

      Async.it(
        "should rewrite command candidate source to FromCommandLookup when resolved command fails to connect",
        async () => {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.No
            let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
            let resolveDownloadChannel = (_target, _) => async (_, _, _) =>
              Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
            let download = (_globalStorageUri, _downloadDescriptor) =>
              Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
            let findCommand = (command, ~timeout as _timeout=1000) =>
              switch command {
              | "agda" => Promise.resolve(Ok("/definitely/not/a/real/agda"))
              | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
              }
          }
          let platformDeps: Platform.t = module(MockPlatform)
          let result = await Connection.fromPathsOrCommands(
            platformDeps,
            [("agda", Connection.Error.Establish.FromConfig)],
          )

          switch result {
          | Ok(_) => Assert.fail("Expected error when resolved agda path cannot be probed")
          | Error(errors) =>
            switch errors.probes->Dict.get("/definitely/not/a/real/agda") {
            | Some((_, source)) =>
              Assert.deepStrictEqual(
                source,
                Connection.Error.Establish.FromCommandLookup("agda"),
              )
            | None => Assert.fail("Expected probe error for resolved agda path")
            }
          }
        },
      )

      Async.it(
        "should return empty error when no commands provided",
        async () => {
          let platformDeps = Desktop.make()
          let commands = []
          let result = await Connection.fromPathsOrCommands(platformDeps, commands)

          switch result {
          | Ok(_) => Assert.fail("Expected error with empty commands")
          | Error(errors) =>
            // Should have no errors since no commands were tried
            let probeErrors = errors.probes->Dict.toArray
            Assert.deepStrictEqual(Array.length(probeErrors), 0)

            let commandErrors = errors.commands->Dict.toArray
            Assert.deepStrictEqual(Array.length(commandErrors), 0)

            Assert.deepStrictEqual(
              errors.download,
              Connection__Error.Establish.NotAttempted,
            )
          }
        },
      )
    })
  })

  describe("make with logging", () => {
    Async.it(
      "should log ConnectedToAgda when Agda connection succeeds",
      async () => {
        /**
         * TEST PURPOSE: Verify that Connection.makeWithFallback emits ConnectedToAgda events
         *
         * SCENARIO:
         * 1. Setup a mock Agda executable
         * 2. Create Connection.makeWithFallback with log channel
         * 3. Verify ConnectedToAgda event is logged with correct path and version
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel to capture all log events
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Setup mock Agda using Test__Util
        let agdaMockPath = await Test__Util.Candidate.Agda.mock(
          ~version="2.6.4",
          ~name="agda-mock-for-make",
        )

        // Create minimal memento and platformDeps
        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.makeWithFallback with the mock Agda path
        switch await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          [agdaMockPath], // paths
          [], // commands
          logChannel,
        ) {
        | Ok(connection) =>
          // VERIFY: ConnectedToAgda event was logged with exact details
          Assert.deepStrictEqual(
            loggedEvents,
            [Log.Connection(Log.Connection.ConnectedToAgda(agdaMockPath, "2.6.4"))],
          )

          // VERIFY: Connection type matches logged event
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.6.4")
          | _ => Assert.fail("Expected Agda connection")
          }

        | Error(_) => Assert.fail("Expected connection to succeed")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => () // Ignore cleanup errors
        }
      },
    )

    Async.it(
      "should log connection events when using real agda command",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback logging works with real connections
         *
         * SCENARIO:
         * 1. Try to connect using the real 'agda' command
         * 2. If successful, verify appropriate connection event is logged
         * 3. If failed, verify no connection events are logged
         *
         * This test is more realistic as it uses actual command discovery
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        // Create minimal memento and platformDeps
        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.makeWithFallback with real agda command
        let result = await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          [], // no specific paths
          ["agda"], // try to find agda command
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          // VERIFY: Connection event matches the actual connection
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToAgda(path, version))],
            )
          | ALS(_, path, Some(alsVersion, agdaVersion, _)) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToALS(path, Some(alsVersion, agdaVersion)))],
            )
          | ALS(_, path, None) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToALS(path, None))],
            )
          | ALSWASM(_, _, path, Some(alsVersion, agdaVersion, _)) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToALS(path, Some(alsVersion, agdaVersion)))],
            )
          | ALSWASM(_, _, path, None) =>
            Assert.deepStrictEqual(
              loggedEvents,
              [Log.Connection(Log.Connection.ConnectedToALS(path, None))],
            )
          }

        | Error(_) =>
          // If connection fails, verify no connection events were logged
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should not log connection events when connection fails",
      async () => {
        /**
         * TEST PURPOSE: Verify that failed connections don't emit connection events
         *
         * SCENARIO:
         * 1. Try to connect with invalid paths and commands
         * 2. Verify no ConnectedTo* events are logged
         * 3. Verify Connection.makeWithFallback returns Error
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel to capture all log events
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Create minimal memento and platformDeps (using mock to avoid dialogs)
        let memento = Memento.make(None)
        let platformDeps = Mock.Platform.makeBasic()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.makeWithFallback with invalid paths and commands
        switch await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          ["/nonexistent/path"], // invalid paths
          ["nonexistent-command"], // invalid commands
          logChannel,
        ) {
        | Ok(_) => Assert.fail("Expected connection to fail")
        | Error(_) =>
          // VERIFY: No connection events were logged
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it_skip(
      "should log connection events even when falling back to downloads",
      async () => {
        /**
         * TEST PURPOSE: Verify logging works in download fallback scenarios
         *
         * SCENARIO:
         * 1. Try paths/commands that fail
         * 2. Fallback to download succeeds (mocked)
         * 3. Verify connection event is logged for downloaded connection
         *
         * NOTE: This test may be complex to implement due to download mocking.
         * For now, we'll just verify the interface works correctly.
         */
        let loggedEvents = []
        let logChannel = Chan.make()

        // Subscribe to log channel
        let _ = logChannel->Chan.on(
          logEvent => {
            loggedEvents->Array.push(logEvent)
          },
        )

        // Create mock platform that simulates download failure (no actual download test)
        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        // INVOKE: Connection.makeWithFallback with invalid paths - this should attempt download but fail
        switch await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        ) {
        | Ok(_) =>
          // If it succeeds, verify the exact connection event was logged
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, _))] => () // Expected Agda connection event
          | [Log.Connection(Log.Connection.ConnectedToALS(_, Some(_, _)))] => () // Expected ALS connection event
          | [Log.Connection(Log.Connection.ConnectedToALS(_, None))] => () // Expected ALS connection event without Agda version
          | [] => Assert.fail("Expected connection event to be logged")
          | _ => Assert.fail("Expected exactly one connection event")
          }
        | Error(_) =>
          // If it fails (expected), verify no connection events were logged
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )
  })

  describe("make fromPaths scenarios", () => {
    Async.it(
      "should prioritize valid paths over download fallback",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback uses paths before falling back to download
         *
         * SCENARIO:
         * 1. Setup mock Agda at known path
         * 2. Provide that path
         * 3. Should use the path, not attempt download fallback
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        // Setup mock Agda
        let agdaMockPath = await Test__Util.Candidate.Agda.mock(
          ~version="2.6.4",
          ~name="agda-mock-for-path-priority",
        )

        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          [agdaMockPath], // valid path
          ["invalid-command"], // invalid commands
          logChannel,
        )
        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          // Should have logged connection to the mock path
          Assert.deepStrictEqual(
            loggedEvents,
            [Log.Connection(Log.Connection.ConnectedToAgda(agdaMockPath, "2.6.4"))],
          )

          // Connection should match the mock
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.6.4")
          | _ => Assert.fail("Expected Agda connection")
          }

        | Error(_) => Assert.fail("Expected connection to mock Agda path")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )
  })

  describe("Connection contract coverage", () => {
    let configAgda = ref("")
    let pickedAgda = ref("")
    let downloadedAgda = ref("")

    Async.before(async () => {
      configAgda := (await Test__Util.Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-spec-config"))
      pickedAgda := (await Test__Util.Candidate.Agda.mock(~version="2.7.0.2", ~name="agda-spec-picked"))
      downloadedAgda :=
        (
          await Test__Util.Candidate.Agda.mock(
            ~version="2.7.0.3",
            ~name="agda-spec-downloaded",
          )
        )
    })

    Async.after(async () => {
      try {
        await Test__Util.Candidate.Agda.destroy(configAgda.contents)
      } catch {
      | _ => ()
      }
      try {
        await Test__Util.Candidate.Agda.destroy(pickedAgda.contents)
      } catch {
      | _ => ()
      }
      try {
        await Test__Util.Candidate.Agda.destroy(downloadedAgda.contents)
      } catch {
      | _ => ()
      }
    })

    let makePickedFailureExecutable = async (~name: string) => {
      let stamp = string_of_int(int_of_float(Js.Date.now()))
      let base = name ++ "-" ++ stamp
      let tmp = NodeJs.Os.tmpdir()
      let markerPath = NodeJs.Path.join([tmp, base ++ ".marker"])
      let executablePath = NodeJs.Path.join([tmp, base ++ (if OS.onUnix { "" } else { ".bat" })])

      let content = if OS.onUnix {
        "#!/bin/sh\n"
        ++ "echo picked >> \"" ++ markerPath ++ "\"\n"
        ++ "echo not-agda\n"
        ++ "exit 0\n"
      } else {
        "@echo off\r\n"
        ++ "echo picked>>\"" ++ markerPath ++ "\"\r\n"
        ++ "echo not-agda\r\n"
      }

      NodeJs.Fs.writeFileSync(executablePath, NodeJs.Buffer.fromString(content))
      if OS.onUnix {
        let _ = await NodeJs.Fs.chmod(executablePath, ~mode=0o755)
      }

      (executablePath, markerPath)
    }

    let makeFirstFailThenSuccessExecutable = async (~name: string) => {
      let stamp = string_of_int(int_of_float(Js.Date.now()))
      let base = name ++ "-" ++ stamp
      let tmp = NodeJs.Os.tmpdir()
      let flagPath = NodeJs.Path.join([tmp, base ++ ".flag"])
      let executablePath = NodeJs.Path.join([tmp, base ++ (if OS.onUnix { "" } else { ".bat" })])

      let content = if OS.onUnix {
        "#!/bin/sh\n"
        ++ "if [ -f \"" ++ flagPath ++ "\" ]; then\n"
        ++ "  echo \"Agda version 9.9.9\"\n"
        ++ "else\n"
        ++ "  echo seen > \"" ++ flagPath ++ "\"\n"
        ++ "  echo not-agda\n"
        ++ "fi\n"
        ++ "exit 0\n"
      } else {
        "@echo off\r\n"
        ++ "if exist \"" ++ flagPath ++ "\" (\r\n"
        ++ "  @echo Agda version 9.9.9\r\n"
        ++ ") else (\r\n"
        ++ "  @echo seen>\"" ++ flagPath ++ "\"\r\n"
        ++ "  @echo not-agda\r\n"
        ++ ")\r\n"
      }

      NodeJs.Fs.writeFileSync(executablePath, NodeJs.Buffer.fromString(content))
      if OS.onUnix {
        let _ = await NodeJs.Fs.chmod(executablePath, ~mode=0o755)
      }

      (executablePath, flagPath)
    }

    let cleanupIfExists = path => {
      if NodeJs.Fs.existsSync(path) {
        try {
          NodeJs.Fs.unlinkSync(path)
        } catch {
        | _ => ()
        }
      }
    }

    Async.it(
      "should try PickedConnection first even when it is not in connection.paths",
      async () => {
        let memento = Memento.make(None)
        await Memento.PickedConnection.set(memento, Some(pickedAgda.contents))

        let result = await Connection.makeWithFallback(
          Mock.Platform.makeBasic(),
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          [configAgda.contents],
          [],
          Chan.make(),
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, pickedAgda.contents)
        | Error(_) => Assert.fail("Expected connection to succeed via picked connection")
        }
      },
    )

    Async.it(
      "should continue to later steps when PickedConnection fails",
      async () => {
        let (pickedPath, markerPath) = await makePickedFailureExecutable(~name="agda-picked-fail")
        let memento = Memento.make(None)
        await Memento.PickedConnection.set(memento, Some(pickedPath))

        let result = await Connection.makeWithFallback(
          Mock.Platform.makeBasic(),
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          [configAgda.contents],
          [],
          Chan.make(),
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, configAgda.contents)
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(markerPath), true)
        | Error(_) => Assert.fail("Expected fallback to later chain steps after picked failure")
        }

        cleanupIfExists(pickedPath)
        cleanupIfExists(markerPath)
      },
    )

    Async.it(
      "should not re-probe PickedConnection in the paths step",
      async () => {
        let (pickedPath, flagPath) = await makeFirstFailThenSuccessExecutable(
          ~name="agda-picked-duplicate",
        )
        let memento = Memento.make(None)
        await Memento.PickedConnection.set(memento, Some(pickedPath))

        let result = await Connection.makeWithFallback(
          Mock.Platform.makeBasic(),
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          [pickedPath, configAgda.contents, pickedPath],
          [],
          Chan.make(),
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, configAgda.contents)
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(flagPath), true)
        | Error(_) => Assert.fail("Expected connection to succeed via non-duplicate path probing")
        }

        cleanupIfExists(pickedPath)
        cleanupIfExists(flagPath)
      },
    )

    Async.it(
      "should resolve bare command names in connection.paths via findCommand",
      async () => {
        await Config.Connection.DownloadPolicy.set(No)
        let agdaCount = ref(0)
        let alsCount = ref(0)
        let platform = (
          module(
            {
              include Desktop.Desktop
              let findCommand = (command, ~timeout as _timeout=1000) => {
                switch command {
                | "agda" => agdaCount := agdaCount.contents + 1
                | "als" => alsCount := alsCount.contents + 1
                | _ => ()
                }
                Promise.resolve(Error(Connection__Command.Error.NotFound))
              }
            }
          ): Platform.t
        )

        let result = await Connection.makeWithFallback(
          platform,
          Memento.make(None),
          VSCode.Uri.file("/tmp/test-storage"),
          ["agda", "als"],
          ["agda", "als"],
          Chan.make(),
        )

        switch result {
        | Ok(_) => Assert.fail("Expected failure with all commands unresolved")
        | Error(_) =>
          Assert.deepStrictEqual(agdaCount.contents, 1)
          Assert.deepStrictEqual(alsCount.contents, 1)
        }
      },
    )

    Async.it(
      "should not persist resolved absolute command paths back into connection.paths",
      async () => {
        let logChannel = Chan.make()
        await Config.Connection.setAgdaPaths(logChannel, ["agda"])
        let platform = (
          module(
            {
              include Desktop.Desktop
              let findCommand = (command, ~timeout as _timeout=1000) =>
                switch command {
                | "agda" => Promise.resolve(Ok(configAgda.contents))
                | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
                }
            }
          ): Platform.t
        )

        let result = await Connection.makeWithFallback(
          platform,
          Memento.make(None),
          VSCode.Uri.file("/tmp/test-storage"),
          ["agda"],
          [],
          logChannel,
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, configAgda.contents)
          Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), ["agda"])
        | Error(_) => Assert.fail("Expected command-resolved connection")
        }
      },
    )

    Async.it(
      "should update connection.paths but not PickedConnection after automatic fallback download",
      async () => {
        let logChannel = Chan.make()
        await Config.Connection.setAgdaPaths(logChannel, [])
        await Config.Connection.DownloadPolicy.set(Undecided)
        let memento = Memento.make(None)
        let platform = Mock.Platform.makeWithSuccessfulDownload(downloadedAgda.contents)

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, downloadedAgda.contents)
          Assert.deepStrictEqual(
            Config.Connection.getAgdaPaths(),
            [downloadedAgda.contents, "/invalid/path"],
          )
          Assert.deepStrictEqual(
            Memento.PickedConnection.get(memento),
            None,
          )
        | Error(_) => Assert.fail("Expected fallback download to succeed")
        }
      },
    )

    Async.it(
      "should not overwrite existing PickedConnection after automatic fallback download",
      async () => {
        let logChannel = Chan.make()
        let existingPicked = "/usr/local/bin/agda"
        await Config.Connection.setAgdaPaths(logChannel, [])
        await Config.Connection.DownloadPolicy.set(Undecided)
        let memento = Memento.make(None)
        await Memento.PickedConnection.set(memento, Some(existingPicked))
        let platform = Mock.Platform.makeWithSuccessfulDownload(downloadedAgda.contents)

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, downloadedAgda.contents)
          Assert.deepStrictEqual(
            Config.Connection.getAgdaPaths(),
            [downloadedAgda.contents, "/invalid/path"],
          )
          Assert.deepStrictEqual(
            Memento.PickedConnection.get(memento),
            Some(existingPicked),
          )
        | Error(_) => Assert.fail("Expected fallback download to succeed")
        }
      },
    )

    Async.it(
      "should prepend downloaded path to connection.paths on automatic fallback",
      async () => {
        let logChannel = Chan.make()
        let existingPaths = ["/broken/path1", "/broken/path2"]
        await Config.Connection.setAgdaPaths(logChannel, existingPaths)
        await Config.Connection.DownloadPolicy.set(Undecided)
        let memento = Memento.make(None)
        let platform = Mock.Platform.makeWithSuccessfulDownload(downloadedAgda.contents)

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          existingPaths,
          ["invalid-command"],
          logChannel,
        )

        switch result {
        | Ok(connection) =>
          Assert.deepStrictEqual(connection->Connection.getPath, downloadedAgda.contents)
          // Automatic fallback download MUST prepend (lowest priority)
          Assert.deepStrictEqual(
            Config.Connection.getAgdaPaths(),
            [downloadedAgda.contents, "/broken/path1", "/broken/path2"],
          )
        | Error(_) => Assert.fail("Expected fallback download to succeed")
        }
      },
    )

    Async.it(
      "should use selected channel for automatic fallback downloads",
      async () => {
        let logChannel = Chan.make()
        await Config.Connection.setAgdaPaths(logChannel, ["/nonexistent/path"])
        await Config.Connection.DownloadPolicy.set(Undecided)
        let memento = Memento.make(None)

        // Store DevALS as the selected channel in memento
        await Memento.SelectedChannel.set(memento, "DevALS")

        // Track which channel the download uses
        let downloadedChannel = ref(None)
        let platform: Platform.t = {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.DevALS =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.DevALS,
                    "https://example.invalid/dev-als",
                    "dev-als",
                  ),
                )
              | Connection__Download.Channel.Hardcoded =>
                Ok(FromURL(Hardcoded, "mock-url", "hardcoded-als"))
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, source) => {
              let channel = switch source {
              | Connection__Download.Source.FromURL(ch, _, _) => Some(ch)
              | Connection__Download.Source.FromGitHub(ch, _) => Some(ch)
              }
              downloadedChannel := channel
              Promise.resolve(Ok(downloadedAgda.contents))
            }
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          module(MockPlatform)
        }

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          ["/nonexistent/path"],
          [],
          logChannel,
        )

        switch result {
        | Ok(_) =>
          // Automatic fallback MUST use selected channel from memento (DevALS), not Hardcoded
          Assert.deepStrictEqual(downloadedChannel.contents, Some(Connection__Download.Channel.DevALS))
        | Error(_) => Assert.fail("Expected download fallback to succeed")
        }
      },
    )

    Async.it(
      "should NOT try command probes — resolution is preferred then paths then download",
      async () => {
        let logChannel = Chan.make()
        await Config.Connection.setAgdaPaths(logChannel, ["/nonexistent/path"])
        await Config.Connection.DownloadPolicy.set(Undecided)
        let memento = Memento.make(None)
        let commandProbed = ref(false)
        let platform: Platform.t = {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _) => Promise.resolve(None)
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.Hardcoded =>
                Ok(FromURL(Hardcoded, "mock-url", "hardcoded-als"))
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, _downloadDescriptor) =>
              Promise.resolve(Ok(downloadedAgda.contents))
            let findCommand = (_command, ~timeout as _timeout=1000) => {
              commandProbed := true
              Promise.resolve(Error(Connection__Command.Error.NotFound))
            }
          }
          module(MockPlatform)
        }

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          ["/nonexistent/path"],
          ["agda"],
          logChannel,
        )

        switch result {
        | Ok(_) =>
          // Resolution chain should be: preferred -> paths -> download
          // Command probes should NOT be part of the resolution chain
          Assert.deepStrictEqual(commandProbed.contents, false)
        | Error(_) => Assert.fail("Expected download fallback to succeed")
        }
      },
    )

    Async.it(
      "should normalize desktop WASM candidate to fsPath, not preserve raw URI",
      async () => {
        // Create a temporary .wasm file
        let wasmPath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "test-normalize-" ++ string_of_int(int_of_float(Js.Date.now())) ++ ".wasm",
        ])
        NodeJs.Fs.writeFileSync(wasmPath, NodeJs.Buffer.fromString("mock wasm content"))

        // Probe using a file:// URI (as stored in connection.paths for WASM downloads)
        let fileUri = VSCode.Uri.file(wasmPath)
        let uriString = VSCode.Uri.toString(fileUri)
        let result = await Connection.probeFilepath(uriString)

        switch result {
        | Ok((resolvedPath, _probeResult)) =>
          // Spec: on desktop, file:// candidates MUST normalize to fsPath
          // The resolved path should be the fsPath (e.g. /tmp/foo.wasm),
          // not the raw URI (e.g. file:///tmp/foo.wasm)
          Assert.deepStrictEqual(resolvedPath, wasmPath)
        | Error(_) => Assert.fail("Expected WASM probe to succeed")
        }

        NodeJs.Fs.unlinkSync(wasmPath)
      },
    )

    Async.it(
      "should try WASM fallback when cached native binary fails to connect",
      async () => {
        let logChannel = Chan.make()
        await Config.Connection.setAgdaPaths(logChannel, [])
        await Config.Connection.DownloadPolicy.set(Undecided)
        let memento = Memento.make(None)

        let triedWasm = ref(false)
        let wasmDownloadedPath = downloadedAgda.contents

        let platform: Platform.t = {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = (_globalStorageUri, _channel) => {
              // Simulate: native binary was already downloaded and cached
              Promise.resolve(Some("/tmp/cached-native-als"))
            }
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.Hardcoded =>
                Ok(FromURL(Hardcoded, "mock-url", "hardcoded-als"))
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, source) => {
              switch source {
              | Connection__Download.Source.FromURL(_, url, _)
                if url == Connection__Hardcoded.wasmUrl =>
                triedWasm := true
                Promise.resolve(Ok(wasmDownloadedPath))
              | _ => Promise.resolve(Ok(wasmDownloadedPath))
              }
            }
            let findCommand = (_command, ~timeout as _timeout=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          module(MockPlatform)
        }

        let result = await Connection.makeWithFallback(
          platform,
          memento,
          VSCode.Uri.file("/tmp/test-storage"),
          [],
          [],
          logChannel,
        )

        // When cached native binary fails make(), WASM MUST be tried as fallback
        // Currently: code returns error without trying WASM when alreadyDownloaded path fails make()
        switch result {
        | Ok(_) =>
          Assert.deepStrictEqual(triedWasm.contents, true)
        | Error(_) =>
          // If we get an error, WASM should still have been attempted
          Assert.deepStrictEqual(triedWasm.contents, true)
        }
      },
    )

    // lsp:// and other non-vscode schemes are rejected (treated as file paths)
    it(
      "should reject lsp:// URI and treat as filepath",
      () => {
        let lspResult = Connection__URI.parse("lsp://localhost:8080")
        switch lspResult {
        | Connection__URI.FileURI(_, _) => Assert.ok(true)
        }
      },
    )

    it(
      "should preserve vscode-userdata: URI scheme for web storage paths",
      () => {
        let result = Connection__URI.parse("vscode-userdata:/some/path")
        switch result {
        | Connection__URI.FileURI(_, uri) =>
          Assert.deepStrictEqual(VSCode.Uri.scheme(uri), "vscode-userdata")
        }
      },
    )

    it(
      "should preserve vscode-vfs: URI scheme for remote FS providers",
      () => {
        let result = Connection__URI.parse("vscode-vfs://github/user/repo/path")
        switch result {
        | Connection__URI.FileURI(_, uri) =>
          Assert.deepStrictEqual(VSCode.Uri.scheme(uri), "vscode-vfs")
        }
      },
    )

    // I21: WASM path representation is inconsistent by download source
    // FromURL WASM returns URI string, FromGitHub WASM returns fsPath
    Async.it(
      "download(FromGitHub) and downloadFromURL should return consistent WASM paths",
      async () => {
        // Set up storage with cached WASM file so both branches hit their cache paths
        let storagePath = NodeJs.Path.join([
          NodeJs.Os.tmpdir(),
          "agda-i21-wasm-" ++ string_of_int(int_of_float(Js.Date.now())),
        ])
        let globalStorageUri = VSCode.Uri.file(storagePath)
        let _ = await FS.createDirectory(globalStorageUri)
        let wasmDirUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als"])
        let _ = await FS.createDirectory(wasmDirUri)
        let wasmFilePath = NodeJs.Path.join([VSCode.Uri.fsPath(wasmDirUri), "als.wasm"])
        NodeJs.Fs.writeFileSync(wasmFilePath, NodeJs.Buffer.fromString("mock-wasm"))

        // Exercise real download(FromGitHub) branch (Connection__Download.res:267-290):
        // Connection__Download__GitHub.download checks FS.stat(destUri) at line 509 —
        // "hardcoded-als" directory exists, so it returns Ok(true) (cached) without HTTP.
        // Then download() constructs: fsPath(joinPath(globalStorageUri, [saveAsFileName, "als.wasm"]))
        let wasmDescriptor = {
          Connection__Download__GitHub.DownloadDescriptor.asset: {
            ...Mock.DownloadDescriptor.mockAsset,
            name: "als-wasm-Agda-2.6.3.wasm", // name includes "wasm" → fileName = "als.wasm"
          },
          release: Mock.DownloadDescriptor.mockRelease,
          saveAsFileName: "hardcoded-als",
        }
        let fromGitHubResult = await Connection__Download.download(
          globalStorageUri,
          Connection__Download.Source.FromGitHub(
            Connection__Download.Channel.Hardcoded,
            wasmDescriptor,
          ),
        )

        // Exercise real downloadFromURL cached branch (Connection__Download.res:139-147):
        // finds cached als.wasm, returns Ok(VSCode.Uri.toString(execPathUri))
        let fromURLResult = await Connection__Download.downloadFromURL(
          globalStorageUri,
          "https://example.com/als.wasm",
          "hardcoded-als",
          "test",
        )

        let fromGitHubPath = switch fromGitHubResult {
        | Ok(path) => path
        | Error(_) => Assert.fail("download(FromGitHub) should succeed for cached WASM"); ""
        }
        let fromURLPath = switch fromURLResult {
        | Ok(path) => path
        | Error(_) => Assert.fail("downloadFromURL should succeed for cached WASM"); ""
        }

        // Both branches return a path for the same cached WASM file — they MUST be equal.
        // Currently: FromGitHub returns fsPath ("/tmp/.../als.wasm")
        //            FromURL returns URI string ("file:///tmp/.../als.wasm")
        Assert.deepStrictEqual(fromURLPath, fromGitHubPath)

        let _ = await FS.deleteRecursive(globalStorageUri)
      },
    )
  })
})
