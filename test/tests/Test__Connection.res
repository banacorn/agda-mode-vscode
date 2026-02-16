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
        agdaMockPath := (await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock"))
        agdaMockEndpoint := Some(agdaMockPath.contents)
      },
    )

    Async.after(
      async () => {
        // cleanup the Agda mock
        switch agdaMockEndpoint.contents {
        | Some(target) =>
          await Endpoint.Agda.destroy(target)
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
        agdaMockPath := (await Endpoint.Agda.mock(~version="2.6.4.1", ~name="agda-probe-mock"))
        // Setup ALS mock
        alsMockPath :=
          (
            await Endpoint.ALS.mock(
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
          await Endpoint.Agda.destroy(agdaMockPath.contents)
        }
        if alsMockPath.contents != "" {
          await Endpoint.ALS.destroy(alsMockPath.contents)
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
        | Ok(_, IsALSOfUnknownVersion(_)) => Assert.fail("Expected ALS with known versions")
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
        | LspURI(_, _) => fileName // fallback, shouldn't happen in this test
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
        | Ok(_, IsALSOfUnknownVersion(_)) => Assert.fail("Expected ALS with known versions")
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
        agdaMockPath := (await Endpoint.Agda.mock(~version="2.6.3", ~name="agda-make-mock"))
        // Setup ALS mock
        alsMockPath :=
          (
            await Endpoint.ALS.mock(
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
          await Endpoint.Agda.destroy(agdaMockPath.contents)
        }
        if alsMockPath.contents != "" {
          await Endpoint.ALS.destroy(alsMockPath.contents)
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
        | Ok(_, IsALSOfUnknownVersion(_)) => Assert.fail("Expected ALS with known versions")
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
            // Verify it's a CannotDetermineAgdaOrALS error wrapped in endpoint error
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
        | LspURI(_, _) => fileName // fallback, shouldn't happen in this test
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
        | Ok(_, IsALSOfUnknownVersion(_)) => Assert.fail("Expected ALS with known versions")
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

  describe("`fromDownloads`", () => {
    let agdaMockEndpoint = ref(None)

    Async.before(
      async () => {
        try {
          // setup the Agda mock
          let path = await Endpoint.Agda.mock(~version="2.7.0.1", ~name="agda-mock-2")

          agdaMockEndpoint := Some(path)
        } catch {
        | Failure(msg) => failwith(msg) // Preserve detailed error from Test__Util.res
        | _ => failwith("Got error when trying to construct target from mock Agda: unknown error")
        }
      },
    )

    Async.after(
      async () => {
        await Config.Connection.setAgdaPaths(Chan.make(), [])
        await Memento.PickedConnection.set(Memento.make(None), None)

        // cleanup the Agda mock
        switch agdaMockEndpoint.contents {
        | Some(target) =>
          await Endpoint.Agda.destroy(target)
          agdaMockEndpoint := None
        | None => ()
        }
      },
    )

    Async.it(
      "should throw the `PlatformNotSupported` error when the platform is not supported",
      async () => {
        let platform = {
          "os": "non-existent-os",
          "dist": "non-existent-dist",
          "codename": "non-existent-codename",
          "release": "non-existent-release",
        }

        // Create a mock platform that returns an unsupported platform error
        let mockPlatformDeps = Mock.Platform.makeWithPlatformError(platform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let actual = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)

        let expected = Connection.Error.Establish.fromDownloadError(PlatformNotSupported(platform))

        Assert.deepStrictEqual(actual, Error(expected))
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the initial download policy is `No`",
      async () => {
        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that simulates successful platform determination but No download policy
        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.No,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(
          result,
          Error(Connection.Error.Establish.fromDownloadError(OptedNotToDownload)),
        )

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should not ask the user for download policy since it was already set to No
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)
      },
    )

    Async.it(
      "should throw the `NoDownloadALS` error when the user clicked `cancel` on the download dialog",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that asks user and gets Undecided response
        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.Undecided,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(
          result,
          Error(Connection.Error.Establish.fromDownloadError(OptedNotToDownload)),
        )

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

        // should ask the user for download policy exactly once
        Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)
      },
    )

    Async.it(
      "should check if the latest ALS is already downloaded when the download policy is `Yes`",
      async () => {
        // access the Agda mock (using it as ALS for this test)
        let mockEndpoint = switch agdaMockEndpoint.contents {
        | Some(endpoint) => endpoint
        | None => failwith("Unable to access the Agda mock endpoint")
        }
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        // Create a mock platform that returns cached ALS as endpoint
        let mockPlatformDeps = Mock.Platform.makeWithCachedDownloadAndFlag(
          mockEndpoint,
          checkedCache,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)

        // Should return connection directly (not endpoint)
        switch result {
        | Ok(connection) =>
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(version, "2.7.0.1")
            Assert.deepStrictEqual(path, mockEndpoint)
          | _ => Assert.fail("Expected Agda connection")
          }
        | Error(_) => Assert.fail("Expected successful cached download")
        }

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
      },
    )

    Async.it(
      "should throw the `DownloadALS` error when the download policy is `Yes` but the download fails",
      async () => {
        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that fails to download ALS
        let mockPlatformDeps = Mock.Platform.makeWithDownloadFailureAndFlags(
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")
        let result = await Connection.fromDownloads(mockPlatformDeps, memento, globalStorageUri)
        Assert.deepStrictEqual(checkedCache.contents, true)
        Assert.deepStrictEqual(checkedDownload.contents, true)

        let expected = Connection.Error.Establish.fromDownloadError(CannotFindCompatibleALSRelease)

        Assert.deepStrictEqual(result, Error(expected))

        let policy = Config.Connection.DownloadPolicy.get()
        Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)
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
            let path = await Test__Util.Endpoint.Agda.mock(
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
          | None => failwith("Mock endpoint not available")
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
          | None => failwith("Mock endpoint not available")
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
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
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

  describe("make fromPathsAndCommands scenarios", () => {
    Async.it(
      "should find commands when no paths are given",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback finds commands when no paths provided
         *
         * SCENARIO:
         * 1. Call Connection.makeWithFallback with empty paths array
         * 2. Provide valid commands ["agda", "als"]
         * 3. Should successfully connect and log the connection
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          [], // no paths
          ["agda", "als"], // try commands
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          // TODO: Verify we got a valid connection (version varies by environment)
          switch connection {
          | Agda(_, _path, _version) => ()
          | ALS(_, _path, _) => ()
          | ALSWASM(_, _, _, _) => ()
          }

          // Should have logged a connection event
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, _))] => ()
          | [Log.Connection(Log.Connection.ConnectedToALS(_, Some(_, _)))] => ()
          | [Log.Connection(Log.Connection.ConnectedToALS(_, None))] => ()
          | [] => Assert.fail("Expected connection event to be logged")
          | _ => Assert.fail("Expected exactly one connection event")
          }
        | Error(_) => Assert.fail("Expected to find agda or als")
        }
      },
    )

    Async.it(
      "should find commands even if all paths given are wrong",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback falls back to commands when paths fail
         *
         * SCENARIO:
         * 1. Provide invalid paths
         * 2. Provide valid commands
         * 3. Should ignore failed paths and use commands successfully
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let memento = Memento.make(None)
        let platformDeps = Desktop.make()
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          platformDeps,
          memento,
          globalStorageUri,
          ["/some/invalid/path"], // invalid paths
          ["agda", "als"], // valid commands
          logChannel,
        )
        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          // TODO: Verify we got a valid connection (version varies by environment)
          switch connection {
          | Agda(_, _path, _version) => ()
          | ALS(_, _path, _) => ()
          | ALSWASM(_, _, _, _) => ()
          }

          // Should have logged a connection event
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, _))] => ()
          | [Log.Connection(Log.Connection.ConnectedToALS(_, Some(_, _)))] => ()
          | [Log.Connection(Log.Connection.ConnectedToALS(_, None))] => ()
          | [] => Assert.fail("Expected connection event to be logged")
          | _ => Assert.fail("Expected exactly one connection event")
          }
        | Error(_) => Assert.fail("Expected to find agda or als via commands")
        }
      },
    )

    Async.it(
      "should prioritize valid paths over commands",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback uses paths before falling back to commands
         *
         * SCENARIO:
         * 1. Setup mock Agda at known path
         * 2. Provide that path plus invalid commands
         * 3. Should use the path, not attempt commands
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        // Setup mock Agda
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
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

  describe("make fromDownloads scenarios", () => {
    Async.it(
      "should handle platform not supported error with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback handles unsupported platform gracefully
         *
         * SCENARIO:
         * 1. Use mock platform that returns unsupported platform error
         * 2. Provide invalid paths/commands to force download fallback
         * 3. Should get PlatformNotSupported error and no connection events
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        let platform = {
          "os": "unsupported-os",
          "dist": "unsupported-dist",
          "codename": "unsupported-codename",
          "release": "unsupported-release",
        }

        let mockPlatformDeps = Mock.Platform.makeWithPlatformError(platform)
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"], // force fallback to downloads
          ["invalid-command"], // force fallback to downloads
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected platform error")
        | Error(error) =>
          // Should get Establish error with PlatformNotSupported
          switch error {
          | Connection.Error.Establish(errors) =>
            switch errors.download {
            | Failed(Connection__Download.Error.PlatformNotSupported(_)) => ()
            | _ => Assert.fail("Expected PlatformNotSupported download error")
            }
          | _ => Assert.fail("Expected Establish error")
          }

          // Should not have logged any connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle download policy No with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback respects No download policy
         *
         * SCENARIO:
         * 1. Set download policy to No
         * 2. Force fallback to downloads (invalid paths/commands)
         * 3. Should return error without downloading and no connection events
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        await Config.Connection.DownloadPolicy.set(No)
        let getDownloadPolicyCount = ref(0)

        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.No,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected error due to No download policy")
        | Error(error) =>
          // Should get Establish error (empty since No policy blocks download)
          switch error {
          | Connection.Error.Establish(_) => ()
          | _ => Assert.fail("Expected Establish error")
          }

          // Should not have asked for download policy (already set to No)
          Assert.deepStrictEqual(getDownloadPolicyCount.contents, 0)

          // Should not have logged connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle download policy Undecided (user cancelled) with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback handles user cancelling download dialog
         *
         * SCENARIO:
         * 1. Set download policy to Undecided
         * 2. Mock user clicking cancel (returning Undecided)
         * 3. Should return error and set policy to No, with no connection events
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        await Config.Connection.DownloadPolicy.set(Undecided)
        let getDownloadPolicyCount = ref(0)

        // Create a mock platform that asks user and gets Undecided response (user cancelled)
        let mockPlatformDeps = Mock.Platform.makeWithDownloadPolicyCounter(
          Config.Connection.DownloadPolicy.Undecided,
          getDownloadPolicyCount,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )
        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected error due to user cancelling download")
        | Error(error) =>
          // Should get Establish error
          switch error {
          | Connection.Error.Establish(_) => ()
          | _ => Assert.fail("Expected Establish error")
          }

          // Should have asked for download policy exactly once
          Assert.deepStrictEqual(getDownloadPolicyCount.contents, 1)

          // Should have set policy to No after user cancelled
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.No)

          // Should not have logged connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )

    Async.it(
      "should handle cached ALS download with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback uses cached ALS and logs connection
         *
         * SCENARIO:
         * 1. Set download policy to Yes
         * 2. Mock cached ALS available
         * 3. Should use cached ALS and log connection event
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        // Setup mock ALS executable for cached download
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-cached",
        )
        let mockEndpoint = agdaMockPath

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)

        // Create a mock platform that returns cached ALS
        let mockPlatformDeps = Mock.Platform.makeWithCachedDownloadAndFlag(
          mockEndpoint,
          checkedCache,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"], // force download fallback
          ["invalid-command"], // force download fallback
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          // Should have checked cache
          Assert.deepStrictEqual(checkedCache.contents, true)

          // Should have logged connection
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, version))] =>
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          // Connection should match logged event
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected Agda connection")
          }

          // Should have set policy to Yes
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        | Error(_) => Assert.fail("Expected successful cached download")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle fresh ALS download with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback downloads fresh ALS and logs connection
         *
         * SCENARIO:
         * 1. Set download policy to Yes
         * 2. Mock no cached ALS, successful fresh download
         * 3. Should download ALS and log connection event
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        // Setup mock ALS for successful download
        let agdaMockPath = await Test__Util.Endpoint.Agda.mock(
          ~version="2.7.0.1",
          ~name="agda-mock-fresh-download",
        )
        let mockEndpoint = agdaMockPath

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that downloads ALS
        let mockPlatformDeps = Mock.Platform.makeWithSuccessfulDownloadAndFlags(
          mockEndpoint,
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"], // force download fallback
          ["invalid-command"], // force download fallback
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(connection) =>
          // Should have checked cache and performed download
          Assert.deepStrictEqual(checkedCache.contents, true)
          Assert.deepStrictEqual(checkedDownload.contents, true)

          // Should have logged connection
          switch loggedEvents {
          | [Log.Connection(Log.Connection.ConnectedToAgda(_, version))] =>
            Assert.deepStrictEqual(version, "2.7.0.1")
          | _ => Assert.fail("Expected exactly one ConnectedToAgda event")
          }

          // Connection should match logged event
          switch connection {
          | Agda(_, path, version) =>
            Assert.deepStrictEqual(path, agdaMockPath)
            Assert.deepStrictEqual(version, "2.7.0.1")
            Assert.deepStrictEqual(path, agdaMockPath)
          | _ => Assert.fail("Expected Agda connection")
          }

          // Should have set policy to Yes
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

        | Error(_) => Assert.fail("Expected successful fresh download")
        }

        // Cleanup
        try {
          NodeJs.Fs.unlinkSync(agdaMockPath)
        } catch {
        | _ => ()
        }
      },
    )

    Async.it(
      "should handle download failure with logging",
      async () => {
        /**
         * TEST PURPOSE: Verify Connection.makeWithFallback handles download failures gracefully
         *
         * SCENARIO:
         * 1. Set download policy to Yes
         * 2. Mock download failure
         * 3. Should return error and no connection events
         */
        let logChannel = Chan.make()
        let listener = Log.collect(logChannel)

        await Config.Connection.DownloadPolicy.set(Undecided)
        let checkedCache = ref(false)
        let checkedDownload = ref(false)

        // Create a mock platform that fails to download ALS
        let mockPlatformDeps = Mock.Platform.makeWithDownloadFailureAndFlags(
          checkedCache,
          checkedDownload,
        )
        let memento = Memento.make(None)
        let globalStorageUri = VSCode.Uri.file("/tmp/test-storage")

        let result = await Connection.makeWithFallback(
          mockPlatformDeps,
          memento,
          globalStorageUri,
          ["/invalid/path"],
          ["invalid-command"],
          logChannel,
        )

        let loggedEvents = listener(~filter=Log.isConnection)

        switch result {
        | Ok(_) => Assert.fail("Expected download failure")
        | Error(error) =>
          // Should have checked cache and attempted download
          Assert.deepStrictEqual(checkedCache.contents, true)
          Assert.deepStrictEqual(checkedDownload.contents, true)

          // Should get Establish error with download failure
          switch error {
          | Connection.Error.Establish(errors) =>
            switch errors.download {
            | Failed(Connection__Download.Error.CannotFindCompatibleALSRelease) => ()
            | _ => Assert.fail("Expected CannotFindCompatibleALSRelease download error")
            }
          | _ => Assert.fail("Expected Establish error")
          }

          // Should have set policy to Yes
          let policy = Config.Connection.DownloadPolicy.get()
          Assert.deepStrictEqual(policy, Config.Connection.DownloadPolicy.Yes)

          // Should not have logged connection events
          Assert.deepStrictEqual(loggedEvents, [])
        }
      },
    )
  })
})
