module Error = Connection__Error
module Agda = Connection__Endpoint__Agda
module ALS = Connection__Endpoint__ALS
module Endpoint = Connection__Endpoint
module URI = Connection__URI

module type Module = {
  type t =
    | Agda(Agda.t, string, string) // connection, path, version
    | ALS(ALS.t, string, string, string) // connection, path, ALS version, Agda version

  // lifecycle
  let make: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    array<string>,
    array<string>,
    Chan.t<Log.t>,
  ) => promise<result<t, Error.t>>
  let destroy: (option<t>, Chan.t<Log.t>) => promise<result<unit, Error.t>>

  let fromPaths: (Platform.t, array<string>) => promise<result<t, Error.Construction.t>>

  let fromCommands: (Platform.t, array<string>) => promise<result<t, Error.Construction.t>>

  let fromPathsAndCommands: (
    Platform.t,
    Memento.t,
    array<string>,
    array<string>,
  ) => promise<result<t, Error.Construction.t>>

  let fromDownloads: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
  ) => promise<result<t, Error.Construction.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<unit, Error.t>>

  // utility
  let checkForPrebuiltDataDirectory: string => promise<option<string>>
}

module Module: Module = {
  module InitOptions = {
    type t = {commandLineOptions: array<string>}

    let encode = ({commandLineOptions}) => {
      open JsonCombinators.Json.Encode
      Unsafe.object({
        "commandLineOptions": array(string)(commandLineOptions),
      })
    }

    let getFromConfig = () =>
      {
        commandLineOptions: Config.Connection.getCommandLineOptions(),
      }->encode
  }

  // internal state singleton
  type t =
    | Agda(Agda.t, string, string) // connection, path, version
    | ALS(ALS.t, string, string, string) // connection, path, ALS version, Agda version

  let destroy = async (connection, logChannel: Chan.t<Log.t>) =>
    switch connection {
    | None => Ok()
    | Some(connection) =>
      // Log disconnection before destroying
      switch connection {
      | Agda(_, path, _) => logChannel->Chan.emit(Log.Connection(Disconnected(path)))
      | ALS(_, path, _, _) => logChannel->Chan.emit(Log.Connection(Disconnected(path)))
      }

      switch connection {
      | Agda(conn, _, _) =>
        await Agda.destroy(conn)
        Ok()
      | ALS(conn, _, _, _) =>
        switch await ALS.destroy(conn) {
        | Error(error) => Error(Error.ALS(error))
        | Ok(_) => Ok()
        }
      }
    }

  let getPath = connection =>
    switch connection {
    | Agda(_, path, _) => path
    | ALS(_, path, _, _) => path
    }

  let makeWithRawPath = async (rawpath: string): result<t, Error.Construction.t> => {
    let uri = URI.parse(rawpath)
    switch uri {
    | URI.LspURI(_) => Error(Error.Construction.fromEndpointError(rawpath, CannotHandleURLsATM))
    | FileURI(_, uri) =>
      let path = VSCode.Uri.fsPath(uri)
      let result = await Connection__Process__Exec.run(path, ["--version"])
      switch result {
      | Ok(output) =>
        // try Agda
        switch String.match(output, %re("/Agda version (.*)/")) {
        | Some([_, Some(version)]) =>
          switch await Agda.make(path, version) {
          | Error(error) =>
            Error(Error.Construction.fromEndpointError(path, CannotMakeConnectionWithAgda(error)))
          | Ok(conn) => Ok(Agda(conn, path, version))
          }
        | _ =>
          // try ALS
          switch String.match(output, %re("/Agda v(.*) Language Server v(.*)/")) {
          | Some([_, Some(agdaVersion), Some(alsVersion)]) =>
            let lspOptions = switch await Connection__Endpoint.checkForPrebuiltDataDirectory(path) {
            | Some(assetPath) =>
              let env = Dict.fromArray([("Agda_datadir", assetPath)])
              Some({Connection__Endpoint__Protocol__LSP__Binding.env: env})
            | None => None
            }
            switch await ALS.make(
              Connection__Transport.ViaPipe(path, []),
              lspOptions,
              InitOptions.getFromConfig(),
            ) {
            | Error(error) =>
              Error(Error.Construction.fromEndpointError(path, CannotMakeConnectionWithALS(error)))
            | Ok(conn) => Ok(ALS(conn, path, alsVersion, agdaVersion))
            }
          | _ =>
            Error(
              Error.Construction.fromEndpointError(
                path,
                Connection__Endpoint__Error.NotAgdaOrALS(output),
              ),
            )
          }
        }
      | Error(error) =>
        Error(Error.Construction.fromEndpointError(path, CannotDetermineAgdaOrALS(error)))
      }
    }
  }

  // Combinator: try each item in array until one succeeds, or return all failures
  let tryUntilSuccess = async (fn: 'a => promise<result<'b, 'e>>, items: array<'a>) => {
    let rec loop = async (remaining, errors) => {
      switch remaining[0] {
      | Some(item) =>
        switch await fn(item) {
        | Ok(success) => Ok(success)
        | Error(error) =>
          let newErrors = errors->Array.concat([error])
          let nextItems = remaining->Array.sliceToEnd(~start=1)
          await loop(nextItems, newErrors)
        }
      | None => Error(errors)
      }
    }
    await loop(items, [])
  }

  // Make a connection by trying all given paths in order
  let fromPaths = async (platformDeps: Platform.t, paths: array<string>): result<
    t,
    Error.Construction.t,
  > => {
    module PlatformOps = unpack(platformDeps)

    switch await tryUntilSuccess(makeWithRawPath, paths) {
    | Ok(connection) => Ok(connection)
    | Error(pathErrors) => Error(Error.Construction.mergeMany(pathErrors))
    }
  }

  // Make a connection from commands, trying each command until one succeeds
  let fromCommands = async (platformDeps: Platform.t, commands: array<string>): result<
    t,
    Error.Construction.t,
  > => {
    module PlatformOps = unpack(platformDeps)

    let tryCommand = async (command): result<t, Error.Construction.t> => {
      switch await PlatformOps.findCommand(command) {
      | Ok(rawPath) => await makeWithRawPath(rawPath)
      | Error(commandError) => Error(Error.Construction.fromCommandError(command, commandError))
      }
    }

    switch await tryUntilSuccess(tryCommand, commands) {
    | Ok(connection) => Ok(connection)
    | Error(errors) =>
      // merge all errors from `tryCommand`
      Error(errors->Error.Construction.mergeMany)
    }
  }

  // Make a connection using raw paths and commands, by trying in order:
  //  1. Previously picked path (if it exists in the supplied paths)
  //  2. Each path from the paths array sequentially
  //  3. Each command from the commands array sequentially
  // Returns Ok(connection) on first success, or Error with all collected failures
  let fromPathsAndCommands = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    paths: array<string>,
    commands: array<string>,
  ): result<t, Error.Construction.t> => {
    module PlatformOps = unpack(platformDeps)

    let tryCommand = async (command): result<t, Error.Construction.t> => {
      switch await PlatformOps.findCommand(command) {
      | Ok(rawPath) => await makeWithRawPath(rawPath)
      | Error(commandError) => Error(Error.Construction.fromCommandError(command, commandError))
      }
    }

    // Build the complete list of things to try in order
    let allPaths = switch Memento.PickedConnection.get(memento) {
    | Some(pickedPath) => [pickedPath]->Array.concat(paths)
    | None => paths
    }

    // Step 1 & 2: Try picked path first, then all settings paths
    switch await tryUntilSuccess(makeWithRawPath, allPaths) {
    | Ok(connection) => Ok(connection)
    | Error(pathErrors) =>
      // Step 3: Try all commands
      switch await tryUntilSuccess(tryCommand, commands) {
      | Ok(connection) => Ok(connection)
      | Error(commandErrors) =>
        Error(Error.Construction.mergeMany([...pathErrors, ...commandErrors]))
      }
    }
  }

  // Try to download ALS and create connection directly, with the following steps:
  // 1. Check platform support
  // 2. Check download policy
  // 3. Check if already downloaded or download latest ALS
  // 4. Create connection directly from downloaded raw path
  // Returns Ok(connection) on success, or Error with failure details
  let fromDownloads = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
  ): result<t, Error.Construction.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) => Error(Error.Construction.fromDownloadError(PlatformNotSupported(platform)))
    | Ok(platform) =>
      // Check download policy
      let policy = switch Config.Connection.DownloadPolicy.get() {
      | Undecided => await PlatformOps.askUserAboutDownloadPolicy()
      | policy => policy
      }

      switch policy {
      | Config.Connection.DownloadPolicy.Undecided =>
        // User cancelled, treat as No
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Construction.make())
      | No =>
        // User opted not to download, set policy and return error
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Construction.make())
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        // Get the downloaded path, download if not already done
        let downloadResult = switch await PlatformOps.alreadyDownloaded(globalStorageUri)() {
        | Some(path) => Ok(path)
        | None =>
          switch await PlatformOps.downloadLatestALS(memento, globalStorageUri)(platform) {
          | Error(error) => Error(Error.Construction.fromDownloadError(error))
          | Ok(path) => Ok(path)
          }
        }

        switch downloadResult {
        | Ok(path) =>
          switch await makeWithRawPath(path) {
          | Ok(connection) =>
            await Config.Connection.addAgdaPath(getPath(connection))
            await Memento.PickedConnection.set(memento, Some(getPath(connection)))
            Ok(connection)
          | Error(error) => Error(error)
          }
        | Error(error) => Error(error)
        }
      }
    }
  }

  // Make a connection to Agda or ALS, by trying:
  //  1. The previously picked raw path if it exists in settings
  //  2. Each raw path from the settings sequentially
  //  3. Each command from the commands array sequentially
  //  4. Downloading the latest ALS
  let make = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    paths: array<string>,
    commands: array<string>,
    logChannel: Chan.t<Log.t>,
  ): result<t, Error.t> => {
    let logConnection = connection => {
      switch connection {
      | Agda(_, path, version) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToAgda(path, version)))
      | ALS(_, path, alsVersion, agdaVersion) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, alsVersion, agdaVersion)))
      }
    }

    let pathsWithSelectedConnection = switch Memento.PickedConnection.get(memento) {
    | Some(pickedPath) => [pickedPath]->Array.concat(paths)
    | None => paths
    }

    switch await fromPaths(platformDeps, pathsWithSelectedConnection) {
    | Ok(connection) =>
      logConnection(connection)
      await Config.Connection.addAgdaPath(getPath(connection))
      await Memento.PickedConnection.set(memento, Some(getPath(connection)))
      Ok(connection)
    | Error(fromPathsErrors) =>
      switch await fromCommands(platformDeps, commands) {
      | Ok(connection) =>
        logConnection(connection)
        await Config.Connection.addAgdaPath(getPath(connection))
        await Memento.PickedConnection.set(memento, Some(getPath(connection)))
        Ok(connection)
      | Error(fromCommandsErrors) =>
        switch await fromDownloads(platformDeps, memento, globalStorageUri) {
        | Ok(connection) =>
          logConnection(connection)
          await Config.Connection.addAgdaPath(getPath(connection))
          await Memento.PickedConnection.set(memento, Some(getPath(connection)))
          Ok(connection)
        | Error(fromDownloadsErrors) =>
          Error(
            Construction(
              Error.Construction.mergeMany([
                fromPathsErrors,
                fromCommandsErrors,
                fromDownloadsErrors,
              ]),
            ),
          )
        }
      }
    }
  }

  let sendRequest = async (connection, document, request, handler) => {
    // encode the Request to some string
    let encodeRequest = (document, version) => {
      let filepath = document->VSCode.TextDocument.fileName->Parser.filepath
      let libraryPath = Config.getLibraryPath()
      let highlightingMethod = Config.Highlighting.getHighlightingMethod()
      let backend = Config.getBackend()
      Request.encode(document, version, filepath, backend, libraryPath, highlightingMethod, request)
    }

    switch connection {
    | ALS(conn, path, alsVersion, agdaVersion) =>
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.agdaVersion), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(ALS(conn, path, alsVersion, agdaVersion)), Chan.make())
        Error(Error.ALS(error))
      | Ok() => Ok()
      }

    | Agda(conn, path, version) =>
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(Agda(conn, path, version)), Chan.make())
        Error(Error.Agda(error))
      | Ok() => Ok()
      }
    }
  }

  // Helper function to check for prebuilt data directory
  let checkForPrebuiltDataDirectory = async (executablePath: string) => {
    // the executable needs to be accompanied by a `data` directory
    // which can be specified by the environment variable "Agda_datadir"
    // prebuilt executables on GitHub have this directory placed alongside the executable
    let prebuildDataDirPath = NodeJs.Path.join([executablePath, "..", "data"])
    let prebuildDataDirURI = VSCode.Uri.file(prebuildDataDirPath)

    switch await FS.stat(prebuildDataDirURI) {
    | Ok(_) => Some(NodeJs.Path.join([executablePath, "..", "data"]))
    | Error(_) => None
    }
  }
}

include Module
