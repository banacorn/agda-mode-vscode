module Error = Connection__Error
module Agda = Connection__Endpoint__Agda
module ALS = Connection__Endpoint__ALS
module URI = Connection__URI

module type Module = {
  type agdaVersion = string // Agda version
  type alsVersion = (string, string, option<Connection__Protocol__LSP__Binding.executableOptions>) // (ALS version, Agda version, LSP options)
  type t =
    | Agda(Agda.t, string, agdaVersion) // connection, path
    | ALS(ALS.t, string, alsVersion) // connection, path

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

  let fromPaths: (Platform.t, array<string>) => promise<result<t, Error.Establish.t>>

  let fromCommands: (Platform.t, array<string>) => promise<result<t, Error.Establish.t>>

  let fromDownloads: (Platform.t, Memento.t, VSCode.Uri.t) => promise<result<t, Error.Establish.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<unit, Error.t>>

  // utility
  let checkForPrebuiltDataDirectory: string => promise<option<string>>
  let probeFilepath: string => promise<
    result<(string, result<agdaVersion, alsVersion>), Connection__Endpoint__Error.t>,
  >
  let makeWithRawPath: string => promise<result<t, Error.Establish.t>>
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

  type agdaVersion = string // Agda version
  type alsVersion = (string, string, option<Connection__Protocol__LSP__Binding.executableOptions>) // (ALS version, Agda version, LSP options)
  type t =
    | Agda(Agda.t, string, agdaVersion) // connection, path
    | ALS(ALS.t, string, alsVersion) // connection, path

  let destroy = async (connection, logChannel: Chan.t<Log.t>) =>
    switch connection {
    | None => Ok()
    | Some(connection) =>
      // Log disconnection before destroying
      switch connection {
      | Agda(_, path, _) => logChannel->Chan.emit(Log.Connection(Disconnected(path)))
      | ALS(_, path, _) => logChannel->Chan.emit(Log.Connection(Disconnected(path)))
      }

      switch connection {
      | Agda(conn, _, _) =>
        await Agda.destroy(conn)
        Ok()
      | ALS(conn, _, _) =>
        switch await ALS.destroy(conn) {
        | Error(error) => Error(Error.ALS(error))
        | Ok(_) => Ok()
        }
      }
    }

  let getPath = connection =>
    switch connection {
    | Agda(_, path, _) => path
    | ALS(_, path, _) => path
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

  // see if it's a Agda executable or a language server
  let probeFilepath = async path =>
    switch URI.parse(path) {
    | Connection__URI.LspURI(_) => Error(Connection__Endpoint__Error.CannotHandleURLsAtTheMoment)
    | FileURI(_, vscodeUri) =>
      let path = VSCode.Uri.fsPath(vscodeUri)
      let result = await Connection__Process__Exec.run(path, ["--version"])
      switch result {
      | Ok(output) =>
        // try Agda
        switch String.match(output, %re("/Agda version (.*)/")) {
        | Some([_, Some(version)]) => Ok(path, Ok(version))
        | _ =>
          // try ALS
          switch String.match(output, %re("/Agda v(.*) Language Server v(.*)/")) {
          | Some([_, Some(agdaVersion), Some(alsVersion)]) =>
            let lspOptions = switch await checkForPrebuiltDataDirectory(path) {
            | Some(assetPath) =>
              let env = Dict.fromArray([("Agda_datadir", assetPath)])
              Some({Connection__Protocol__LSP__Binding.env: env})
            | None => None
            }
            Ok(path, Error(alsVersion, agdaVersion, lspOptions))
          | _ => Error(Connection__Endpoint__Error.NotAgdaOrALS(output))
          }
        }
      | Error(error) => Error(Connection__Endpoint__Error.CannotDetermineAgdaOrALS(error))
      }
    }

  let makeWithRawPath = async (rawpath: string): result<t, Error.Establish.t> => {
    switch await probeFilepath(rawpath) {
    | Ok(path, Ok(agdaVersion)) =>
      let connection = await Agda.make(rawpath, agdaVersion)
      Ok(Agda(connection, path, agdaVersion))
    | Ok(path, Error(alsVersion, agdaVersion, lspOptions)) =>
      switch await ALS.make(
        Connection__Transport.ViaPipe(rawpath, []),
        lspOptions,
        InitOptions.getFromConfig(),
      ) {
      | Error(error) =>
        Error(Error.Establish.fromEndpointError(path, CannotMakeConnectionWithALS(error)))
      | Ok(conn) => Ok(ALS(conn, path, (alsVersion, agdaVersion, lspOptions)))
      }
    | Error(error) => Error(Error.Establish.fromEndpointError(rawpath, error))
    }
  }

  // Combinator: try each task in array until one succeeds, or return all failures
  let tryUntilSuccess = async (xs: array<unit => promise<result<'b, 'e>>>): result<
    'b,
    array<'e>,
  > => {
    let rec loop = async (remaining, errors) => {
      switch remaining[0] {
      | Some(task) =>
        switch await task() {
        | Ok(success) => Ok(success)
        | Error(error) =>
          let newErrors = errors->Array.concat([error])
          let nextTasks = remaining->Array.sliceToEnd(~start=1)
          await loop(nextTasks, newErrors)
        }
      | None => Error(errors)
      }
    }
    await loop(xs, [])
  }

  // Make a connection by trying all given paths in order
  let fromPaths = async (platformDeps: Platform.t, paths: array<string>): result<
    t,
    Error.Establish.t,
  > => {
    module PlatformOps = unpack(platformDeps)

    let tasks = paths->Array.map(path => () => makeWithRawPath(path))
    switch await tryUntilSuccess(tasks) {
    | Ok(connection) => Ok(connection)
    | Error(pathErrors) => Error(Error.Establish.mergeMany(pathErrors))
    }
  }

  // Make a connection from commands, trying each command until one succeeds
  let fromCommands = async (platformDeps: Platform.t, commands: array<string>): result<
    t,
    Error.Establish.t,
  > => {
    module PlatformOps = unpack(platformDeps)

    let tryCommand = async (command): result<t, Error.Establish.t> => {
      switch await PlatformOps.findCommand(command) {
      | Ok(rawPath) => await makeWithRawPath(rawPath)
      | Error(commandError) => Error(Error.Establish.fromCommandError(command, commandError))
      }
    }

    let tasks = commands->Array.map(command => () => tryCommand(command))
    switch await tryUntilSuccess(tasks) {
    | Ok(connection) => Ok(connection)
    | Error(errors) =>
      // merge all errors from `tryCommand`
      Error(errors->Error.Establish.mergeMany)
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
  ): result<t, Error.Establish.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) => Error(Error.Establish.fromDownloadError(PlatformNotSupported(platform)))
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
        Error(Error.Establish.fromDownloadError(Connection__Download.Error.OptedNotToDownload))
      | No =>
        // User opted not to download, set policy and return error
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Establish.fromDownloadError(Connection__Download.Error.OptedNotToDownload))
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        // Get the downloaded path, download if not already done
        let downloadResult = switch await PlatformOps.alreadyDownloaded(globalStorageUri)() {
        | Some(path) => Ok(path)
        | None =>
          switch await PlatformOps.downloadLatestALS(memento, globalStorageUri)(platform) {
          | Error(error) => Error(Error.Establish.fromDownloadError(error))
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
      | ALS(_, path, (alsVersion, agdaVersion, _lspOptions)) =>
        logChannel->Chan.emit(Log.Connection(ConnectedToALS(path, alsVersion, agdaVersion)))
      }
    }

    let pathsWithSelectedConnection = switch Memento.PickedConnection.get(memento) {
    | Some(pickedPath) => [pickedPath]->Array.concat(paths)
    | None => paths
    }

    // Try each method in order
    let tasks = [
      () => fromPaths(platformDeps, pathsWithSelectedConnection),
      () => fromCommands(platformDeps, commands),
      () => fromDownloads(platformDeps, memento, globalStorageUri),
    ]

    switch await tryUntilSuccess(tasks) {
    | Ok(connection) =>
      logConnection(connection)
      // Set the connection in the memento
      await Config.Connection.addAgdaPath(getPath(connection))
      // Set the picked connection in the memento
      await Memento.PickedConnection.set(memento, Some(getPath(connection)))
      Ok(connection)
    | Error(errors) =>
      // Merge all errors from `fromPaths`, `fromCommands`, and `fromDownloads`
      Error(Establish(Error.Establish.mergeMany(errors)))
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
    | ALS(conn, path, (alsVersion, agdaVersion, lspOptions)) =>
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.agdaVersion), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(
          Some(ALS(conn, path, (alsVersion, agdaVersion, lspOptions))),
          Chan.make(),
        )
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
}

include Module
