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
    Error.Construction.t,
  ) => promise<result<t, Error.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<unit, Error.t>>

  // command (uses platform dependencies)
  let findCommands: (
    Platform.t,
    array<string>,
  ) => promise<result<Endpoint.t, Dict.t<Connection__Command.Error.t>>>
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

  let makeWithRawPath = async (rawpath: string): result<t, Connection__Endpoint.Error.t> => {
    let uri = URI.parse(rawpath)
    switch uri {
    | URI.LspURI(_) => Error(CannotHandleURLsATM)
    | FileURI(_, uri) =>
      let path = VSCode.Uri.fsPath(uri)
      let result = await Connection__Process__Exec.run(path, ["--version"])
      switch result {
      | Ok(output) =>
        // try Agda
        switch String.match(output, %re("/Agda version (.*)/")) {
        | Some([_, Some(version)]) =>
          switch await Agda.make(path, version) {
          | Error(error) => Error(CannotMakeConnectionWithAgda(error))
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
            | Error(error) => Error(CannotMakeConnectionWithALS(error))
            | Ok(conn) => Ok(ALS(conn, path, alsVersion, agdaVersion))
            }
          | _ => Error(Connection__Endpoint.Error.NotAgdaOrALS(output))
          }
        }
      | Error(error) => Error(CannotDetermineAgdaOrALS(error))
      }
    }
  }

  // search through a list of commands until one is found
  let findCommands = async (platformDeps: Platform.t, commands) => {
    module PlatformOps = unpack(platformDeps)
    await PlatformOps.findCommands(commands)
  }

  // Combinator: try each item in array until one succeeds, or return all failures
  let tryUntilSuccess = async (fn: 'a => promise<result<'b, 'e>>, items: array<'a>) => {
    let rec loop = async (remaining, errors) => {
      switch remaining[0] {
      | Some(item) =>
        switch await fn(item) {
        | Ok(success) => Ok(success)
        | Error(error) =>
          let newErrors = errors->Array.concat([(item, error)])
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
    | Error(pathErrors) =>
      // Build final error with path failures
      let constructionError = {
        Error.Construction.endpoints: pathErrors->Dict.fromArray,
        commands: Dict.make(),
        download: None,
      }
      Error(constructionError)
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
      | Ok(rawPath) =>
        switch await makeWithRawPath(rawPath) {
        | Ok(connection) => Ok(connection)
        | Error(endpointError) =>
          Error(
            Error.Construction.make()->Error.Construction.addEndpointError(rawPath, endpointError),
          )
        }
      | Error(commandError) =>
        Error(Error.Construction.make()->Error.Construction.addCommandError(command, commandError))
      }
    }

    switch await tryUntilSuccess(tryCommand, commands) {
    | Ok(connection) => Ok(connection)
    | Error(errors) => Error(errors->Array.map(snd)->Error.Construction.mergeMany)
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

    let tryCommand = async (command): result<t, Connection__Command.Error.t> => {
      switch await PlatformOps.findCommand(command) {
      | Ok(rawPath) =>
        switch await makeWithRawPath(rawPath) {
        | Ok(connection) => Ok(connection)
        | Error(_endpointError) =>
          // Convert endpoint error to a command error for consistency
          Error(Connection__Command.Error.NotFound)
        }
      | Error(commandError) => Error(commandError)
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
        // Build final error with both path and command failures
        let constructionError = {
          Error.Construction.endpoints: pathErrors->Dict.fromArray,
          commands: commandErrors->Dict.fromArray,
          download: None,
        }
        Error(constructionError)
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
    constructionError: Error.Construction.t,
  ): result<t, Error.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) =>
      let errors =
        constructionError->Error.Construction.addDownloadError(PlatformNotSupported(platform))
      Error(Error.Construction(errors))
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
        Error(Error.Construction(constructionError))
      | No =>
        // User opted not to download, set policy and return error
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Construction(constructionError))
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        // Get the downloaded path, download if not already done
        let downloadResult = switch await PlatformOps.alreadyDownloaded2(globalStorageUri)() {
        | Some(path) => Ok(path)
        | None =>
          switch await PlatformOps.downloadLatestALS2(memento, globalStorageUri)(platform) {
          | Error(error) =>
            Error(Error.Construction(constructionError->Error.Construction.addDownloadError(error)))
          | Ok(path) => Ok(path)
          }
        }

        switch downloadResult {
        | Ok(path) =>
          switch await makeWithRawPath(path) {
          | Ok(connection) =>
            await Config.Connection.addAgdaPath2(getPath(connection))
            await Memento.PickedConnection.set(memento, Some(getPath(connection)))
            Ok(connection)
          | Error(error) =>
            Error(
              Error.Construction(
                Error.Construction.make()->Error.Construction.addEndpointError(path, error),
              ),
            )
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

    switch await fromPathsAndCommands(platformDeps, memento, paths, commands) {
    | Ok(connection) =>
      logConnection(connection)
      await Config.Connection.addAgdaPath2(getPath(connection))
      await Memento.PickedConnection.set(memento, Some(getPath(connection)))
      Ok(connection)
    | Error(constructionError) =>
      switch await fromDownloads(platformDeps, memento, globalStorageUri, constructionError) {
      | Ok(connection) =>
        logConnection(connection)
        await Config.Connection.addAgdaPath2(getPath(connection))
        await Memento.PickedConnection.set(memento, Some(getPath(connection)))
        Ok(connection)
      | Error(error) => Error(error)
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
}

include Module
