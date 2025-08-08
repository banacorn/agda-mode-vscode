module Error = Connection__Error
module Agda = Connection__Endpoint__Agda
module ALS = Connection__Endpoint__ALS
module Endpoint = Connection__Endpoint
module URI = Connection__URI

module type Module = {
  type t =
    | Agda(Agda.t, string, string) // connection, path, version
    | ALS(ALS.t, string, string, string) // connection, path, ALS version, Agda version

  // Platform dependencies type
  // lifecycle
  let make: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<t, Error.t>>
  let make2: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    array<string>,
    array<string>,
  ) => promise<result<t, Error.t>>
  let destroy: option<t> => promise<result<unit, Error.t>>

  // components (now use platform dependencies)
  let fromPathsAndCommands: (
    Platform.t,
    Memento.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<Endpoint.t, Error.Construction.Attempts.t>>
  let fromDownloads: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    Error.Construction.Attempts.t,
  ) => promise<result<Endpoint.t, Error.t>>

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

  let destroy = async connection =>
    switch connection {
    | None => Ok()
    | Some(connection) =>
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

  let makeWithEndpoint = async (endpoint: Endpoint.t): result<t, Error.t> =>
    switch endpoint {
    | Agda(version, path) =>
      switch await Agda.make(path, version) {
      | Error(error) => Error(Error.Agda(error))
      | Ok(conn) => Ok(Agda(conn, path, version))
      }
    | ALS(alsVersion, agdaVersion, method, lspOptions) =>
      switch await ALS.make(method, lspOptions, InitOptions.getFromConfig()) {
      | Error(error) => Error(ALS(error))
      | Ok(conn) =>
        switch method {
        | Connection__Transport.ViaPipe(path, _) => Ok(ALS(conn, path, alsVersion, agdaVersion))
        | Connection__Transport.ViaTCP(_, _) =>
          Ok(
            ALS(
              conn,
              Endpoint.toURI(endpoint)->Connection__Endpoint.URI.getOriginalPath,
              alsVersion,
              agdaVersion,
            ),
          )
        }
      }
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

  // Try to connect to Agda or ALS, with paths and commands ('agda' and 'als'), in the following steps:
  // 1. Go through the list of endpoints in the configuration, use the first one that works
  // 2. Try the `agda` command, add it to the list of endpoints if it works, else proceed to 3.
  // 3. Try the `als` command, add it to the list of endpoints if it works
  let fromPathsAndCommands = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    paths: array<Connection__URI.t>,
    commands: array<string>,
  ): result<Endpoint.t, Error.Construction.Attempts.t> => {
    switch await Endpoint.getPicked(memento, paths) {
    | Error(endpointErrors) =>
      switch await findCommands(platformDeps, commands) {
      | Error(commandErrors) =>
        let attempts = {
          Error.Construction.Attempts.endpoints: endpointErrors,
          commands: commandErrors,
        }

        Error(attempts)

      | Ok(endpoint) => Ok(endpoint)
      }
    | Ok(endpoint) => Ok(endpoint)
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
          let newErrors = errors->Array.concat([(item, error)])
          let nextItems = remaining->Array.sliceToEnd(~start=1)
          await loop(nextItems, newErrors)
        }
      | None => Error(errors)
      }
    }
    await loop(items, [])
  }

  // Make a connection using raw paths and commands, by trying in order:
  //  1. Previously picked path (if it exists in the supplied paths)
  //  2. Each path from the paths array sequentially
  //  3. Each command from the commands array sequentially
  // Returns Ok(connection) on first success, or Error with all collected failures
  let fromPathsAndCommands2 = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    paths: array<string>,
    commands: array<string>,
  ): result<t, Error.Construction.Attempts.t> => {
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
        let attempts = {
          Error.Construction.Attempts.endpoints: pathErrors->Dict.fromArray,
          commands: commandErrors->Dict.fromArray,
        }
        Error(attempts)
      }
    }
  }

  // Try to download ALS, with the following steps:
  // 1. See if the platform is supported:
  //      No  : exit with the `PlatformNotSupported` error ❌
  //      Yes : proceed to 2.
  // 2. Check the download policy:
  //      Undecided : ask the user if they want to download ALS or not, go back to 1.
  //      No        : exit with the `NoDownloadALS` error ❌
  //      Yes       : proceed to 3.
  // 3. Check if the latest ALS is already downloaded:
  //      Yes       : ✅
  //      No        : proceed to 4.
  // 4. Download the latest ALS:
  //      Succeed   : add it to the list of endpoints ✅
  //      Failed    : exit with the `DownloadALS` error ❌

  let fromDownloads = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    attempts: Error.Construction.Attempts.t,
  ): result<Endpoint.t, Error.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) =>
      Error(Error.Construction(DownloadALS(attempts, PlatformNotSupported(platform))))
    | Ok(platform) =>
      // if the policy has not been set, ask the user
      let policy = switch Config.Connection.DownloadPolicy.get() {
      | Undecided => await PlatformOps.askUserAboutDownloadPolicy()
      | policy => policy
      }

      switch policy {
      | Config.Connection.DownloadPolicy.Undecided =>
        // the user has clicked on "cancel" in the dialog, treat it as "No"
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Construction(NoDownloadALS(attempts)))
      | No =>
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Construction(NoDownloadALS(attempts)))
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        switch await PlatformOps.alreadyDownloaded(globalStorageUri)() {
        | Some(endpoint) =>
          await Config.Connection.addAgdaPath(Endpoint.toURI(endpoint))
          Ok(endpoint)
        | None =>
          switch await PlatformOps.downloadLatestALS(memento, globalStorageUri)(platform) {
          | Error(error) => Error(Error.Construction(DownloadALS(attempts, error)))
          | Ok(endpoint) =>
            await Config.Connection.addAgdaPath(Connection__Endpoint.toURI(endpoint))
            Ok(endpoint)
          }
        }
      }
    }
  }

  // Try to download ALS and create connection directly, with the following steps:
  // 1. Check platform support
  // 2. Check download policy
  // 3. Check if already downloaded or download latest ALS
  // 4. Create connection directly from downloaded raw path
  // Returns Ok(connection) on success, or Error with failure details
  let fromDownloads2 = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    attempts: Error.Construction.Attempts.t,
  ): result<t, Error.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) =>
      Error(Error.Construction(DownloadALS(attempts, PlatformNotSupported(platform))))
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
        Error(Error.Construction(NoDownloadALS(attempts)))
      | No =>
        // User opted not to download, set policy and return error
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Construction(NoDownloadALS(attempts)))
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        // Get the downloaded path, download if not already done
        let downloadResult = switch await PlatformOps.alreadyDownloaded2(globalStorageUri)() {
        | Some(path) => Ok(path)
        | None =>
          switch await PlatformOps.downloadLatestALS2(memento, globalStorageUri)(platform) {
          | Error(error) => Error(Error.Construction(DownloadALS(attempts, error)))
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
          | Error(error) => Error(Error.Construction(Endpoint(path, error)))
          }
        | Error(error) => Error(error)
        }
      }
    }
  }

  // Try to make a connection to Agda or ALS, by trying:
  //  1. `fromPathsAndCommands` to connect to Agda or ALS with paths and commands
  //  2. `fromDownloads` to download the latest ALS
  let make = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    paths: array<Connection__URI.t>,
    commands: array<string>,
  ) =>
    switch await fromPathsAndCommands(platformDeps, memento, paths, commands) {
    | Error(attempts) =>
      switch await fromDownloads(platformDeps, memento, globalStorageUri, attempts) {
      | Error(error) => Error(error)
      | Ok(endpoint) =>
        await Config.Connection.addAgdaPath(endpoint->Endpoint.toURI)
        await makeWithEndpoint(endpoint)
      }
    | Ok(endpoint) =>
      await Config.Connection.addAgdaPath(endpoint->Endpoint.toURI)
      await makeWithEndpoint(endpoint)
    }

  // Make a connection to Agda or ALS, by trying:
  //  1. The previously picked raw path if it exists in settings
  //  2. Each raw path from the settings sequentially
  //  3. Each command from the commands array sequentially
  //  4. Downloading the latest ALS
  let make2 = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
    paths: array<string>,
    commands: array<string>,
  ): result<t, Error.t> =>
    switch await fromPathsAndCommands2(platformDeps, memento, paths, commands) {
    | Ok(connection) =>
      await Config.Connection.addAgdaPath2(getPath(connection))
      await Memento.PickedConnection.set(memento, Some(getPath(connection)))
      Ok(connection)
    | Error(attempts) =>
      switch await fromDownloads2(platformDeps, memento, globalStorageUri, attempts) {
      | Ok(connection) =>
        await Config.Connection.addAgdaPath2(getPath(connection))
        await Memento.PickedConnection.set(memento, Some(getPath(connection)))
        Ok(connection)
      | Error(error) => Error(error)
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
        let _ = await destroy(Some(ALS(conn, path, alsVersion, agdaVersion)))
        Error(Error.ALS(error))
      | Ok() => Ok()
      }

    | Agda(conn, path, version) =>
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(Agda(conn, path, version)))
        Error(Error.Agda(error))
      | Ok() => Ok()
      }
    }
  }
}

include Module
