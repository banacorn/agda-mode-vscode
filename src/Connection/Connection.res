module Error = Connection__Error
module Agda = Connection__Endpoint__Agda
module ALS = Connection__Endpoint__ALS
module Endpoint = Connection__Endpoint
module URI = Connection__URI

module type Module = {
  type t = Agda(Agda.t, Endpoint.t) | ALS(ALS.t, Endpoint.t)

  // Platform dependencies type
  // lifecycle
  let make: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<t, Error.t>>
  let destroy: option<t> => promise<result<unit, Error.t>>

  // components (now use platform dependencies)
  let fromPathsAndCommands: (
    Platform.t,
    Memento.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<Endpoint.t, Error.Aggregated.Attempts.t>>
  let fromDownloads: (
    Platform.t,
    Memento.t,
    VSCode.Uri.t,
    Error.Aggregated.Attempts.t,
  ) => promise<result<Endpoint.t, Error.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<Endpoint.t, Error.t>>

  // command (uses platform dependencies)
  let findCommands: (
    Platform.t,
    array<string>,
  ) => promise<result<Endpoint.t, array<Connection__Command.Error.t>>>
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
  type t = Agda(Agda.t, Endpoint.t) | ALS(ALS.t, Endpoint.t)

  let destroy = async connection =>
    switch connection {
    | None => Ok()
    | Some(connection) =>
      switch connection {
      | Agda(conn, _) =>
        await Agda.destroy(conn)
        Ok()
      | ALS(conn, _) =>
        switch await ALS.destroy(conn) {
        | Error(error) => Error(Error.ALS(error))
        | Ok(_) => Ok()
        }
      }
    }

  let makeWithEndpoint = async (endpoint: Endpoint.t): result<t, Error.t> =>
    switch endpoint {
    | Agda(version, path) =>
      let method = Connection__Transport.ViaPipe(path, [])
      switch await Agda.make(method, version, path) {
      | Error(error) => Error(Error.Agda(error, path))
      | Ok(conn) => Ok(Agda(conn, endpoint))
      }
    | ALS(_, _, method, lspOptions) =>
      switch await ALS.make(method, lspOptions, InitOptions.getFromConfig()) {
      | Error(error) => Error(ALS(error))
      | Ok(conn) => Ok(ALS(conn, endpoint))
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
  ): result<Endpoint.t, Error.Aggregated.Attempts.t> => {
    switch await Endpoint.getPicked(memento, paths) {
    | Error(endpointErrors) =>
      switch await findCommands(platformDeps, commands) {
      | Error(commandErrors) =>
        let attempts = {
          Error.Aggregated.Attempts.endpoints: endpointErrors,
          commands: commandErrors,
        }

        Error(attempts)

      | Ok(endpoint) => Ok(endpoint)
      }
    | Ok(endpoint) => Ok(endpoint)
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
    attempts: Error.Aggregated.Attempts.t,
  ): result<Endpoint.t, Error.t> => {
    module PlatformOps = unpack(platformDeps)

    switch await PlatformOps.determinePlatform() {
    | Error(platform) => Error(Error.Aggregated(PlatformNotSupported(attempts, platform)))
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
        Error(Error.Aggregated(NoDownloadALS(attempts)))
      | No =>
        await Config.Connection.DownloadPolicy.set(No)
        Error(Error.Aggregated(NoDownloadALS(attempts)))
      | Yes =>
        await Config.Connection.DownloadPolicy.set(Yes)
        switch await PlatformOps.alreadyDownloaded(globalStorageUri)() {
        | Some(endpoint) =>
          await Config.Connection.addAgdaPath(Endpoint.toURI(endpoint))
          Ok(endpoint)
        | None =>
          switch await PlatformOps.downloadLatestALS(memento, globalStorageUri)(platform) {
          | Error(error) => Error(Error.Aggregated(DownloadALS(attempts, error)))
          | Ok(endpoint) =>
            await Config.Connection.addAgdaPath(Connection__Endpoint.toURI(endpoint))
            Ok(endpoint)
          }
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
    | ALS(conn, endpoint) =>
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.agdaVersion), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(ALS(conn, endpoint)))
        Error(Error.ALS(error))
      | Ok(_) => Ok(endpoint)
      }

    | Agda(conn, endpoint) =>
      let (version, path) = Agda.getInfo(conn)
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(Agda(conn, endpoint)))
        Error(Error.Agda(error, path))
      | Ok(_) => Ok(endpoint)
      }
    }
  }
}

include Module
