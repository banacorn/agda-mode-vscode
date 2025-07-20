module Error = Connection__Error
module Agda = Connection__Target__Agda
module ALS = Connection__Target__ALS
module Target = Connection__Target
module URI = Connection__URI

module type Module = {
  type t = Agda(Agda.t, Target.t) | ALS(ALS.t, Target.t)

  // Platform dependencies type
  type platformDeps = Platform.platformDeps

  // lifecycle
  let make: (
    platformDeps,
    State__Memento.t,
    VSCode.Uri.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<t, Error.t>>
  let destroy: option<t> => promise<result<unit, Error.t>>

  // components (now use platform dependencies)
  let fromPathsAndCommands: (
    platformDeps,
    State__Memento.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<Target.t, Error.Aggregated.Attempts.t>>
  let fromDownloads: (
    platformDeps,
    State__Memento.t,
    VSCode.Uri.t,
    Error.Aggregated.Attempts.t,
  ) => promise<result<Target.t, Error.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<Target.t, Error.t>>

  // command (uses platform dependencies)
  let findCommands: (
    platformDeps,
    array<string>,
  ) => promise<result<Target.t, array<Connection__Command.Error.t>>>
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
  type t = Agda(Agda.t, Target.t) | ALS(ALS.t, Target.t)

  // Platform dependencies type
  type platformDeps = Platform.platformDeps

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

  let makeWithTarget = async (target: Target.t): result<t, Error.t> =>
    switch target {
    | Agda(version, path) =>
      let method = Connection__Target__IPC.ViaPipe(path, [], None)
      switch await Agda.make(method, version, path) {
      | Error(error) => Error(Error.Agda(error, path))
      | Ok(conn) => Ok(Agda(conn, target))
      }
    | ALS(_, _, method) =>
      switch await ALS.make(method, InitOptions.getFromConfig()) {
      | Error(error) => Error(ALS(error))
      | Ok(conn) => Ok(ALS(conn, target))
      }
    }

  // search through a list of commands until one is found
  let findCommands = async (platformDeps: platformDeps, commands) => {
    module PlatformOps = unpack(platformDeps)
    await PlatformOps.findCommands(commands)
  }

  // Try to connect to Agda or ALS, with paths and commands ('agda' and 'als'), in the following steps:
  // 1. Go through the list of targets in the configuration, use the first one that works
  // 2. Try the `agda` command, add it to the list of targets if it works, else proceed to 3.
  // 3. Try the `als` command, add it to the list of targets if it works
  let fromPathsAndCommands = async (
    platformDeps: platformDeps,
    memento: State__Memento.t,
    paths: array<Connection__URI.t>,
    commands: array<string>,
  ): result<Target.t, Error.Aggregated.Attempts.t> => {
    switch await Target.getPicked(memento, paths) {
    | Error(targetErrors) =>
      switch await findCommands(platformDeps, commands) {
      | Error(commandErrors) =>
        let attempts = {
          Error.Aggregated.Attempts.targets: targetErrors,
          commands: commandErrors,
        }

        Error(attempts)

      | Ok(target) => Ok(target)
      }
    | Ok(target) => Ok(target)
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
  //      Succeed   : add it to the list of targets ✅
  //      Failed    : exit with the `DownloadALS` error ❌

  let fromDownloads = async (
    platformDeps: platformDeps,
    memento: State__Memento.t,
    globalStorageUri: VSCode.Uri.t,
    attempts: Error.Aggregated.Attempts.t,
  ): result<Target.t, Error.t> => {
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
        | Some(target) =>
          await Config.Connection.addAgdaPath(Target.toURI(target))
          Ok(target)
        | None =>
          switch await PlatformOps.downloadLatestALS(memento, globalStorageUri)(platform) {
          | Error(error) => Error(Error.Aggregated(DownloadALS(attempts, error)))
          | Ok(target) =>
            await Config.Connection.addAgdaPath(Connection__Target.toURI(target))
            Ok(target)
          }
        }
      }
    }
  }

  // Try to make a connection to Agda or ALS, by trying:
  //  1. `fromPathsAndCommands` to connect to Agda or ALS with paths and commands
  //  2. `fromDownloads` to download the latest ALS
  let make = async (
    platformDeps: platformDeps,
    memento: State__Memento.t,
    globalStorageUri: VSCode.Uri.t,
    paths: array<Connection__URI.t>,
    commands: array<string>,
  ) =>
    switch await fromPathsAndCommands(platformDeps, memento, paths, commands) {
    | Error(attempts) =>
      switch await fromDownloads(platformDeps, memento, globalStorageUri, attempts) {
      | Error(error) => Error(error)
      | Ok(target) =>
        await Config.Connection.addAgdaPath(target->Target.toURI)
        await makeWithTarget(target)
      }
    | Ok(target) =>
      await Config.Connection.addAgdaPath(target->Target.toURI)
      await makeWithTarget(target)
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
    | ALS(conn, target) =>
      switch await ALS.sendRequest(conn, encodeRequest(document, conn.agdaVersion), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(ALS(conn, target)))
        Error(Error.ALS(error))
      | Ok(_) => Ok(target)
      }

    | Agda(conn, target) =>
      let (version, path) = Agda.getInfo(conn)
      switch await Agda.sendRequest(conn, encodeRequest(document, version), handler) {
      | Error(error) =>
        // stop the connection on error
        let _ = await destroy(Some(Agda(conn, target)))
        Error(Error.Agda(error, path))
      | Ok(_) => Ok(target)
      }
    }
  }
}

include Module
