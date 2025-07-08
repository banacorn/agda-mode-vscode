module Error = Connection__Error
module Agda = Connection__Target__Agda
module ALS = Connection__Target__ALS
module Target = Connection__Target
module URI = Connection__URI

module type Module = {
  type t = Agda(Agda.t, Target.t) | ALS(ALS.t, Target.t)
  // lifecycle
  let make: (
    State__Memento.t,
    array<Connection__URI.t>,
    array<string>,
    result<Connection__Download__Platform.t, Connection__Download__Platform.raw>,
    unit => promise<Config.Connection.DownloadPolicy.t>,
    unit => promise<option<Target.t>>,
    Connection__Download__Platform.t => promise<result<Target.t, Connection__Download.Error.t>>,
  ) => promise<result<t, Error.t>>
  let destroy: option<t> => promise<result<unit, Error.t>>
  // components
  let fromPathsAndCommands: (
    State__Memento.t,
    array<Connection__URI.t>,
    array<string>,
  ) => promise<result<Target.t, Error.Aggregated.Attempts.t>>
  let fromDownloads: (
    Error.Aggregated.Attempts.t,
    result<Connection__Download__Platform.t, Connection__Download__Platform.raw>,
    // callbacks
    unit => promise<Config.Connection.DownloadPolicy.t>,
    unit => promise<option<Target.t>>,
    Connection__Download__Platform.t => promise<result<Target.t, Connection__Download.Error.t>>,
  ) => promise<result<Target.t, Error.t>>

  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<Target.t, Error.t>>

  // command
  let findCommands: array<string> => promise<result<Target.t, array<Connection__Command.Error.t>>>

  // misc
  let getInstalledTargetsAndPersistThem: VSCode.Uri.t => promise<
    Dict.t<result<Target.t, Target.Error.t>>,
  >
  let makeAgdaLanguageServerRepo: (
    State__Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__GitHub.Repo.t
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
  let findCommands = async commands => {
    let commands = List.fromArray(commands)
    let rec step = async (acc, commands) =>
      switch commands {
      | list{} => Error(acc)
      | list{command, ...rest} =>
        switch await Connection__Command.search(command) {
        | Ok(path) => Ok(path) // found, stop searching
        | Error(error) => await step(list{error, ...acc}, rest) // accumulate the error and continue searching
        }
      }
    switch await step(list{}, commands) {
    | Error(error) => Error(List.toArray(error))

    | Ok(path) => Ok(path)
    }
  }

  // Try to connect to Agda or ALS, with paths and commands ('agda' and 'als'), in the following steps:
  // 1. Go through the list of targets in the configuration, use the first one that works
  // 2. Try the `agda` command, add it to the list of targets if it works, else proceed to 3.
  // 3. Try the `als` command, add it to the list of targets if it works
  let fromPathsAndCommands = async (
    memento: State__Memento.t,
    paths: array<Connection__URI.t>,
    commands: array<string>,
  ): result<Target.t, Error.Aggregated.Attempts.t> => {
    switch await Target.getPicked(memento, paths) {
    | Error(targetErrors) =>
      switch await findCommands(commands) {
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
    attempts: Error.Aggregated.Attempts.t,
    platform: result<Connection__Download__Platform.t, Connection__Download__Platform.raw>,
    // callbacks
    getDownloadPolicyFromUser: unit => promise<Config.Connection.DownloadPolicy.t>,
    alreadyDownloaded: unit => promise<option<Target.t>>,
    downloadLatestALS: Connection__Download__Platform.t => promise<
      result<Target.t, Connection__Download.Error.t>,
    >,
  ): result<Target.t, Error.t> => {
    switch platform {
    | Error(platform) => Error(Error.Aggregated(PlatformNotSupported(attempts, platform)))
    | Ok(platform) =>
      // if the policy has not been set, ask the user
      let policy = switch Config.Connection.DownloadPolicy.get() {
      | Undecided => await getDownloadPolicyFromUser()
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
        switch await alreadyDownloaded() {
        | Some(target) =>
          await Config.Connection.addAgdaPath(Target.toURI(target))
          Ok(target)
        | None =>
          switch await downloadLatestALS(platform) {
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
    memento: State__Memento.t,
    paths: array<Connection__URI.t>,
    commands: array<string>,
    platform: result<Connection__Download__Platform.t, Connection__Download__Platform.raw>,
    // callbacks
    getDownloadPolicyFromUser: unit => promise<Config.Connection.DownloadPolicy.t>,
    alreadyDownloaded: unit => promise<option<Target.t>>,
    downloadLatestALS: Connection__Download__Platform.t => promise<
      result<Target.t, Connection__Download.Error.t>,
    >,
  ) =>
    switch await fromPathsAndCommands(memento, paths, commands) {
    | Error(attempts) =>
      switch await fromDownloads(
        attempts,
        platform,
        getDownloadPolicyFromUser,
        alreadyDownloaded,
        downloadLatestALS,
      ) {
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

  // 1. Get all installed targets from:
  //    * `agdaMode.connection.paths` in the settings
  //    * `agda` and `als` from the PATH
  //    * `agda` and `als` from the download folder
  // 2. Persist them in the settings
  let getInstalledTargetsAndPersistThem = async (globalStorageUri: VSCode.Uri.t) => {
    // from `agdaMode.connection.paths` in the settings
    let dict = {
      let pairs =
        await Config.Connection.getAgdaPaths()
        ->Array.map(async uri => {
          let target = await Target.fromURI(uri)
          (Connection__URI.toString(uri), target)
        })
        ->Promise.all

      Dict.fromArray(pairs)
    }

    // add `agda` and `als` from the PATH
    switch await findCommands(["agda"]) {
    | Ok(agda) =>
      let uri = agda->Connection__Target.toURI
      await Config.Connection.addAgdaPath(uri)
      dict->Dict.set(uri->Target.URI.toString, Ok(agda))
    | Error(_) => ()
    }
    switch await findCommands(["als"]) {
    | Ok(als) =>
      let uri = als->Connection__Target.toURI
      await Config.Connection.addAgdaPath(uri)
      dict->Dict.set(uri->Target.URI.toString, Ok(als))
    | Error(_) => ()
    }

    // assuming that all `als` and `agda` executables are placed in folders one level below the download folder
    // add `agda` and `als` from the download folder
    let addAgdaOrALS = async (folderURI, fileName) => {
      let executablePath = VSCode.Uri.joinPath(folderURI, [fileName])
      switch await Target.fromRawPath(VSCode.Uri.fsPath(executablePath)) {
      | Ok(target) =>
        let uri = target->Connection__Target.toURI
        await Config.Connection.addAgdaPath(uri)
        dict->Dict.set(uri->Target.URI.toString, Ok(target))
      | Error(_) => ()
      }
    }

    // handle files in the folders in the global storage
    let handleFile = folderPath => async ((filename, _fileType)) =>
      switch filename {
      | "agda" | "agda.exe" | "als" | "als.exe " => await addAgdaOrALS(folderPath, filename)
      | _ => ()
      }

    // handle folders in the global storage
    let handleFolder = async ((filename, _fileType)) => {
      let uri = VSCode.Uri.joinPath(globalStorageUri, [filename])
      switch await FS.readDirectory(uri) {
      | Ok(files) =>
        let _ = await files->Array.map(handleFile(uri))->Promise.all
      | Error(_) => ()
      }
    }

    // read the global storage directory and handle each folder
    switch await FS.readDirectory(globalStorageUri) {
    | Ok(folders) =>
      let _ = await folders->Array.map(handleFolder)->Promise.all
    | Error(_) => ()
    }

    // switch await Node__Fs.readdir(downloadPath) {
    // | folders =>
    //   let _ =
    //     await folders
    //     ->Array.map(async folder => {
    //       let folderPath = NodeJs.Path.join([downloadPath, folder])
    //       switch await \(folderPath) {
    //       | files =>
    //         let agdaFile = files->Array.find(file => file == "agda" || file == "agda.exe")
    //         let alsFile = files->Array.find(file => file == "als" || file == "als.exe")

    //         switch agdaFile {
    //         | Some(fileName) =>
    //           let executablePath = NodeJs.Path.join([folderPath, fileName])
    //           switch await Target.fromRawPath(executablePath) {
    //           | Ok(target) =>
    //             let uri = target->Connection__Target.toURI
    //             await Config.Connection.addAgdaPath(uri)
    //             dict->Dict.set(uri->Target.URI.toString, Ok(target))
    //           | Error(_) => ()
    //           }
    //         | None => ()
    //         }

    //         switch alsFile {
    //         | Some(fileName) =>
    //           let executablePath = NodeJs.Path.join([folderPath, fileName])
    //           switch await Target.fromRawPath(executablePath) {
    //           | Ok(target) =>
    //             let uri = target->Connection__Target.toURI
    //             await Config.Connection.addAgdaPath(uri)
    //             dict->Dict.set(uri->Target.URI.toString, Ok(target))
    //           | Error(_) => ()
    //           }
    //         | None => ()
    //         }
    //       | exception _ => ()
    //       }
    //     })
    //     ->Promise.all
    // | exception _ => ()
    // }

    dict
  }

  let makeAgdaLanguageServerRepo: (
    State__Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__GitHub.Repo.t = (memento, globalStorageUri) => {
    username: "agda",
    repository: "agda-language-server",
    userAgent: "agda/agda-mode-vscode",
    memento,
    globalStoragePath: VSCode.Uri.fsPath(globalStorageUri),
    cacheInvalidateExpirationSecs: 86400,
  }
}

include Module

module LatestALS = {
  let getALSReleaseManifest = async (memento, globalStorageUri) => {
    switch await Connection__Download__GitHub.ReleaseManifest.fetch(
      makeAgdaLanguageServerRepo(memento, globalStorageUri),
    ) {
    | (Error(error), _) => Error(Connection__Download.Error.CannotFetchALSReleases(error))
    | (Ok(manifest), _) => Ok(manifest)
    }
  }

  let chooseAssetByPlatform = async (
    release: Connection__Download__GitHub.Release.t,
    platform,
  ): array<Connection__Download__GitHub.Asset.t> => {
    let assetName = Connection__Download__Platform.toAssetName(platform)
    release.assets->Array.filter(asset => asset.name->String.endsWith(assetName ++ ".zip"))
  }

  let getFetchSpec = async (memento, globalStorageUri, platform) =>
    switch await getALSReleaseManifest(memento, globalStorageUri) {
    | Error(error) => Error(error)
    | Ok(releases) =>
      // only releases after 2024-12-18 are considered
      let laterReleases =
        releases->Array.filter(release =>
          Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
        )
      // present only the latest release at the moment

      // NOTE: using only v0.2.7.0.1.5 for now, we'll remove this constraint later
      let pinnedRelease = laterReleases->Array.find(release => release.name == "v0.2.7.0.1.5")

      // let latestRelease =
      //   laterReleases
      //   ->Array.toSorted((a, b) =>
      //     Date.compare(Date.fromString(b.published_at), Date.fromString(a.published_at))
      //   )
      //   ->Array.get(0)

      switch pinnedRelease {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(pinnedRelease) =>
        // for v0.2.7.0.0 onward, the ALS version is represented by the last digit
        let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
          asset.name
          ->String.replaceRegExp(%re("/als-Agda-/"), "")
          ->String.replaceRegExp(%re("/-.*/"), "")
        // choose the assets of the corresponding platform
        let assets = await chooseAssetByPlatform(pinnedRelease, platform)
        // choose the asset with the latest Agda version
        let result =
          assets
          ->Array.toSorted((a, b) => Util.Version.compare(getAgdaVersion(b), getAgdaVersion(a)))
          ->Array.map(asset => {
            Connection__Download__GitHub.FetchSpec.release: pinnedRelease,
            asset,
            saveAsFileName: "latest-als",
          })
          ->Array.get(0)

        switch result {
        | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
        | Some(fetchSpec) => Ok(fetchSpec)
        }
      }
    }

  // download the latest ALS and return the path of the downloaded file
  let download = (memento, globalStorageUri) => async platform =>
    switch await getFetchSpec(memento, globalStorageUri, platform) {
    | Error(error) => Error(error)
    | Ok(fetchSpec) => await Connection__Download.download(memento, globalStorageUri, fetchSpec)
    }

  // check if the latest ALS is already downloaded
  let alreadyDownloaded = globalStorageUri => async () => {
    let uri = VSCode.Uri.joinPath(globalStorageUri, ["latest-als", "als"])
    switch await FS.stat(uri) {
    | Ok(_) =>
      switch await Target.fromVSCodeUri(uri) {
      | Ok(target) => Some(target)
      | Error(_) => None
      }
    | Error(_) => None
    }
  }
}
