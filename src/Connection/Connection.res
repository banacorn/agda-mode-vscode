module Error = Connection__Error
module Agda = Connection__Target__Agda
module ALS = Connection__Target__ALS
module Target = Connection__Target
module URI = Connection__URI

module type Module = {
  type t = Agda(Agda.t, Target.t) | ALS(ALS.t, Target.t)
  // lifecycle
  let make: State__Memento.t => promise<result<t, Error.t>>
  let destroy: option<t> => promise<result<unit, Error.t>>
  // messaging
  let sendRequest: (
    t,
    VSCode.TextDocument.t,
    Request.t,
    Response.t => promise<unit>,
  ) => promise<result<Target.t, Error.t>>

  //
  let findCommand: string => promise<result<t, Error.t>>

  // misc
  let makeAgdaLanguageServerRepo: (
    State__Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__GitHub.Repo.t
  let getALSReleaseManifest: (
    State__Memento.t,
    VSCode.Uri.t,
  ) => promise<result<array<Connection__Download__GitHub.Release.t>, Error.t>>

  // download
  let isLatestALSDownloaded: VSCode.Uri.t => promise<bool>
  let downloadLatestALS: (
    State__Memento.t,
    VSCode.Uri.t,
    Connection__Download__Util.Event.t => unit,
  ) => promise<option<Connection__Download__GitHub.Target.t>>
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

  let start_ = async (target: Target.t): result<t, Error.t> =>
    switch target {
    | Agda(version, path) =>
      let method = Connection__IPC.ViaPipe(path, [], None, FromFile(path))
      switch await Agda.make(method) {
      | Error(error) => Error(Error.Agda(error, path))
      | Ok(conn) => Ok(Agda(conn, Agda(version, path)))
      }
    | ALS(alsVersion, agdaVersion, Ok(method)) =>
      switch await ALS.make(method, InitOptions.getFromConfig()) {
      | Error(error) => Error(ALS(error))
      | Ok(conn) =>
        let method = ALS.getIPCMethod(conn)
        Ok(ALS(conn, ALS(alsVersion, agdaVersion, Ok(method))))
      }
    | ALS(alsVersion, agdaVersion, Error(path)) =>
      switch await ALS.make(
        Connection__IPC.ViaPipe(path, [], None, FromFile(path)),
        InitOptions.getFromConfig(),
      ) {
      | Error(error) => Error(ALS(error))
      | Ok(conn) =>
        // let method = ALS.getIPCMethod(conn)
        Ok(ALS(conn, ALS(alsVersion, agdaVersion, Error(path))))
      }
    }

  let findCommand = async command => {
    switch await Connection__Command__Search.search(command) {
    | Error(_error) => Error(Error.CannotFindALSorAgda)
    | Ok(path) =>
      switch await Target.fromRawPath(path) {
      | Error(error) => Error(error)
      | Ok(target) =>
        await Config.Connection.addAgdaPath(path)
        await start_(target)
      }
    }
  }

  let findALSAndAgda = async () => {
    switch await findCommand("als") {
    | Error(_error) => await findCommand("agda")
    | Ok(conn) => Ok(conn)
    }
  }

  let make = async (memento: State__Memento.t) =>
    switch await Target.getPicked(memento) {
    | None => await findALSAndAgda()
    | Some(target) => await start_(target)
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

  let getALSReleaseManifest = async (memento, globalStorageUri) => {
    switch await Connection__Download__GitHub.ReleaseManifest.fetch(
      makeAgdaLanguageServerRepo(memento, globalStorageUri),
    ) {
    | (Error(error), _) => Error(Error.CannotFetchALSReleases(error))
    | (Ok(manifest), _) => Ok(manifest)
    }
  }

  module LatestALS = {
    let chooseAssetFromRelease = (release: Connection__Download__GitHub.Release.t): array<
      Connection__Download__GitHub.Asset.t,
    > => {
      // determine the platform
      let platform = switch NodeJs.Os.platform() {
      | "darwin" =>
        switch Node__OS.arch() {
        | "x64" => Some("macos-x64")
        | "arm64" => Some("macos-arm64")
        | _ => None
        }
      | "linux" => Some("ubuntu")
      | "win32" => Some("windows")
      | _ => None
      }
      switch platform {
      | Some(platform) =>
        release.assets->Array.filter(asset => asset.name->String.endsWith(platform ++ ".zip"))
      | None => []
      }
    }

    let getTarget = async (memento, globalStorageUri) =>
      switch await getALSReleaseManifest(memento, globalStorageUri) {
      | Error(_error) => None
      | Ok(releases) =>
        // only releases after 2024-12-18 are considered
        let laterReleases =
          releases->Array.filter(release =>
            Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
          )
        // present only the latest release at the moment
        let latestRelease =
          laterReleases
          ->Array.toSorted((a, b) =>
            Date.compare(Date.fromString(b.published_at), Date.fromString(a.published_at))
          )
          ->Array.get(0)

        switch latestRelease {
        | None => None
        | Some(latestRelease) =>
          // for v0.2.7.0.0 onward, the ALS version is represented by the last digit
          let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
            asset.name
            ->String.replaceRegExp(%re("/als-Agda-/"), "")
            ->String.replaceRegExp(%re("/-.*/"), "")
          // choose the assets of the corresponding platform
          let assets = chooseAssetFromRelease(latestRelease)
          // choose the asset with the latest Agda version
          assets
          ->Array.toSorted((a, b) => Util.Version.compare(getAgdaVersion(b), getAgdaVersion(a)))
          ->Array.map(asset => {
            Connection__Download__GitHub.Target.release: latestRelease,
            asset,
            saveAsFileName: "latest-als",
          })
          ->Array.get(0)
        }
      }

    // check if the latest ALS is already downloaded
    let alreadyDownloaded = async globalStoragePath => {
      let path = NodeJs.Path.join([globalStoragePath, "latest-als"])
      switch await NodeJs.Fs.access(path) {
      | () => true
      | exception _ => false
      }
    }

    // download the latest ALS
    let download = async (memento, globalStoragePath, onDownload, target) => {
      switch await Connection__Download__GitHub.download(
        target,
        memento,
        globalStoragePath,
        onDownload,
      ) {
      | Error(e) => Js.log("Failed to download: " ++ Connection__Download__GitHub.Error.toString(e))
      | Ok(isCached) =>
        Js.log("isCached: " ++ string_of_bool(isCached))

        // add the path of the downloaded file to the config
        let destPath = NodeJs.Path.join([globalStoragePath, target.saveAsFileName, "als"])
        Js.log("Downloaded to: " ++ destPath)
        Config.Connection.addAgdaPath(destPath)->ignore
      }
    }
  }

  let isLatestALSDownloaded = globalStorageUri =>
    LatestALS.alreadyDownloaded(VSCode.Uri.fsPath(globalStorageUri))

  let downloadLatestALS = async (memento, globalStorageUri, progressCallback) => {
    switch await LatestALS.getTarget(memento, globalStorageUri) {
    | None => None
    | Some(target) =>
      await LatestALS.download(
        memento,
        VSCode.Uri.fsPath(globalStorageUri),
        progressCallback,
        target,
      )
      Some(target)
    }
  }
}

include Module
