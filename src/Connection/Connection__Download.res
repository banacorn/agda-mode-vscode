module Error = {
  type t =
    | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
    | CannotDownloadALS(Connection__Download__GitHub.Error.t)
    | CannotConnectToALS(Connection__Target.Error.t)
    | CannotFindCompatibleALSRelease

  let toString = x =>
    switch x {
    | CannotFetchALSReleases(e) =>
      "Cannot fetch releases of Agda Language Server: " ++
      Connection__Download__GitHub.Error.toString(e)

    | CannotFindCompatibleALSRelease => "Cannot find compatible Agda Language Server release for download. Prebuilts are only available for download on Ubuntu, Windows, and macOS (arm64, x64).\nPlease build from source if you are on a different platform. \nSee https://github.com/agda/agda-language-server for more information."
    | CannotConnectToALS(e) => Connection__Target.Error.toString(e)
    | CannotDownloadALS(e) =>
      "Failed to download the Agda Language Server: " ++ Connection__Download__GitHub.Error.toString(e)
    }
}

let makeRepo: (State__Memento.t, VSCode.Uri.t) => Connection__Download__GitHub.Repo.t = (
  memento,
  globalStorageUri,
) => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "agda/agda-mode-vscode",
  memento,
  globalStoragePath: VSCode.Uri.fsPath(globalStorageUri),
  cacheInvalidateExpirationSecs: 86400,
}

let getReleaseManifest = async (memento, globalStorageUri) => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetch(
    makeRepo(memento, globalStorageUri),
  ) {
  | (Error(error), _) => Error(Error.CannotFetchALSReleases(error))
  | (Ok(manifest), _) => Ok(manifest)
  }
}

// Download the given FetchSpec and return the path of the downloaded file
let download = async (memento, globalStorageUri, fetchSpec) => {
  let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
  let globalStoragePath = VSCode.Uri.fsPath(globalStorageUri)
  switch await Connection__Download__GitHub.download(
    fetchSpec,
    memento,
    globalStoragePath,
    reportProgress,
  ) {
  | Error(error) => Error(Error.CannotDownloadALS(error))
  | Ok(_isCached) =>
    // add the path of the downloaded file to the config
    let destPath = Connection__URI.parse(
      NodeJs.Path.join([globalStoragePath, fetchSpec.saveAsFileName, "als"]),
    )
    await Config.Connection.addAgdaPath(destPath)
    switch await Connection__Target.fromURI(destPath) {
    | Error(e) => Error(Error.CannotConnectToALS(e))
    | Ok(target) => Ok(target)
    }
  }
}
