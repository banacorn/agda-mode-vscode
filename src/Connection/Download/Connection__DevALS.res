let makeAgdaLanguageServerRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "banacorn",
  repository: "agda-language-server",
  userAgent: "banacorn/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 86400,
}

let getALSReleaseManifestWithoutCache = async globalStorageUri => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetchFromGitHub(
    makeAgdaLanguageServerRepo(globalStorageUri),
  ) {
  | Error(error) => Error(Connection__Download.Error.CannotFetchALSReleases(error))
  | Ok(manifest) => Ok(manifest)
  }
}

let chooseAssetByPlatform = async (
  release: Connection__Download__GitHub.Release.t,
  platform,
): array<Connection__Download__GitHub.Asset.t> => {
  let assetName = Connection__Download__Platform.toAssetName(platform)
  release.assets->Array.filter(asset => asset.name->String.endsWith(assetName ++ ".zip"))
}

let getFetchSpec = async (globalStorageUri, platform) => {
  switch await getALSReleaseManifestWithoutCache(globalStorageUri) {
  | Error(error) => Error(error)
  | Ok(releases) =>
    // target the specific "dev" release
    let devRelease = releases->Array.find(release => release.tag_name == "dev")

    switch devRelease {
    | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
    | Some(devRelease) =>
      let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
        asset.name
        ->String.replaceRegExp(%re("/als-dev-Agda-/"), "")
        ->String.replaceRegExp(%re("/-.*/"), "")
      // choose the assets of the corresponding platform
      let assets = await chooseAssetByPlatform(devRelease, platform)
      // choose the asset with the latest Agda version
      let result =
        assets
        ->Array.toSorted((a, b) => Util.Version.compare(getAgdaVersion(b), getAgdaVersion(a)))
        ->Array.map(asset => {
          Connection__Download__GitHub.FetchSpec.release: devRelease,
          asset,
          saveAsFileName: "dev-als",
        })
        ->Array.get(0)

      switch result {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(fetchSpec) => Ok(fetchSpec)
      }
    }
  }
}

// download the dev ALS and return the path of the downloaded file
let download = globalStorageUri => async platform =>
  switch await getFetchSpec(globalStorageUri, platform) {
  | Error(error) => Error(error)
  | Ok(fetchSpec) => await Connection__Download.download(globalStorageUri, fetchSpec)
  }

// check if the dev ALS is already downloaded
let alreadyDownloaded = globalStorageUri => async () => {
  let uri = VSCode.Uri.joinPath(globalStorageUri, ["dev-als", "als"])
  switch await FS.stat(uri) {
  | Ok(_) => Some(uri->VSCode.Uri.fsPath)
  | Error(_) => None
  }
}
