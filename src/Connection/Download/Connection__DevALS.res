let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "banacorn",
  repository: "agda-language-server",
  userAgent: "banacorn/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 86400,
}

let chooseAssetByPlatform = (release: Connection__Download__GitHub.Release.t, platform): array<
  Connection__Download__GitHub.Asset.t,
> => {
  let assetName = Connection__Download__Platform.toAssetName(platform)
  release.assets->Array.filter(asset => asset.name->String.endsWith(assetName ++ ".zip"))
}

let toDownloadOrder = (releases: array<Connection__Download__GitHub.Release.t>, platform) => {
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
    let assets = chooseAssetByPlatform(devRelease, platform)
    // choose the asset with the latest Agda version
    let result =
      assets
      ->Array.toSorted((a, b) => Util.Version.compare(getAgdaVersion(b), getAgdaVersion(a)))
      ->Array.map(asset => {
        Connection__Download__GitHub.DownloadDescriptor.release: devRelease,
        asset,
        saveAsFileName: "dev-als",
      })
      ->Array.get(0)

    switch result {
    | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
    | Some(downloadDescriptor) => Ok(Connection__Download.DownloadOrderConcrete.FromGitHub(DevALS, downloadDescriptor))
    }
  }
}