let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "banacorn/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 3600,
}

let chooseAssetByPlatform = (release: Connection__Download__GitHub.Release.t, platform): array<
  Connection__Download__GitHub.Asset.t,
> => Connection__Download__Assets.forPlatform(release, platform)

let getAgdaVersionFromAssetName = (asset: Connection__Download__GitHub.Asset.t) =>
  Connection__Download__Assets.getAgdaVersionFromAssetName(asset)

let toDownloadOrder = (releases: array<Connection__Download__GitHub.Release.t>, platform) => {
  // target the specific "dev" release
  let devRelease = releases->Array.find(release => release.tag_name == "dev")

  switch devRelease {
  | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
  | Some(devRelease) =>
    // choose the assets of the corresponding platform
    let assets = chooseAssetByPlatform(devRelease, platform)
    // choose the asset with the latest Agda version
    let result =
      assets
      ->Array.toSorted((a, b) =>
        Util.Version.compare(getAgdaVersionFromAssetName(b), getAgdaVersionFromAssetName(a))
      )
      ->Array.map(asset => {
        Connection__Download__GitHub.DownloadDescriptor.release: devRelease,
        asset,
        saveAsFileName: "dev-als",
      })
      ->Array.get(0)

    switch result {
    | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
    | Some(downloadDescriptor) => Ok(Connection__Download.Source.FromGitHub(DevALS, downloadDescriptor))
    }
  }
}
