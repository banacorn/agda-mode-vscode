let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "banacorn/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 3600,
}

let chooseAssetByPlatform = (release: Connection__Download__GitHub.Release.t, platform): array<
  Connection__Download__GitHub.Asset.t,
> => {
  let assetName = Connection__Download__Platform.toAssetName(platform)
  release.assets->Array.filter(asset =>
    asset.name->String.endsWith(".zip") &&
      switch Connection__Download.AssetName.parse(asset.name) {
      | Some(parsed) => parsed.platform == assetName
      | None => false
      }
  )
}

let getAgdaVersionFromAssetName = (asset: Connection__Download__GitHub.Asset.t) =>
  Connection__Download.AssetName.parse(asset.name)->Option.mapOr("", parsed => parsed.agdaVersion)

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

let allNativeAssetsForPlatform = (
  release: Connection__Download__GitHub.Release.t,
  platform,
): array<Connection__Download__GitHub.Asset.t> => {
  let assetName = Connection__Download__Platform.toAssetName(platform)
  release.assets
  ->Array.filter(asset =>
    asset.name->String.endsWith(".zip") &&
      switch Connection__Download.AssetName.parse(asset.name) {
      | Some(parsed) => parsed.platform == assetName
      | None => false
      }
  )
  ->Array.toSorted((a, b) =>
    Util.Version.compare(getAgdaVersionFromAssetName(b), getAgdaVersionFromAssetName(a))
  )
}

let allWasmAssets = (
  release: Connection__Download__GitHub.Release.t,
): array<Connection__Download__GitHub.Asset.t> => {
  release.assets
  ->Array.filter(asset =>
    asset.name->String.endsWith(".wasm") &&
      switch Connection__Download.AssetName.parse(asset.name) {
      | Some(parsed) => parsed.platform == "wasm"
      | None => false
      }
  )
  ->Array.toSorted((a, b) =>
    Util.Version.compare(getAgdaVersionFromAssetName(b), getAgdaVersionFromAssetName(a))
  )
}
