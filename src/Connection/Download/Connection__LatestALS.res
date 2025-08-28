let makeAgdaLanguageServerRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "agda/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 86400,
}

let getALSReleaseManifest = async (memento, globalStorageUri) => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetch(
    memento,
    makeAgdaLanguageServerRepo(globalStorageUri),
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

let getDownloadDescriptor = async (memento, globalStorageUri, platform) =>
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
          Connection__Download__GitHub.DownloadDescriptor.release: pinnedRelease,
          asset,
          saveAsFileName: "latest-als",
        })
        ->Array.get(0)

      switch result {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(downloadDescriptor) => Ok(downloadDescriptor)
      }
    }
  }

// download the latest ALS and return the path of the downloaded file
let download = (memento, globalStorageUri) => async platform =>
  switch await getDownloadDescriptor(memento, globalStorageUri, platform) {
  | Error(error) => Error(error)
  | Ok(downloadDescriptor) => await Connection__Download.download(globalStorageUri, downloadDescriptor)
  }