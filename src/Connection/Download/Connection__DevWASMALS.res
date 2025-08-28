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

let getDownloadDescriptor = async globalStorageUri => {
  switch await getALSReleaseManifestWithoutCache(globalStorageUri) {
  | Error(error) => Error(error)
  | Ok(releases) =>
    // target the specific "dev" release
    let devRelease = releases->Array.find(release => release.tag_name == "dev")

    switch devRelease {
    | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
    | Some(devRelease) =>
      // find the WASM asset specifically
      let wasmAsset = devRelease.assets->Array.find(asset => asset.name == "als-dev-wasm")
      
      switch wasmAsset {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(asset) => Ok({
          Connection__Download__GitHub.DownloadDescriptor.release: devRelease,
          asset,
          saveAsFileName: "dev-wasm-als",
        })
      }
    }
  }
}

// download the dev WASM ALS and return the path of the downloaded file
let download = globalStorageUri => async () =>
  switch await getDownloadDescriptor(globalStorageUri) {
  | Error(error) => Error(error)
  | Ok(downloadDescriptor) => await Connection__Download.download(globalStorageUri, downloadDescriptor)
  }

// check if the dev WASM ALS is already downloaded
let alreadyDownloaded = globalStorageUri => async () => {
  let uri = VSCode.Uri.joinPath(globalStorageUri, ["dev-wasm-als", "als-dev-wasm"])
  switch await FS.stat(uri) {
  | Ok(_) => Some(uri->VSCode.Uri.fsPath)
  | Error(_) => None
  }
}