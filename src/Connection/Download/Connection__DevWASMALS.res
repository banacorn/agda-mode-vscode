let makeAgdaLanguageServerRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "banacorn",
  repository: "agda-language-server",
  userAgent: "banacorn/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 86400,
}

let getDownloadDescriptor = async (memento, globalStorageUri) => {
  switch await Connection__Download.getReleaseManifestFromGitHub(
    memento,
    makeAgdaLanguageServerRepo(globalStorageUri),

    ~useCache=false,
  ) {
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
      | Some(asset) =>
        Ok({
          Connection__Download__GitHub.DownloadDescriptor.release: devRelease,
          asset,
          saveAsFileName: "dev-wasm-als",
        })
      }
    }
  }
}

// download the dev WASM ALS and return the path of the downloaded file
let download = (memento, globalStorageUri) => async () =>
  switch await getDownloadDescriptor(memento, globalStorageUri) {
  | Error(error) => Error(error)
  | Ok(downloadDescriptor) =>
    await Connection__Download.download(globalStorageUri, downloadDescriptor)
  }
