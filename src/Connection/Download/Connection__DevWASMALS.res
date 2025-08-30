let makeRepo = (globalStorageUri): Connection__Download__GitHub.Repo.t => {
  username: "banacorn",
  repository: "agda-language-server",
  userAgent: "banacorn/agda-mode-vscode",
  globalStorageUri,
  cacheInvalidateExpirationSecs: 3600,
}

let toDownloadOrder = (releases: array<Connection__Download__GitHub.Release.t>) => {
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
      Ok(
        Connection__Download.DownloadOrderConcrete.FromGitHub(
          DevWASMALS,
          {
            Connection__Download__GitHub.DownloadDescriptor.release: devRelease,
            asset,
            saveAsFileName: "dev-wasm-als",
          },
        ),
      )
    }
  }
}
