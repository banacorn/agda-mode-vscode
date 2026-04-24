let getAgdaVersionFromAssetName = (asset: Connection__Download__GitHub.Asset.t) =>
  Connection__Download__DownloadArtifact.parseName(asset.name)->Option.mapOr(
    "",
    artifact => artifact.agdaVersion,
  )

let nativeForPlatform = (
  release: Connection__Download__GitHub.Release.t,
  platform,
): array<Connection__Download__GitHub.Asset.t> => {
  release.assets
  ->Array.filter(asset =>
    asset.name->String.endsWith(".zip") &&
      switch Connection__Download__DownloadArtifact.parseName(asset.name) {
      | Some(artifact) =>
        Connection__Download__DownloadArtifact.Platform.matchesDownloadPlatform(
          artifact.platform,
          platform,
        )
      | None => false
      }
  )
  ->Array.toSorted((a, b) =>
    Util.Version.compare(getAgdaVersionFromAssetName(b), getAgdaVersionFromAssetName(a))
  )
}

let wasm = (release: Connection__Download__GitHub.Release.t): array<
  Connection__Download__GitHub.Asset.t,
> => {
  release.assets
  ->Array.filter(asset =>
    asset.name->String.endsWith(".wasm") &&
      switch Connection__Download__DownloadArtifact.parseName(asset.name) {
      | Some(artifact) => artifact.platform == Connection__Download__DownloadArtifact.Platform.Wasm
      | None => false
      }
  )
  ->Array.toSorted((a, b) =>
    Util.Version.compare(getAgdaVersionFromAssetName(b), getAgdaVersionFromAssetName(a))
  )
}

let forPlatform = (
  release: Connection__Download__GitHub.Release.t,
  platform,
): array<Connection__Download__GitHub.Asset.t> =>
  switch platform {
  | Connection__Download__Platform.Web => wasm(release)
  | _ => nativeForPlatform(release, platform)
  }
