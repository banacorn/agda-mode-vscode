type availableDownload = {
  downloaded: bool,
  versionString: string,
  platform: Connection__Download__DownloadArtifact.Platform.t,
}

type sourceDownloadItem = {
  download: availableDownload,
  source: option<Connection__Download__Source.t>,
}

let isDownloadedSource = async (
  globalStorageUri: VSCode.Uri.t,
  source: Connection__Download__Source.t,
): bool =>
  switch await FS.stat(Connection__Download.expectedUriForSource(globalStorageUri, source)) {
  | Ok(_) => true
  | Error(_) => false
  }

let makeDownloadItem = async (
  globalStorageUri: VSCode.Uri.t,
  ~platform: Connection__Download__DownloadArtifact.Platform.t,
  source: Connection__Download__Source.t,
): sourceDownloadItem => {
  let downloaded = await isDownloadedSource(globalStorageUri, source)
  {
    download: {
      downloaded,
      versionString: Connection__Download__Source.toVersionString(source),
      platform,
    },
    source: Some(source),
  }
}

let unavailableSourceItem = (
  ~downloadUnavailable: string,
  ~platform: Connection__Download__DownloadArtifact.Platform.t,
): sourceDownloadItem => {
  download: {
    downloaded: false,
    versionString: downloadUnavailable,
    platform,
  },
  source: None,
}

let suppressManagedVariants = async (
  globalStorageUri: VSCode.Uri.t,
  configPaths: array<string>,
  downloadItems: array<sourceDownloadItem>,
): array<sourceDownloadItem> => {
  let shouldKeep = async item => {
    switch item.source {
    | None => true
    | Some(source) =>
      let expectedPath = Connection__Download.expectedPathForSource(globalStorageUri, source)
      let expectedCandidate = Connection__Candidate.make(expectedPath)
      let inConfig = configPaths->Array.some(configPath =>
        Connection__Candidate.equal(Connection__Candidate.make(configPath), expectedCandidate)
      )
      if !inConfig {
        true
      } else {
        let fileExists =
          (await FS.stat(Connection__Download.expectedUriForSource(globalStorageUri, source)))
          ->Result.isOk
        !fileExists
      }
    }
  }
  let keeps = await Promise.all(downloadItems->Array.map(shouldKeep))
  downloadItems->Array.filterWithIndex((_, i) => Belt.Array.getExn(keeps, i))
}

let getAll = async (
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
  platformDeps: Platform.t,
  configPaths: array<string>,
  ~channel: Connection__Download__Channel.t=DevALS,
  ~downloadUnavailable: string,
): array<availableDownload> => {
  module PlatformOps = unpack(platformDeps)

  let unavailable = (~platform) => unavailableSourceItem(~downloadUnavailable, ~platform)

  let allItems = switch await PlatformOps.determinePlatform() {
  | Error(_) => [unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm)]
  | Ok(Connection__Download__Platform.Web) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, Connection__Download__Platform.Web) {
    | Error(_) => [unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm)]
    | Ok(Connection__Download__Source.FromURL(_, _, _)) =>
      [unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm)]
    | Ok(Connection__Download__Source.FromGitHub(_, descriptor)) =>
      let release = descriptor.release
      let makeSource = asset =>
        Connection__Download__Source.FromGitHub(channel, {
          Connection__Download__GitHub.DownloadDescriptor.asset: asset,
          release: release,
          saveAsFileName: descriptor.saveAsFileName,
        })
      let wasmAssets = Connection__Download__Assets.wasm(release)
      if Array.length(wasmAssets) == 0 {
        [unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm)]
      } else {
        await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(
              globalStorageUri,
              ~platform=Connection__Download__DownloadArtifact.Platform.Wasm,
              makeSource(asset),
            )
          ),
        )
      }
    }
  | Ok(downloadPlatform) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, downloadPlatform) {
    | Error(_) => [
        unavailable(~platform=Connection__Download__DownloadArtifact.Platform.fromDownloadPlatform(downloadPlatform)),
        unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm),
      ]
    | Ok(Connection__Download__Source.FromURL(_, _, _)) =>
      [
        unavailable(~platform=Connection__Download__DownloadArtifact.Platform.fromDownloadPlatform(downloadPlatform)),
        unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm),
      ]
    | Ok(Connection__Download__Source.FromGitHub(_, descriptor)) =>
      let release = descriptor.release
      let makeSource = asset =>
        Connection__Download__Source.FromGitHub(channel, {
          Connection__Download__GitHub.DownloadDescriptor.asset: asset,
          release: release,
          saveAsFileName: descriptor.saveAsFileName,
        })
      let nativePlatform = Connection__Download__DownloadArtifact.Platform.fromDownloadPlatform(downloadPlatform)
      let nativeAssets = Connection__Download__Assets.nativeForPlatform(release, downloadPlatform)
      let wasmAssets = Connection__Download__Assets.wasm(release)
      let nativeItems = switch channel {
      | Connection__Download__Channel.DevALS =>
        await Promise.all(
          nativeAssets->Array.map(async asset =>
            await makeDownloadItem(
              globalStorageUri,
              ~platform=nativePlatform,
              makeSource(asset),
            )
          ),
        )
      | Connection__Download__Channel.LatestALS =>
        if Array.length(nativeAssets) == 0 {
          [unavailable(~platform=nativePlatform)]
        } else {
          await Promise.all(
            nativeAssets->Array.map(async asset =>
              await makeDownloadItem(
                globalStorageUri,
                ~platform=nativePlatform,
                makeSource(asset),
              )
            ),
          )
        }
      }
      let wasmItems = switch channel {
      | Connection__Download__Channel.DevALS =>
        await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(
              globalStorageUri,
              ~platform=Connection__Download__DownloadArtifact.Platform.Wasm,
              makeSource(asset),
            )
          ),
        )
      | Connection__Download__Channel.LatestALS =>
        if Array.length(wasmAssets) == 0 {
          [unavailable(~platform=Connection__Download__DownloadArtifact.Platform.Wasm)]
        } else {
          await Promise.all(
            wasmAssets->Array.map(async asset =>
              await makeDownloadItem(
                globalStorageUri,
                ~platform=Connection__Download__DownloadArtifact.Platform.Wasm,
                makeSource(asset),
              )
            ),
          )
        }
      }
      Array.concat(nativeItems, wasmItems)
    }
  }

  let filteredItems = await suppressManagedVariants(globalStorageUri, configPaths, allItems)
  filteredItems->Array.map(item => item.download)
}
