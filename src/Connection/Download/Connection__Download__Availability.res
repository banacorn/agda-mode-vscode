type availableDownload = {
  downloaded: bool,
  versionString: string,
  variant: Connection__Download.SelectionVariant.t,
}

type sourceDownloadItem = {
  download: availableDownload,
  source: option<Connection__Download.Source.t>,
}

let isDownloadedSource = async (
  globalStorageUri: VSCode.Uri.t,
  source: Connection__Download.Source.t,
): bool =>
  switch await FS.stat(Connection__Download.expectedUriForSource(globalStorageUri, source)) {
  | Ok(_) => true
  | Error(_) => false
  }

let makeDownloadItem = async (
  globalStorageUri: VSCode.Uri.t,
  variant: Connection__Download.SelectionVariant.t,
  source: Connection__Download.Source.t,
): sourceDownloadItem => {
  let downloaded = await isDownloadedSource(globalStorageUri, source)
  {
    download: {
      downloaded,
      versionString: Connection__Download.Source.toVersionString(source),
      variant,
    },
    source: Some(source),
  }
}

let unavailableSourceItem = (
  ~downloadUnavailable: string,
  variant: Connection__Download.SelectionVariant.t,
): sourceDownloadItem => {
  download: {
    downloaded: false,
    versionString: downloadUnavailable,
    variant,
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
  ~channel: Connection__Download.Channel.t=DevALS,
  ~downloadUnavailable: string,
): array<availableDownload> => {
  module PlatformOps = unpack(platformDeps)

  let unavailable = variant => unavailableSourceItem(~downloadUnavailable, variant)

  let allItems = switch await PlatformOps.determinePlatform() {
  | Error(_) => [
      unavailable(Connection__Download.SelectionVariant.Native),
      unavailable(Connection__Download.SelectionVariant.WASM),
    ]
  | Ok(Connection__Download__Platform.Web) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, Connection__Download__Platform.Web) {
    | Error(_) => [unavailable(Connection__Download.SelectionVariant.WASM)]
    | Ok(Connection__Download.Source.FromURL(_, _, _)) =>
      [unavailable(Connection__Download.SelectionVariant.WASM)]
    | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
      let release = descriptor.release
      let makeSource = asset =>
        Connection__Download.Source.FromGitHub(channel, {
          Connection__Download__GitHub.DownloadDescriptor.asset: asset,
          release: release,
          saveAsFileName: descriptor.saveAsFileName,
        })
      let wasmAssets = Connection__Download__Assets.wasm(release)
      if Array.length(wasmAssets) == 0 {
        [unavailable(Connection__Download.SelectionVariant.WASM)]
      } else {
        await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(
              globalStorageUri,
              Connection__Download.SelectionVariant.WASM,
              makeSource(asset),
            )
          ),
        )
      }
    }
  | Ok(platform) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, platform) {
    | Error(_) => [
        unavailable(Connection__Download.SelectionVariant.Native),
        unavailable(Connection__Download.SelectionVariant.WASM),
      ]
    | Ok(Connection__Download.Source.FromURL(_, _, _)) =>
      [
        unavailable(Connection__Download.SelectionVariant.Native),
        unavailable(Connection__Download.SelectionVariant.WASM),
      ]
    | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
      let release = descriptor.release
      let makeSource = asset =>
        Connection__Download.Source.FromGitHub(channel, {
          Connection__Download__GitHub.DownloadDescriptor.asset: asset,
          release: release,
          saveAsFileName: descriptor.saveAsFileName,
        })
      let nativeAssets = Connection__Download__Assets.nativeForPlatform(release, platform)
      let wasmAssets = Connection__Download__Assets.wasm(release)
      let nativeItems = switch channel {
      | Connection__Download.Channel.DevALS =>
        await Promise.all(
          nativeAssets->Array.map(async asset =>
            await makeDownloadItem(
              globalStorageUri,
              Connection__Download.SelectionVariant.Native,
              makeSource(asset),
            )
          ),
        )
      | Connection__Download.Channel.LatestALS =>
        if Array.length(nativeAssets) == 0 {
          [unavailable(Connection__Download.SelectionVariant.Native)]
        } else {
          await Promise.all(
            nativeAssets->Array.map(async asset =>
              await makeDownloadItem(
                globalStorageUri,
                Connection__Download.SelectionVariant.Native,
                makeSource(asset),
              )
            ),
          )
        }
      }
      let wasmItems = switch channel {
      | Connection__Download.Channel.DevALS =>
        await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(
              globalStorageUri,
              Connection__Download.SelectionVariant.WASM,
              makeSource(asset),
            )
          ),
        )
      | Connection__Download.Channel.LatestALS =>
        if Array.length(wasmAssets) == 0 {
          [unavailable(Connection__Download.SelectionVariant.WASM)]
        } else {
          await Promise.all(
            wasmAssets->Array.map(async asset =>
              await makeDownloadItem(
                globalStorageUri,
                Connection__Download.SelectionVariant.WASM,
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
