type variant =
  | Native
  | WASM

let variantToTag = variant =>
  switch variant {
  | Native => "native"
  | WASM => "wasm"
  }

type sourceDownloadItem = {
  downloaded: bool,
  versionString: string,
  variantTag: string,
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
  variant: variant,
  source: Connection__Download.Source.t,
): sourceDownloadItem => {
  let downloaded = await isDownloadedSource(globalStorageUri, source)
  {
    downloaded,
    versionString: Connection__Download.Source.toVersionString(source),
    variantTag: variantToTag(variant),
    source: Some(source),
  }
}

let unavailableSourceItem = (~downloadUnavailable: string, variant: variant): sourceDownloadItem => {
  downloaded: false,
  versionString: downloadUnavailable,
  variantTag: variantToTag(variant),
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
): array<(bool, string, string)> => {
  module PlatformOps = unpack(platformDeps)

  let unavailable = variant => unavailableSourceItem(~downloadUnavailable, variant)

  let allItems = switch await PlatformOps.determinePlatform() {
  | Error(_) => [unavailable(Native), unavailable(WASM)]
  | Ok(Connection__Download__Platform.Web) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, Connection__Download__Platform.Web) {
    | Error(_) => [unavailable(WASM)]
    | Ok(Connection__Download.Source.FromURL(_, _, _)) => [unavailable(WASM)]
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
        [unavailable(WASM)]
      } else {
        await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(globalStorageUri, WASM, makeSource(asset))
          ),
        )
      }
    }
  | Ok(platform) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, platform) {
    | Error(_) => [unavailable(Native), unavailable(WASM)]
    | Ok(Connection__Download.Source.FromURL(_, _, _)) =>
      [unavailable(Native), unavailable(WASM)]
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
            await makeDownloadItem(globalStorageUri, Native, makeSource(asset))
          ),
        )
      | Connection__Download.Channel.LatestALS =>
        if Array.length(nativeAssets) == 0 {
          [unavailable(Native)]
        } else {
          await Promise.all(
            nativeAssets->Array.map(async asset =>
              await makeDownloadItem(globalStorageUri, Native, makeSource(asset))
            ),
          )
        }
      }
      let wasmItems = switch channel {
      | Connection__Download.Channel.DevALS =>
        await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(globalStorageUri, WASM, makeSource(asset))
          ),
        )
      | Connection__Download.Channel.LatestALS =>
        if Array.length(wasmAssets) == 0 {
          [unavailable(WASM)]
        } else {
          await Promise.all(
            wasmAssets->Array.map(async asset =>
              await makeDownloadItem(globalStorageUri, WASM, makeSource(asset))
            ),
          )
        }
      }
      Array.concat(nativeItems, wasmItems)
    }
  }

  let filteredItems = await suppressManagedVariants(globalStorageUri, configPaths, allItems)
  filteredItems->Array.map(item => (item.downloaded, item.versionString, item.variantTag))
}
