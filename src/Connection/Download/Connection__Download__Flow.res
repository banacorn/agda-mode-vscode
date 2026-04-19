type variant =
  | Native
  | WASM

let assetsForVariant = (release, platform, variant) =>
  switch variant {
  | Native => Connection__Download__Assets.nativeForPlatform(release, platform)
  | WASM => Connection__Download__Assets.wasm(release)
  }

let sourceForSelection = async (
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
  platformDeps: Platform.t,
  ~channel: Connection__Download.Channel.t,
  ~variant: variant,
  ~versionString: string,
): result<Connection__Download.Source.t, Connection__Download.Error.t> => {
  module PlatformOps = unpack(platformDeps)

  switch await PlatformOps.determinePlatform() {
  | Error(_) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
  | Ok(platform) =>
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, platform) {
    | Error(error) => Error(error)
    | Ok(Connection__Download.Source.FromURL(_, _, _) as source) => Ok(source)
    | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
      let release = descriptor.release
      let assets = assetsForVariant(release, platform, variant)
      let matchingSource = assets->Array.reduce(None, (found, asset) =>
        switch found {
        | Some(_) => found
        | None =>
          let source = Connection__Download.Source.FromGitHub(channel, {
            Connection__Download__GitHub.DownloadDescriptor.asset,
            release,
            saveAsFileName: descriptor.saveAsFileName,
          })
          if Connection__Download.Source.toVersionString(source) == versionString {
            Some(source)
          } else {
            None
          }
        }
      )

      switch matchingSource {
      | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Some(source) => Ok(source)
      }
    }
  }
}
