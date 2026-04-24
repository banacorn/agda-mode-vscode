let sourceForSelection = async (
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
  platformDeps: Platform.t,
  ~channel: Connection__Download__Channel.t,
  ~platform: Connection__Download__DownloadArtifact.Platform.t,
  ~versionString: string,
): result<Connection__Download__Source.t, Connection__Download__Error.t> => {
  module PlatformOps = unpack(platformDeps)

  switch await PlatformOps.determinePlatform() {
  | Error(_) => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
  | Ok(downloadPlatform) =>
    let platformOk = switch platform {
    | Connection__Download__DownloadArtifact.Platform.Wasm => true
    | nativePlatform =>
      Connection__Download__DownloadArtifact.Platform.matchesDownloadPlatform(
        nativePlatform,
        downloadPlatform,
      )
    }
    if !platformOk {
      Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
    } else {
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, downloadPlatform) {
    | Error(error) => Error(error)
    | Ok(Connection__Download__Source.FromURL(_, _, _) as source) => Ok(source)
    | Ok(Connection__Download__Source.FromGitHub(_, descriptor)) =>
      let release = descriptor.release
      let assets = switch platform {
      | Connection__Download__DownloadArtifact.Platform.Wasm =>
        Connection__Download__Assets.wasm(release)
      | _ => Connection__Download__Assets.nativeForPlatform(release, downloadPlatform)
      }
      let matchingSource = assets->Array.reduce(None, (found, asset) =>
        switch found {
        | Some(_) => found
        | None =>
          let source = Connection__Download__Source.FromGitHub(channel, {
            Connection__Download__GitHub.DownloadDescriptor.asset,
            release,
            saveAsFileName: descriptor.saveAsFileName,
          })
          if Connection__Download__Source.toVersionString(source) == versionString {
            Some(source)
          } else {
            None
          }
        }
      )

      switch matchingSource {
      | None => Error(Connection__Download__Error.CannotFindCompatibleALSRelease)
      | Some(source) => Ok(source)
      }
    }
    }
  }
}
