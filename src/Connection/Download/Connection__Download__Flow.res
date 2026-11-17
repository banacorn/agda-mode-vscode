let sourceForSelection = async (
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
  platformDeps: Platform.t,
  ~channel: Connection__Download__Channel.t,
  ~platform: Connection__Download__DownloadArtifact.Platform.t,
  ~versionString: string,
  ~onEvent: Log.Connection.DownloadFlow.t => unit=_ => (),
): result<Connection__Download__Source.t, Connection__Download__Error.t> => {
  module PlatformOps = unpack(platformDeps)

  // a host without a native ALS can still select the WASM ALS, whose assets `Web` resolves
  let channelName = Connection__Download__Channel.toString(channel)
  let downloadPlatform = switch await PlatformOps.determinePlatform() {
  | Ok(downloadPlatform) => Ok(downloadPlatform)
  | Error(host) =>
    platform == Connection__Download__DownloadArtifact.Platform.Wasm
      ? Ok(Connection__Download__Platform.Web)
      : Error(host)
  }

  switch downloadPlatform {
  | Error(host) => Error(Connection__Download__Error.PlatformNotSupported(host))
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
      Error(Connection__Download__Error.NoMatchingAsset(channelName, downloadPlatform))
    } else {
    let resolver = PlatformOps.resolveDownloadChannel(channel, true)
    switch await resolver(memento, globalStorageUri, downloadPlatform) {
    | Error(error) => Error(error)
    | Ok(Connection__Download__Source.FromURL(_, _, _) as source) =>
      onEvent(
        Log.Connection.DownloadFlow.SourceResolved(
          Log.Connection.DownloadFlow.URL,
          Connection__Download__Source.toVersionString(source),
        ),
      )
      Ok(source)
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
      | None =>
        if platform != Connection__Download__DownloadArtifact.Platform.Wasm {
          switch Connection__Download__Assets.wasm(release)->Array.get(0) {
          | None => Error(Connection__Download__Error.NoMatchingSelection(versionString, channelName))
          | Some(wasmAsset) =>
            let wasmSource = Connection__Download__Source.FromGitHub(channel, {
              Connection__Download__GitHub.DownloadDescriptor.asset: wasmAsset,
              release,
              saveAsFileName: descriptor.saveAsFileName,
            })
            let wasmVersion = Connection__Download__Source.toVersionString(wasmSource)
            onEvent(Log.Connection.DownloadFlow.FallbackChosen(wasmVersion))
            onEvent(
              Log.Connection.DownloadFlow.SourceResolved(
                Log.Connection.DownloadFlow.GitHub,
                wasmVersion,
              ),
            )
            Ok(wasmSource)
          }
        } else {
          Error(Connection__Download__Error.NoMatchingSelection(versionString, channelName))
        }
      | Some(source) =>
        onEvent(
          Log.Connection.DownloadFlow.SourceResolved(
            Log.Connection.DownloadFlow.GitHub,
            Connection__Download__Source.toVersionString(source),
          ),
        )
        Ok(source)
      }
    }
    }
  }
}
