// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    Promise.resolve(Ok(Connection__Download__Platform.Web))
  }

  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(Error(Connection__Command.Error.NotFound))

  let alreadyDownloaded = async globalStorageUri =>
    await Connection__Download__ManagedStorage.findAnyWasmDownloaded(globalStorageUri)

  let resolveDownloadChannel = (
    channel: Connection__Download.Channel.t,
    _useCache,
  ) => async (memento, globalStorageUri, _platform) => {
    switch channel {
    | LatestALS => {
        // Web doesn't support LatestALS (native binaries)
        Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      }
    | DevALS => {
        let repo = Connection__DevALS.makeRepo(globalStorageUri)
        switch await Connection__Download.getReleaseManifestFromGitHub(
          memento,
          repo,
          ~useCache=_useCache,
        ) {
        | Error(error) => Error(error)
        | Ok(releases) => Connection__DevALS.toDownloadOrder(releases, Connection__Download__Platform.Web)
        }
      }
    }
  }

  let download = (globalStorageUri, channel, ~trace=Connection__Download__Trace.noop) =>
    Connection__Download.download(globalStorageUri, channel, ~trace)

  // Always download, because there are no alternatives in web environment
  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
