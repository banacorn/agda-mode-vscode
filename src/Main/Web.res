// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    let unsupportedRaw = {"os": "web", "dist": "browser", "codename": "web", "release": "1.0"}
    Promise.resolve(Error(unsupportedRaw))
  }

  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(Error(Connection__Command.Error.NotFound))

  let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

  let downloadLatestALS = (_logChannel, _memento, _globalStorageUri) => _platform =>
    Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

  let getFetchSpec = (_memento, _globalStorageUri, _platform) =>
    Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.No)

  let openFolder = _uri => Promise.resolve()
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
