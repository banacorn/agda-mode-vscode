// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    let unsupportedRaw = {"os": "web", "dist": "browser", "codename": "web", "release": "1.0"}
    Promise.resolve(Error(unsupportedRaw))
  }

  let findCommands = _commands =>
    Promise.resolve(
      Error([Connection__Command.Error.NotFound("Platform not supported in web environment")]),
    )
  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(
      Error(Connection__Command.Error.NotFound("Platform not supported in web environment")),
    )

  let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

  let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
    Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

  let getInstalledEndpointsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.fromArray([]))
  let getInstalledEndpointsAndPersistThem2 = _globalStorageUri => Promise.resolve(Dict.make())

  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.No)
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
