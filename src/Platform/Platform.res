// Platform abstraction layer for dependency injection
// This module defines interfaces for all platform-dependent operations
// and provides implementations for different environments (Desktop, Web)

module type PlatformOps = {
  // Platform detection
  let determinePlatform: unit => promise<
    result<Connection__Download__Platform.t, Connection__Download__Platform.raw>,
  >

  // Command/executable searching
  let findCommands: array<string> => promise<
    result<Connection__Target.t, array<Connection__Command.Error.t>>,
  >

  // Download operations
  let alreadyDownloaded: VSCode.Uri.t => unit => promise<option<Connection__Target.t>>
  let downloadLatestALS: (
    State__Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__Platform.t => promise<
    result<Connection__Target.t, Connection__Download.Error.t>,
  >

  // Target operations
  let getInstalledTargetsAndPersistThem: VSCode.Uri.t => promise<
    Dict.t<result<Connection__Target.t, Connection__Target.Error.t>>,
  >

  // User interaction
  let askUserAboutDownloadPolicy: unit => promise<Config.Connection.DownloadPolicy.t>
}

// Desktop implementation - uses actual platform-specific code
module Desktop: PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommands = Connection__Command.findCommands

  let alreadyDownloaded = globalStorageUri =>
    Connection__LatestALS.alreadyDownloaded(globalStorageUri)

  let downloadLatestALS = (memento, globalStorageUri) =>
    Connection__LatestALS.download(memento, globalStorageUri)

  let getInstalledTargetsAndPersistThem = globalStorageUri =>
    Connection__TargetManager.getInstalledTargetsAndPersistThem(findCommands, globalStorageUri)

  let askUserAboutDownloadPolicy = async () => {
    let messageOptions = {
      VSCode.MessageOptions.modal: true,
      detail: "Do you want to download and install the latest Agda Language Server?",
    }
    let result = await VSCode.Window.showWarningMessageWithOptions(
      "Cannot find Agda or Agda Language Server",
      messageOptions,
      [
        Config.Connection.DownloadPolicy.toString(Yes),
        Config.Connection.DownloadPolicy.toString(No),
      ],
    ) // 📺

    // parse the result
    result->Option.mapOr(
      Config.Connection.DownloadPolicy.No,
      Config.Connection.DownloadPolicy.fromString,
    )
  }
}

// Web implementation - provides mock/limited functionality for browser environment
module Web: PlatformOps = {
  let determinePlatform = () => {
    let unsupportedRaw = {"os": "web", "dist": "browser", "codename": "web", "release": "1.0"}
    Promise.resolve(Error(unsupportedRaw))
  }

  let findCommands = _commands =>
    Promise.resolve(
      Error([Connection__Command.Error.NotFound("Platform not supported in web environment")]),
    )

  let alreadyDownloaded = _globalStorageUri => () => Promise.resolve(None)

  let downloadLatestALS = (_memento, _globalStorageUri) => _platform =>
    Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))

  let getInstalledTargetsAndPersistThem = _globalStorageUri => Promise.resolve(Dict.fromArray([]))

  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.No)
}

// Type alias for platform dependencies to be used throughout the codebase
type platformDeps = module(PlatformOps)

// Helper functions to create platform instances
let makeDesktop = (): platformDeps => module(Desktop)
let makeWeb = (): platformDeps => module(Web)

// Function to determine the appropriate platform implementation
// This can be used for runtime platform detection if needed
let makePlatform = (~isWeb: bool): platformDeps =>
  if isWeb {
    makeWeb()
  } else {
    makeDesktop()
  }
