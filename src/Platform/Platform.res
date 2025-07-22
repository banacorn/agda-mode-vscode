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
// Type alias for platform dependencies to be used throughout the codebase
type t = module(PlatformOps)
