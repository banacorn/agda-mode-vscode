// Platform abstraction layer for dependency injection
// This module defines interfaces for all platform-dependent operations
// and provides implementations for different environments (Desktop, Web)

module type PlatformOps = {
  // Platform detection
  let determinePlatform: unit => promise<
    result<Connection__Download__Platform.t, Connection__Download__Platform.raw>,
  >

  // Command/executable searching
  let findCommand: (string, ~timeout: int=?) => promise<result<string, Connection__Command.Error.t>>

  // Download operations
  let alreadyDownloaded: VSCode.Uri.t => unit => promise<option<string>> // returns the path to ALS if already downloaded
  let downloadLatestALS: (
    Chan.t<Log.t>,
    Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__Platform.t => promise<result<string, Connection__Download.Error.t>> // returns the path to the downloaded ALS executable on success
  let getFetchSpec: (
    Memento.t,
    VSCode.Uri.t,
    Connection__Download__Platform.t,
  ) => promise<result<Connection__Download__GitHub.FetchSpec.t, Connection__Download.Error.t>>

  // User interaction
  let askUserAboutDownloadPolicy: unit => promise<Config.Connection.DownloadPolicy.t>

  // File system UI
  let openFolder: VSCode.Uri.t => promise<unit> // Opens a folder in the file explorer
}
// Type alias for platform dependencies to be used throughout the codebase
type t = module(PlatformOps)
