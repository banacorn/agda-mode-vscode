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

  // Scans the filesystem for all available Agda and ALS executables from multiple sources:
  //  1. User config paths: Paths from agdaMode.connection.paths setting
  //  2. System PATH: agda and als commands found via PATH environment
  //  3. Download folder: Previously downloaded ALS executables in the global storage directory
  let getInstalledEndpoints: VSCode.Uri.t => promise<
    Dict.t<Memento.Endpoints.endpoint>,
  >

  // User interaction
  let askUserAboutDownloadPolicy: unit => promise<Config.Connection.DownloadPolicy.t>
}
// Type alias for platform dependencies to be used throughout the codebase
type t = module(PlatformOps)
