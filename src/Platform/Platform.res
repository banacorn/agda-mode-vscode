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
    result<Connection__Endpoint.t, array<Connection__Command.Error.t>>,
  >
  let findCommand: (string, ~timeout: int=?) => promise<result<string, Connection__Command.Error.t>>

  // Download operations
  let alreadyDownloaded: VSCode.Uri.t => unit => promise<option<Connection__Endpoint.t>>
  let alreadyDownloaded2: VSCode.Uri.t => unit => promise<option<string>> // returns the path to ALS if already downloaded
  let downloadLatestALS: (
    Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__Platform.t => promise<
    result<Connection__Endpoint.t, Connection__Download.Error.t>,
  >
  let downloadLatestALS2: (
    Memento.t,
    VSCode.Uri.t,
  ) => Connection__Download__Platform.t => promise<
    result<string, Connection__Download.Error.t>,
  > // returns the path to the downloaded ALS executable on success

  // Endpoint operations
  let getInstalledEndpointsAndPersistThem: VSCode.Uri.t => promise<
    Dict.t<result<Connection__Endpoint.t, Connection__Endpoint.Error.t>>,
  >
  let getInstalledEndpointsAndPersistThem2: VSCode.Uri.t => promise<
    Dict.t<Memento.Endpoints.endpoint>,
  >

  // User interaction
  let askUserAboutDownloadPolicy: unit => promise<Config.Connection.DownloadPolicy.t>
}
// Type alias for platform dependencies to be used throughout the codebase
type t = module(PlatformOps)
