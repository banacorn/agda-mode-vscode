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
  let alreadyDownloaded: (
    VSCode.Uri.t,
    Connection__Download.DownloadOrderAbstract.t,
  ) => promise<option<string>>
  let resolveDownloadOrder: (
    Connection__Download.DownloadOrderAbstract.t,
    bool,
  ) => (
    Memento.t,
    VSCode.Uri.t,
    Connection__Download__Platform.t,
  ) => promise<result<Connection__Download.DownloadOrderConcrete.t, Connection__Download.Error.t>>
  let download: (
    VSCode.Uri.t,
    Connection__Download.DownloadOrderConcrete.t,
  ) => promise<result<string, Connection__Download.Error.t>>

  // User interaction
  let askUserAboutDownloadPolicy: unit => promise<Config.Connection.DownloadPolicy.t>
}
// Type alias for platform dependencies to be used throughout the codebase
type t = module(PlatformOps)
