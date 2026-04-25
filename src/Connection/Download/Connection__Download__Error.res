type t =
  | OptedNotToDownload
  | PlatformNotSupported(Connection__Download__Platform.raw)
  | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
  | CannotDownloadALS(Connection__Download__GitHub.Error.t)
  | CannotFindCompatibleALSRelease
  | CannotDownloadFromURL(Connection__Download__GitHub.Error.t)

let toString = x =>
  switch x {
  | OptedNotToDownload => "Opted not to download the Agda Language Server"
  | PlatformNotSupported(platform) =>
    "The platform `" ++
    platform["os"] ++
    "/" ++
    platform["dist"] ++ "` is not supported for downloading the Agda Language Server.\n"
  | CannotFetchALSReleases(e) =>
    "Cannot fetch releases of Agda Language Server: " ++
    Connection__Download__GitHub.Error.toString(e)
  | CannotFindCompatibleALSRelease => "Cannot find compatible Agda Language Server release for download. Prebuilts are only available for download on Ubuntu, Windows, and macOS (arm64, x64).\nPlease build from source if you are on a different platform. \nSee https://github.com/agda/agda-language-server for more information."
  | CannotDownloadALS(e) =>
    "Failed to download the Agda Language Server: " ++
    Connection__Download__GitHub.Error.toString(e)
  | CannotDownloadFromURL(e) =>
    "Failed to download from URL: " ++ Connection__Download__GitHub.Error.toString(e)
  }
