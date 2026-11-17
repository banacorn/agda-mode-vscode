type t =
  | OptedNotToDownload
  | PlatformNotSupported(Connection__Download__Platform.host)
  | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
  | CannotDownloadALS(Connection__Download__GitHub.Error.t)
  | CannotFindCompatibleALSRelease
  | CannotDownloadFromURL(Connection__Download__GitHub.Error.t)

// the host, like "ubuntu 22.04 on x64"
let describeHost = (host: Connection__Download__Platform.host) => {
  let system = switch (host.os, host.osRelease) {
  | ("linux", Some({id: Some(id), versionId: Some(versionId)})) => id ++ " " ++ versionId
  | ("linux", Some({id: Some(id), versionId: None})) => id
  | ("linux", _) => "an unknown Linux distribution"
  | (os, _) => os
  }
  system ++ " on " ++ host.arch
}

// the first check of the native ALS that the host fails
let whyNoNativeALS = (host: Connection__Download__Platform.host) =>
  switch (host.os, host.osRelease) {
  | ("linux", None) => "the distribution could not be read from /etc/os-release"
  | ("linux", Some({id: Some("ubuntu"), versionId})) =>
    let newEnough = versionId->Option.mapOr(false, Connection__Download__Platform.isUbuntu2404OrLater)
    if !newEnough {
      "it needs Ubuntu 24.04 or later"
    } else {
      "it needs x86_64"
    }
  | ("linux", Some(_)) => "only Ubuntu is supported"
  | _ => "there is no native build for it"
  }

let toString = x =>
  switch x {
  | OptedNotToDownload => "Opted not to download the Agda Language Server"
  | PlatformNotSupported(host) =>
    "No native Agda Language Server for " ++
    describeHost(host) ++
    ": " ++
    whyNoNativeALS(host) ++ ". Add a local Agda or ALS to `agdaMode.connection.paths` instead.\n"
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
