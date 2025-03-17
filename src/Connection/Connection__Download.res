module Error = {
  type t =
    | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
    | CannotDownloadALS(Connection__Download__GitHub.Error.t)
    | CannotConnectToALS(Connection__Target.Error.t)
    | CannotFindCompatibleALSRelease

  let toString = x =>
    switch x {
    | CannotFetchALSReleases(e) =>
      "Cannot fetch releases of Agda Language Server: " ++
      Connection__Download__GitHub.Error.toString(e)

    | CannotFindCompatibleALSRelease => "Cannot find compatible Agda Language Server release for download. Prebuilts are only available for download on Ubuntu, Windows, and macOS (arm64, x64).\nPlease build from source if you are on a different platform. \nSee https://github.com/agda/agda-language-server for more information."
    | CannotConnectToALS(e) => Connection__Target.Error.toString(e)
    | CannotDownloadALS(e) =>
      "Failed download the Agda Language Server: " ++ Connection__Download__GitHub.Error.toString(e)
    }
}