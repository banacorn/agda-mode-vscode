type t =
  // Agda
  | Agda(Connection__Target__Agda__Error.t, string)
  // ALS
  | ALS(Connection__Target__ALS__Error.t)
  // Connection
  | CannotFindALSorAgda
  | CannotHandleURLsATM(string)
  | NotAgdaOrALS(string)
  | ValidationError(string, Connection__Validation.Error.t)
  // Download
  | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
  | CannotFindLatestALSRelease
  | CannotDownloadALS(Connection__Download__GitHub.Error.t)

let toString = x =>
  switch x {
  | Agda(e, _) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | CannotFindALSorAgda => (
      "Cannot find Agda or Agda Language Server",
      "Please make sure that either `agda` or `als` is in your PATH",
    )
  | CannotFetchALSReleases(e) => (
      "Cannot fetch releases of Agda Language Server",
      Connection__Download__GitHub.Error.toString(e),
    )
  | CannotFindLatestALSRelease => ("Cannot find the latest release of Agda Language Server", "")
  | CannotDownloadALS(e) => (
      "Failed download the Agda Language Server",
      Connection__Download__GitHub.Error.toString(e),
    )
  | CannotHandleURLsATM(_) => (
      "Cannot handle URLs at the moment",
      "This will be supported again in the future",
    )
  | NotAgdaOrALS(path) => (
      "Not Agda or Agda Language Server",
      "`" ++ path ++ "` doesn't seem to be an Agda executable or an Agda Language Server",
    )
  | ValidationError(_, e) => ("Error", Connection__Validation.Error.toString(e))
  }
