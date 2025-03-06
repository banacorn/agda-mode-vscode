type t =
  // Agda
  | Agda(Connection__Target__Agda__Error.t, string)
  // ALS
  | ALS(Connection__Target__ALS__Error.t)
  // Connection
  | CommandsNotFound(array<(string, option<Connection__Process__Exec.Error.t>)>)
  | CannotHandleURLsATM(string)
  | NotAgdaOrALS(string)
  | ValidationError(string, Connection__Process__Exec.Error.t)
  // Download
  | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
  | CannotDownloadALS(Connection__Download__GitHub.Error.t)
  | CannotFindCompatibleALSRelease

let toString = x =>
  switch x {
  | Agda(e, _) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | CommandsNotFound(pairs) => (
      "Cannot find the `agda` or `als` commands",
      pairs
      ->Array.map(((command, error)) =>
        switch error {
          | None => "Cannot find `" ++ command ++ "` in PATH"
          | Some(e) =>
            "Cannot find `" ++
            command ++
            "` because: " ++
            Connection__Process__Exec.Error.toString(e) ++ "."
        }
      )
      ->Array.join(
        "\n",
      ) ++ "\n\nIf `agda` or `als` is installed somewhere outside of PATH, please add the path to the configuration at `agdaMode.connection.paths`.",
    )
  | CannotFetchALSReleases(e) => (
      "Cannot fetch releases of Agda Language Server",
      Connection__Download__GitHub.Error.toString(e),
    )
  | CannotFindCompatibleALSRelease => (
      "Cannot find compatible Agda Language Server release for download",
      "Prebuilts are only available for download on Ubuntu, Windows, and macOS (arm64, x64).\nPlease build from source if you are on a different platform. \nSee https://github.com/agda/agda-language-server for more information.",
    )
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
  | ValidationError(_, e) => ("Error", Connection__Process__Exec.Error.toString(e))
  }
