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

module Aggregated = {
  type commandAttempt = {
    command: string,
    error: Connection__Process__Exec.Error.t,
  }

  type targetAttempt = {
    uri: Connection.URI.t,
    error: t,
  }

  type attempts = {
    targets: array<targetAttempt>,
    agda: commandAttempt,
    als: commandAttempt,
  }

  let attempsToString = attempts => {
    if attempts.targets->Array.length == 0 {
      "Tried to connect with the path from the configuration but there are none.\n"
    } else {
      "Tried to connect with the path from the configuration but all failed:\n" ++
      attempts.targets
      ->Array.map(attempt => Connection__URI.toString(attempt.uri) ++ snd(toString(attempt.error)))
      ->Array.join("\n")
    } ++
    "\nTried to run the `agda` command but failed:\n" ++
    snd(toString(CommandsNotFound([("agda", Some(attempts.agda.error))]))) ++
    "Tried to run the `als` command but failed:\n" ++
    snd(toString(CommandsNotFound([("als", Some(attempts.agda.error))])))
  }

  // Steps for connecting to Agda or ALS:
  // 1. Go through the list of targets in the configuration, use the first one that works
  // 2. Try the `agda` command, add it to the list of targets if it works, else proceed to 3.
  // 3. Try the `als` command, add it to the list of targets if it works, else proceed to 4.
  // 4. See if the platform is supported:
  //      No  : exit with the `PlatformNotSupported` error ❌
  //      Yes : proceed to 5.
  // 5. Check the download policy:
  //      Undecided : ask the user if they want to download ALS or not, go back to 5.
  //      No        : exit with the `NoDownloadALS` error ❌
  //      Yes       : download the ALS, add it to the list of targets if it works, exit with the `DownloadALS` error ❌
  type t =
    | PlatformNotSupported(attempts, string)
    | NoDownloadALS(attempts)
    | DownloadALS(attempts, t)

  let toString = x =>
    switch x {
    | PlatformNotSupported(attempts, platform) =>
      attempsToString(attempts) ++
      "\nTried to download the Agda Language Server but the platform `" ++
      platform ++ "` is not supported.\n"

    | NoDownloadALS(attempts) =>
      attempsToString(
        attempts,
      ) ++ "\nPrebuilt Agda Language Server available for download but you opted not to.\n"

    | DownloadALS(attempts, error) =>
      attempsToString(attempts) ++
      "\nTried to download the Agda Language Server but failed:\n" ++
      snd(toString(error))
    }
}
