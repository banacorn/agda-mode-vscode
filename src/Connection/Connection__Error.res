module Aggregated = {
  type targetAttempt = {
    uri: Connection__URI.t,
    error: Connection__Target.Error.t,
  }

  type attempts = {
    targets: array<targetAttempt>,
    commands: array<Connection__Command.Error.t>,
  }

  let attempsToString = attempts => {
    if attempts.targets->Array.length == 0 {
      "Tried to connect with the path from the configuration but there are none.\n"
    } else {
      "Tried to connect with the path from the configuration but all failed:\n" ++
      attempts.targets
      ->Array.map(attempt =>
        Connection__URI.toString(attempt.uri) ++
        ": " ++
        Connection__Target.Error.toString(attempt.error)
      )
      ->Array.join("\n")
    } ++ if attempts.commands->Array.length == 0 {
      ""
    } else {
      attempts.commands
      ->Array.map(Connection__Command.Error.toString)
      ->Array.join("\n")
    }
  }

  type t =
    | PlatformNotSupported(attempts, Connection__Download__Platform.raw)
    | NoDownloadALS(attempts)
    | DownloadALS(attempts, Connection__Download__Error.t)

  let toString = x =>
    switch x {
    | PlatformNotSupported(attempts, platform) =>
      attempsToString(attempts) ++
      "\nTried to download the Agda Language Server but the platform `" ++
      platform["os"] ++
      "/" ++
      platform["dist"] ++ "` is not supported.\n"

    | NoDownloadALS(attempts) =>
      attempsToString(
        attempts,
      ) ++ "\nPrebuilt Agda Language Server available for download but you opted not to.\n"

    | DownloadALS(attempts, error) =>
      attempsToString(attempts) ++
      "\nTried to download the Agda Language Server but failed:\n" ++
      Connection__Download__Error.toString(error)
    }
}

type t =
  | Agda(Connection__Target__Agda__Error.t, string)
  | ALS(Connection__Target__ALS__Error.t)
  | Target(Connection__Target.Error.t)
  | Download(Connection__Download__Error.t)
  | Aggregated(Aggregated.t)

let toString = x =>
  switch x {
  | Agda(e, _) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | Target(e) => ("Error", Connection__Target.Error.toString(e))
  | Download(e) => ("Error", Connection__Download__Error.toString(e))
  | Aggregated(e) => ("Error", Aggregated.toString(e))
  }
