module Aggregated = {
  module Attempts = {
    type t = {
      targets: array<Connection__Target.Error.t>,
      commands: array<Connection__Command.Error.t>,
    }

    let toString = attempts => {
      if attempts.targets->Array.length == 0 {
        "Tried to connect with the path from the configuration but there are none.\n"
      } else {
        "Tried to connect with the path from the configuration but all failed:\n" ++
        attempts.targets
        ->Array.map(Connection__Target.Error.toString)
        ->Array.join("\n") ++ "\n"
      } ++ if attempts.commands->Array.length == 0 {
        ""
      } else {
        attempts.commands
        ->Array.map(Connection__Command.Error.toString)
        ->Array.join("\n")
      }
    }
  }

  type t =
    | PlatformNotSupported(Attempts.t, Connection__Download__Platform.raw)
    | NoDownloadALS(Attempts.t)
    | DownloadALS(Attempts.t, Connection__Download.Error.t)

  let toString = x =>
    switch x {
    | PlatformNotSupported(attempts, platform) =>
      Attempts.toString(attempts) ++
      "\nTried to download the Agda Language Server but the platform `" ++
      platform["os"] ++
      "/" ++
      platform["dist"] ++ "` is not supported.\n"

    | NoDownloadALS(attempts) =>
      Attempts.toString(
        attempts,
      ) ++ "\nPrebuilt Agda Language Server available for download but you opted not to.\n"

    | DownloadALS(attempts, error) =>
      Attempts.toString(attempts) ++
      "\nTried to download the Agda Language Server but failed:\n" ++
      Connection__Download.Error.toString(error)
    }
}

type t =
  | Agda(Connection__Target__Agda__Error.t, string)
  | ALS(Connection__Target__ALS__Error.t)
  | Aggregated(Aggregated.t)

let toString = x =>
  switch x {
  | Agda(e, _) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | Aggregated(e) => ("Error", Aggregated.toString(e))
  }
