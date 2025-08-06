module Construction = {
  module Attempts = {
    type t = {
      endpoints: Dict.t<Connection__Endpoint.Error.t>, // mapping from path to error
      commands: array<Connection__Command.Error.t>,
    }

    let toString = attempts => {
      if attempts.endpoints->Dict.toArray->Array.length == 0 {
        "Tried to connect with the path from the settings and the system but there are none.\n"
      } else {
        "Tried to connect with these paths but all failed:\n" ++
        attempts.endpoints
        ->Dict.toArray
        ->Array.map(((path, error)) =>
          "  " ++ path ++ ": " ++ Connection__Endpoint.Error.toString(error)
        )
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
  | Agda(Connection__Endpoint__Agda__Error.t)
  | ALS(Connection__Endpoint__ALS__Error.t)
  | Construction(Construction.t)

let toString = x =>
  switch x {
  | Agda(e) => Connection__Endpoint__Agda__Error.toString(e)
  | ALS(e) => Connection__Endpoint__ALS__Error.toString(e)
  | Construction(e) => ("Cannot Construct Connection", Construction.toString(e))
  }
