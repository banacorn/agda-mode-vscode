module Construction = {
  module Attempts = {
    type t = {
      endpoints: Dict.t<Connection__Endpoint.Error.t>, // errors encountered when trying to connect with a path
      commands: array<Connection__Command.Error.t>, // errors encountered when trying to run a command
      // download: option<Connection__Download.Error.t>, // error encountered when trying to download the Agda Language Server
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
    | NoDownloadALS(Attempts.t)
    | DownloadALS(Attempts.t, Connection__Download.Error.t)
    | Endpoint(string, Connection__Endpoint.Error.t)

  let toString = x =>
    switch x {
    | NoDownloadALS(attempts) =>
      Attempts.toString(
        attempts,
      ) ++ "\nPrebuilt Agda Language Server available for download but you opted not to.\n"

    | DownloadALS(attempts, error) =>
      Attempts.toString(attempts) ++
      "\nTried to download the Agda Language Server but failed:\n" ++
      Connection__Download.Error.toString(error)
    | Endpoint(path, error) =>
      "Tried to connect with the path `" ++
      path ++
      "` but failed:\n" ++
      Connection__Endpoint.Error.toString(error)
    }

  // the error should form a semigroup
  // let merge = (x, y) => switch (x, y) {

  // }
}

type t =
  | Agda(Connection__Endpoint__Agda__Error.t)
  | ALS(Connection__Endpoint__ALS__Error.t)
  | Construction(Construction.t)

let toString = x =>
  switch x {
  | Agda(e) => Connection__Endpoint__Agda__Error.toString(e)
  | ALS(e) => Connection__Endpoint__ALS__Error.toString(e)
  | Construction(e) => ("Connection error", Construction.toString(e))
  }
