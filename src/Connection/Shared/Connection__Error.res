module Construction = {
  type t = {
    endpoints: Dict.t<Connection__Endpoint.Error.t>, // connection error index by path
    commands: Dict.t<Connection__Command.Error.t>, // connection error index by command name
    download: option<Connection__Download.Error.t>, // error encountered when trying to download the Agda Language Server
  }

  let toString = x => {
    let endpointsStr = if x.endpoints->Dict.toArray->Array.length == 0 {
      "Tried to connect with the path from the settings and the system but there are none.\n"
    } else {
      "Tried to connect with these paths but all failed:\n" ++
      x.endpoints
      ->Dict.toArray
      ->Array.map(((path, error)) =>
        "  " ++ path ++ ": " ++ Connection__Endpoint.Error.toString(error)
      )
      ->Array.join("\n") ++ "\n"
    }

    let commandsStr = if x.commands->Dict.toArray->Array.length == 0 {
      ""
    } else {
      x.commands
      ->Dict.toArray
      ->Array.map(((command, error)) =>
        "Tried to connect with `" ++
        command ++
        "` but failed:\n. " ++
        Connection__Command.Error.toString(error)
      )
      ->Array.join("\n") ++ "\n"
    }

    let downloadStr = switch x.download {
    | None => "Opted not to download prebuilt Agda Language Server"
    | Some(error) =>
      "Tried to download the Agda Language Server but failed:\n" ++
      Connection__Download.Error.toString(error)
    }
    // Concatenate all parts
    endpointsStr ++ commandsStr ++ downloadStr
  }

  let addEndpointError = (x, path, error) => {
    Dict.set(x.endpoints, path, error)
    x
  }

  let addCommandError = (x, command, error) => {
    Dict.set(x.commands, command, error)
    x
  }

  let addDownloadError = (x, error) => {
    switch x.download {
    | Some(_) => x // If there's already a download error, keep it
    | None => {
        endpoints: x.endpoints,
        commands: x.commands,
        download: Some(error),
      }
    }
  }

  // Should form a monoid
  let make = () => {
    {
      endpoints: Dict.make(),
      commands: Dict.make(),
      download: None,
    }
  }

  // Should form a monoid
  let merge = (x, y) => {
    endpoints: {
      let dict = Dict.make()
      x.endpoints->Dict.forEachWithKey((error, path) => Dict.set(dict, path, error))
      y.endpoints->Dict.forEachWithKey((error, path) => Dict.set(dict, path, error))
      dict
    },
    commands: {
      let dict = Dict.make()
      x.commands->Dict.forEachWithKey((error, command) => Dict.set(dict, command, error))
      y.commands->Dict.forEachWithKey((error, command) => Dict.set(dict, command, error))
      dict
    },
    download: switch (x.download, y.download) {
    | (Some(error), _) => Some(error)
    | (_, Some(error)) => Some(error)
    | _ => None
    },
  }

  let mergeMany = xs =>
    xs->Array.reduce(
      {
        endpoints: Dict.make(),
        commands: Dict.make(),
        download: None,
      },
      (acc, x) => merge(acc, x),
    )
}

type t =
  | Agda(Connection__Endpoint__Agda__Error.t)
  | ALS(Connection__Endpoint__ALS__Error.t)
  | Construction(Construction.t)

let toString = x =>
  switch x {
  | Agda(e) => Connection__Endpoint__Agda__Error.toString(e)
  | ALS(e) => Connection__Endpoint__ALS__Error.toString(e)
  | Construction(e) => ("Cannot Establish Connection", Construction.toString(e))
  }
