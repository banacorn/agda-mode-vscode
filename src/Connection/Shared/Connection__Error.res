// Error occurred while communicating with Agda
module CommWithAgda = {
  type t =
    // Child process related error
    | Process(Connection__Transport__Process.Event.t)
    // Agda is not happy
    | ErrorFromAgda(string)
    // S-expression parse error
    | ResponseParseError(Parser.Error.t)

  let toString = x =>
    switch x {
    | Process(e) => ("Process Error", Connection__Transport__Process.Event.toString(e))
    | ErrorFromAgda(s) => ("Error from Agda", s)
    | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
    }
}

// Error occurred while communicating with the Agda Language Server
module CommWithALS = {
  // Errors when sending Command to the server
  module CommandErr = {
    type t =
      | CannotDecodeJSON(string)
      | CannotParseCommand(string)

    let toString = x =>
      switch x {
      | CannotDecodeJSON(s) => "Cannot decode JSON: \n" ++ s
      | CannotParseCommand(s) => "Cannot read IOTCM: \n" ++ s
      }

    let decode = {
      open JsonCombinators.Json.Decode
      Util.Decode.sum(x => {
        switch x {
        | "CmdErrCannotDecodeJSON" => Payload(string->map(message => CannotDecodeJSON(message)))
        | "CmdErrCannotParseCommand" => Payload(string->map(message => CannotParseCommand(message)))
        | tag =>
          raise(
            DecodeError("[Connection.Target.ALS.Error.CommandErr] Unknown constructor: " ++ tag),
          )
        }
      })
    }
  }

  type t =
    // Errors from the LSP client
    | ConnectionError(Js.Exn.t)
    | ConnectionTimeoutError(int) // timeout in ms
    // Errors when sending Command to the server
    | SendCommand(CommandErr.t)
    // Cannot initialize the connection
    | Initialize
    // Parsing / Decoding
    | CannotDecodeCommandRes(string, JSON.t)
    | CannotDecodeResponse(string, JSON.t)
    // S-expression parse error
    | ResponseParseError(Parser.Error.t)

  let toString = error =>
    switch error {
    | ConnectionError(exn) =>
      // let isECONNREFUSED =
      //   Js.Exn.message(exn)->Option.mapOr(false, String.startsWith("connect ECONNREFUSED", ...))

      // isECONNREFUSED
      //   ? ("Connection Error", "\nPlease enter \":main -d\" in ghci")
      //   : ("Client Internal Connection Error", Js.Exn.message(exn)->Option.getOr(""))

      let errorString = Util.JsError.toString(exn)

      switch errorString {
      | "" => ("Connection Error", "(empty error message)")
      | msg => ("Connection Error", msg)
      }

    | ConnectionTimeoutError(timeout) => (
        "Connection Timeout",
        "Expected to connect within " ++ string_of_int(timeout) ++ "ms",
      )
    | SendCommand(e) => ("Cannot Send Command", CommandErr.toString(e))
    | Initialize => ("Cannot Initialize Connection", "")
    | CannotDecodeCommandRes(msg, json) => (
        "Cannot Send Command",
        "Cannot decode the result after sending command" ++ msg ++ "\n" ++ JSON.stringify(json),
      )
    | CannotDecodeResponse(msg, json) => (
        "Cannot Parse Response",
        "Cannot decode responses from the server" ++ msg ++ "\n" ++ JSON.stringify(json),
      )
    | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
    }
}

module Probe = {
  type t =
    | NotAgdaOrALS(string) // the actual output received
    | CannotDetermineAgdaOrALS(Connection__Process__Exec.Error.t) // cannot determine if it's Agda or ALS
    | CannotMakeConnectionWithALS(CommWithALS.t)
    | CannotMakeConnectionWithALSWASMYet(string) // reason for failure

  let toString = x =>
    switch x {
    | NotAgdaOrALS(output) =>
      // "Not Agda or Agda Language Server",
      let outputInfo = if output == "" {
        "no output (empty string)"
      } else {
        "'" ++ output ++ "'"
      }
      "doesn't seem to be an Agda executable or an Agda Language Server. Output received: " ++
      outputInfo
    | CannotDetermineAgdaOrALS(e) => Connection__Process__Exec.Error.toString(e)
    | CannotMakeConnectionWithALS(e) =>
      "Cannot make connection with Agda Language Server: " ++ snd(CommWithALS.toString(e))
    | CannotMakeConnectionWithALSWASMYet(reason) => "WASM connection failed: " ++ reason
    }
}

// Error occurred while trying to establish a connection
module Establish = {
  // Organized in a way such that we can report all failed attempts of establishing a connection at once
  type t = {
    probes: Dict.t<Probe.t>, // index by path
    commands: Dict.t<Connection__Command.Error.t>, // index by command name
    download: option<Connection__Download.Error.t>, // error encountered when trying to download the Agda Language Server, or when the user opted not to download
  }

  let toString = x => {
    let probesStr = if x.probes->Dict.toArray->Array.length == 0 {
      "Tried to connect with the path from the settings and the system but there are none.\n"
    } else {
      "Tried to connect with these paths but all failed:\n" ++
      x.probes
      ->Dict.toArray
      ->Array.map(((path, error)) => "  " ++ path ++ ": " ++ Probe.toString(error))
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
    probesStr ++ commandsStr ++ downloadStr
  }

  let fromProbeError = (path, error) => {
    probes: Dict.fromArray([(path, error)]),
    commands: Dict.make(),
    download: None,
  }

  let fromCommandError = (command, error) => {
    probes: Dict.make(),
    commands: Dict.fromArray([(command, error)]),
    download: None,
  }

  let fromDownloadError = error => {
    probes: Dict.make(),
    commands: Dict.make(),
    download: Some(error),
  }

  // Should form a monoid
  let merge = (x, y) => {
    probes: {
      let dict = Dict.make()
      x.probes->Dict.forEachWithKey((error, path) => Dict.set(dict, path, error))
      y.probes->Dict.forEachWithKey((error, path) => Dict.set(dict, path, error))
      dict
    },
    commands: {
      let dict = Dict.make()
      x.commands->Dict.forEachWithKey((error, command) => Dict.set(dict, command, error))
      y.commands->Dict.forEachWithKey((error, command) => Dict.set(dict, command, error))
      dict
    },
    download: switch (x.download, y.download) {
    | (Some(error), yDownload) =>
      // Prefer x's error if both exist, but assert y's error is handled
      if Option.isSome(yDownload) {
        // Both have errors - we keep x's error but acknowledge y had one too
        assert(Option.isSome(yDownload))
      }
      Some(error)
    | (None, Some(error)) => Some(error)
    | (None, None) => None
    },
  }

  let mergeMany = xs =>
    xs->Array.reduce(
      {
        probes: Dict.make(),
        commands: Dict.make(),
        download: None,
      },
      (acc, x) => merge(acc, x),
    )
}

type t =
  | Establish(Establish.t)
  | CommWithAgda(CommWithAgda.t)
  | CommWithALS(CommWithALS.t)

let toString = x =>
  switch x {
  | Establish(e) => ("Cannot Establish Connection", Establish.toString(e))
  | CommWithAgda(e) => CommWithAgda.toString(e)
  | CommWithALS(e) => CommWithALS.toString(e)
  }
