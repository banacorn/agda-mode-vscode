open Belt

// TODO: sort these errors out
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
    Util.Decode.sum_(x => {
      switch x {
      | "CmdErrCannotDecodeJSON" => Payload(string->map((. version) => CannotDecodeJSON(version)))
      | "CmdErrCannotParseCommand" =>
        Payload(string->map((. version) => CannotParseCommand(version)))
      | tag => raise(DecodeError("[Connection.LSP.Error.CommandErr] Unknown constructor: " ++ tag))
      }
    })
  }
}

type t =
  // Errors from the LSP client
  | ConnectionError(Js.Exn.t)
  // Errors when sending Command to ther server
  | SendCommand(CommandErr.t)
  // Cannot initialize the connection
  | Initialize
  // Parsing / Decoding
  | CannotDecodeCommandRes(string, Js.Json.t)
  | CannotDecodeResponse(string, Js.Json.t)
  // S-expression parse error
  | ResponseParseError(Parser.Error.t)

let toString = error =>
  switch error {
  | ConnectionError(exn) =>
    let isECONNREFUSED =
      Js.Exn.message(exn)->Option.mapWithDefault(
        false,
        Js.String.startsWith("connect ECONNREFUSED"),
      )

    isECONNREFUSED
      ? ("Connection Error", "Please enter \":main -d\" in ghci")
      : ("Client Internal Connection Error", Js.Exn.message(exn)->Option.getWithDefault(""))
  | SendCommand(e) => ("Cannot Send Command", CommandErr.toString(e))
  | Initialize => ("Cannot Initialize Connection", "")
  | CannotDecodeCommandRes(msg, json) => (
      "Cannot Send Command",
      "Cannot decode the result after sending command" ++ msg ++ "\n" ++ Json.stringify(json),
    )
  | CannotDecodeResponse(msg, json) => (
      "Cannot Parse Response",
      "Cannot decode responses from the server" ++ msg ++ "\n" ++ Json.stringify(json),
    )
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
