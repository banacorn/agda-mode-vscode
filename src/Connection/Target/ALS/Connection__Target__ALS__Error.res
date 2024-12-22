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
    Util.Decode.sum(x => {
      switch x {
      | "CmdErrCannotDecodeJSON" => Payload(string->map(version => CannotDecodeJSON(version)))
      | "CmdErrCannotParseCommand" => Payload(string->map(version => CannotParseCommand(version)))
      | tag =>
        raise(DecodeError("[Connection.Target.ALS.Error.CommandErr] Unknown constructor: " ++ tag))
      }
    })
  }
}

type t =
  // the found program is not ALS
  | Validation(string)
  // Errors from the LSP client
  | ConnectionError(Js.Exn.t)
  // Errors when sending Command to ther server
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
    let isECONNREFUSED =
      Js.Exn.message(exn)->Option.mapOr(false, String.startsWith("connect ECONNREFUSED", ...))

    isECONNREFUSED
      ? ("Connection Error", "Please enter \":main -d\" in ghci")
      : ("Client Internal Connection Error", Js.Exn.message(exn)->Option.getOr(""))
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
  | Validation(msg) => ("Validation Error", msg)
  }
