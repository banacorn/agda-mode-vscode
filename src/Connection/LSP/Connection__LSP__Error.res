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

  open Json.Decode
  open Util.Decode
  let decode: decoder<t> = sum(x =>
    switch x {
    | "CmdErrCannotDecodeJSON" => Contents(string |> map(version => CannotDecodeJSON(version)))
    | "CmdErrCannotParseCommand" => Contents(string |> map(version => CannotParseCommand(version)))
    | tag => raise(DecodeError("[LSP.CommandErr] Unknown constructor: " ++ tag))
    }
  )
}

type t =
  // cannot find Agda or the language server in the path
  | PathSearch(Connection__Process.PathSearch.Error.t)
  // cannot find the language server on localhost
  | PortSearch(int, Js.Exn.t)
  // Errors originated from the LSP client within
  | Connection(Js.Exn.t)
  // server probing
  | CannotAcquireHandle(array<LanguageServerMule.Source.Error.t>)
  // connection
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
  | PathSearch(e) => Connection__Process.PathSearch.Error.toString(e)
  | PortSearch(port, e) => (
      "Cannot connect with the dev server on port " ++ string_of_int(port),
      "Did you forget to enter \":main -d\" in ghci?\n" ++ Util.JsError.toString(e),
    )
  | Connection(exn) =>
    let isECONNREFUSED =
      Js.Exn.message(exn)->Option.mapWithDefault(
        false,
        Js.String.startsWith("connect ECONNREFUSED"),
      )

    isECONNREFUSED
      ? ("[LSP] Connection Error", "Please enter \":main -d\" in ghci")
      : ("[LSP] Client Internal Connection Error", Js.Exn.message(exn)->Option.getWithDefault(""))
  | CannotAcquireHandle(es) => (
      "Cannot connect with \"als\"",
      "Here are the error messages from all the attempts: \n" ++
      es->Array.map(LanguageServerMule.Source.Error.toString)->Js.Array2.joinWith("\n"),
    )
  | ConnectionError(exn) =>
    let isECONNREFUSED =
      Js.Exn.message(exn)->Option.mapWithDefault(
        false,
        Js.String.startsWith("connect ECONNREFUSED"),
      )

    isECONNREFUSED
      ? ("LSP Connection Error", "Please enter \":main -d\" in ghci")
      : ("LSP Client Error", Js.Exn.message(exn)->Option.getWithDefault(""))
  | SendCommand(e) => ("[LSP] Cannot Send Command", CommandErr.toString(e))
  | Initialize => ("[LSP] Cannot Initialize Connection", "")
  | CannotDecodeCommandRes(msg, json) => (
      "[LSP] Cannot Send Command",
      "Cannot decode the result after sending command" ++ msg ++ "\n" ++ Json.stringify(json),
    )
  | CannotDecodeResponse(msg, json) => (
      "[LSP] Cannot Parse Response",
      "Cannot decode responses from the server" ++ msg ++ "\n" ++ Json.stringify(json),
    )
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
