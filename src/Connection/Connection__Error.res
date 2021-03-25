open Belt

// TODO: sort these errors out
module LSP = {
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
      | "CmdErrCannotParseCommand" =>
        Contents(string |> map(version => CannotParseCommand(version)))
      | tag => raise(DecodeError("[LSP.CommandErr] Unknown constructor: " ++ tag))
      }
    )
  }

  type t =
    // Errors originated from the LSP client within
    | Connection(Js.Exn.t)
    // Errors when sending Command to ther server
    | SendCommand(CommandErr.t)
    // Cannot initialize the connection
    | Initialize
    // Parsing / Decoding
    | CannotDecodeCommandRes(string, Js.Json.t)
    | CannotDecodeResponse(string, Js.Json.t)

  let toString = error =>
    switch error {
    | Connection(exn) =>
      let isECONNREFUSED =
        Js.Exn.message(exn)->Option.mapWithDefault(
          false,
          Js.String.startsWith("connect ECONNREFUSED"),
        )

      isECONNREFUSED
        ? ("[LSP] Connection Error", "Please enter \":main -d\" in ghci")
        : ("[LSP] Client Internal Connection Error", Js.Exn.message(exn)->Option.getWithDefault(""))
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
    }
}

type t =
  // probing "agda" or "als"
  | CannotConnectViaStdIO(AgdaModeVscode.Process.PathSearch.Error.t)
  | CannotConnectViaTCP(Js.Exn.t)
  | PathSearch(Process.PathSearch.Error.t)
  | Validation(Process.Validation.Error.t)
  | Process(Process.Error.t)
  // LSP related
  | LSP(LSP.t)
  // Emacs S-expression parse error
  | ResponseParseError(Parser.Error.t)
  | NotConnectedYet


let toString = x =>
  switch x {
  | CannotConnectViaStdIO(e) =>
    let (_header, body) = AgdaModeVscode.Process.PathSearch.Error.toString(e)
    ("Cannot locate \"als\"", body ++ "\nPlease make sure that the executable is in the path")
  | CannotConnectViaTCP(_) => (
      "Cannot connect with the server",
      "Please enter \":main -d\" in ghci",
    )
  | PathSearch(e) => Process.PathSearch.Error.toString(e)
  | Validation(e) => Process.Validation.Error.toString(e)
  | Process(e) => Process.Error.toString(e)
  | LSP(e) => LSP.toString(e)
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  | NotConnectedYet => ("Connection not established yet", "")
  }
