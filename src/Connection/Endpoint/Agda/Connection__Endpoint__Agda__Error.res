type t =
  | ConnectionViaTCPNotSupported
  // Child process related error
  | Process(Connection__Transport__Process.Event.t)
  // Agda is not happy
  | AgdaError(string)
  // S-expression parse error
  | ResponseParseError(Parser.Error.t)

let toString = x =>
  switch x {
  | ConnectionViaTCPNotSupported => ("Connection Error", "Connection via TCP not supported yet")
  | Process(e) => ("Process Error", Connection__Transport__Process.Event.toString(e))
  | AgdaError(s) => ("Agda Error", s)
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
