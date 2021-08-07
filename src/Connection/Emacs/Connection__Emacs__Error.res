open Belt
module Process = LanguageServerMule.Client.Process

type t =
  // cannot find Agda or the language server in the path
  // | PathSearch(Connection__Process.PathSearch.Error.t)
  | ConnectionViaTCPNotSupported
  // server probing
  | CannotAcquireHandle(array<LanguageServerMule.Source.Error.t>)
  // the found program is not Agda
  | Validation(string)
  // Child process related error
  | Process(Process.Event.t)
  // Agda is not happy
  | AgdaError(string)
  // S-expression parse error
  | ResponseParseError(Parser.Error.t)


let toString = x =>
  switch x {
  // | PathSearch(e) => Connection__Process.PathSearch.Error.toString(e)
  | ConnectionViaTCPNotSupported => ("Connection Error", "Connection via TCP not supported yet")
  | CannotAcquireHandle(es) => (
      "Cannot connect with \"agda\"",
      "Here are the error messages from all the attempts: \n" ++
      es->Array.map(LanguageServerMule.Source.Error.toString)->Js.Array2.joinWith("\n"),
    )
  | Validation(e) => ("Validation Error", e)
  | Process(e) => ("Process Error", Process.Event.toString(e))
  | AgdaError(s) => ("Agda Error", s)
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
