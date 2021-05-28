type t =
  // cannot find Agda or the language server in the path
  | PathSearch(Connection__Process.PathSearch.Error.t)
  // the found program is not Agda
  | Validation(Connection__Process.Validation.Error.t)
  // Child process related error
  | Process(Connection__Process.Event.t)
  // Agda is not happy
  | AgdaError(string)
  // S-expression parse error
  | ResponseParseError(Parser.Error.t)


let toString = x =>
  switch x {
  | PathSearch(e) => Connection__Process.PathSearch.Error.toString(e)
  | Validation(e) => Connection__Process.Validation.Error.toString(e)
  | Process(e) => Connection__Process.Event.toString(e)
  | AgdaError(s) => ("Agda Error", s)
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
