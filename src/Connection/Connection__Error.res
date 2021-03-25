type t =
  // connection
  | PathSearch(Process.PathSearch.Error.t)
  | Validation(Process.Validation.Error.t)
  | Process(Process.Error.t)
  // Emacs S-expression parse error
  | ResponseParseError(Parser.Error.t)
  // LSP related
  | LSP(Connection__LSP__Error.t)


let toString = x =>
  switch x {
  | PathSearch(e) => Process.PathSearch.Error.toString(e)
  | Validation(e) => Process.Validation.Error.toString(e)
  | Process(e) => Process.Error.toString(e)
  | LSP(e) => Connection__LSP__Error.toString(e)
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
