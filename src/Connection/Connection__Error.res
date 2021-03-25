type t =
  // 
  // "Network Layer"
  // 

  // cannot find Agda or the language server in the path
  | PathSearch(Process.PathSearch.Error.t)
  // cannot find the language server on localhost
  | PortSearch(int, Js.Exn.t)
  // the found program is not Agda
  | Validation(Process.Validation.Error.t)
  // Agda went wrong 
  | Process(Process.Error.t)

  // 
  // "Presentation Layer"
  // 
  
  // S-expression parse error
  | ResponseParseError(Parser.Error.t)

  // LSP related
  | LSP(Connection__LSP__Error.t)


let toString = x =>
  switch x {
  | PathSearch(e) => Process.PathSearch.Error.toString(e)
  | PortSearch(port, e) => (
      "Cannot connect with the dev server on port " ++ string_of_int(port),
      "Did you forget to enter \":main -d\" in ghci?\n" ++ Util.JsError.toString(e),
    )
  | Validation(e) => Process.Validation.Error.toString(e)
  | Process(e) => Process.Error.toString(e)
  | LSP(e) => Connection__LSP__Error.toString(e)
  | ResponseParseError(e) => ("Internal Parse Error", Parser.Error.toString(e))
  }
