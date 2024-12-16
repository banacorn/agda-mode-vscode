type t =
  // Emacs related
  | Emacs(Connection__Target__Agda__Error.t)
  // LSP related
  | LSP(Connection__Target__ALS__Error.t)
  //
  | CannotAcquireHandle(string, array<LanguageServerMule.Source.Error.t>)

let toString = x =>
  switch x {
  | Emacs(e) => Connection__Target__Agda__Error.toString(e)
  | LSP(e) => Connection__Target__ALS__Error.toString(e)
  | CannotAcquireHandle(target, es) => (
      "Unable to find " ++ target,
      "Here are the error messages from all the attempts: \n" ++
      es->Array.map(LanguageServerMule.Source.Error.toString)->Js.Array2.joinWith("\n"),
    )
  }
