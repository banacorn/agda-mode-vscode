type t =
  // Emacs related
  | Emacs(Connection__Emacs__Error.t)
  // LSP related
  | LSP(Connection__LSP__Error.t)


let toString = x =>
  switch x {
  | Emacs(e) => Connection__Emacs__Error.toString(e)
  | LSP(e) => Connection__LSP__Error.toString(e)
  }
