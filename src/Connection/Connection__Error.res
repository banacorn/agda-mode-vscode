type t =
  // Agda related
  | Agda(Connection__Target__Agda__Error.t)
  // ALS related
  | ALS(Connection__Target__ALS__Error.t)
  //
  | CannotResolve(string, array<Connection__Resolver.Error.t>)

let toString = x =>
  switch x {
  | Agda(e) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | CannotResolve(target, es) => (
      "Unable to find " ++ target,
      "Here are the error messages from all the attempts: \n" ++
      es->Array.map(Connection__Resolver.Error.toString)->Js.Array2.joinWith("\n"),
    )
  }
