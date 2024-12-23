type t =
  // Agda related
  | Agda(Connection__Target__Agda__Error.t)
  // ALS related
  | ALS(Connection__Target__ALS__Error.t)
  //
  | CannotResolve(string, array<Connection__Resolver.Error.t>)
  | CannotFindAgda(Connection__Resolver.Error.t)
  | CannotFindALS(Connection__Resolver.Error.t)
  | CannotResolvePath(string)
  | NotAgdaOrALS(string)
  | CannotFindAgdaOrALS

let toString = x =>
  switch x {
  | Agda(e) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | CannotResolve(target, es) => (
      "Unable to find " ++ target,
      "Here are the error messages from all the attempts: \n" ++
      es->Array.map(Connection__Resolver.Error.toString)->Js.Array2.joinWith("\n"),
    )
  | CannotFindAgda(e) => ("Cannot find Agda", Connection__Resolver.Error.toString(e))
  | CannotResolvePath(path) => ("Cannot resolve path", "The path `" ++ path ++ "` cannot be resolved as a file path or a URL")
  | NotAgdaOrALS(path) => ("Not Agda or ALS", "`" ++ path ++ "` doesn't seem to be an Agda executable or an Agda Language Server")
  | CannotFindAgdaOrALS => ("Cannot find Agda or ALS", "Cannot find Agda or Agda Language Server in the system")
  }
