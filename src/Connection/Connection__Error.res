type t =
  // Agda related
  | Agda(Connection__Target__Agda__Error.t, string)
  // ALS related
  | ALS(Connection__Target__ALS__Error.t)
  //
  | CannotFindAgda(Connection__Resolver.Error.t)
  | CannotFindALS(Connection__Resolver.Error.t)
  | CannotResolvePath(string)
  | NotAgdaOrALS(string)
  | ValidationError(string, Connection__Target__Agda__Process.Validation.Error.t)

let toString = x =>
  switch x {
  | Agda(e, _) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | CannotFindAgda(e) => ("Cannot find Agda", Connection__Resolver.Error.toString(e))
  | CannotFindALS(e) => ("Cannot find ALS", Connection__Resolver.Error.toString(e))
  | CannotResolvePath(path) => (
      "Cannot resolve path",
      "The path `" ++ path ++ "` cannot be resolved as a file path or a URL",
    )
  | NotAgdaOrALS(path) => (
      "Not Agda or ALS",
      "`" ++ path ++ "` doesn't seem to be an Agda executable or an Agda Language Server",
    )
  | ValidationError(path, e) => (
      "Error",
      Connection__Target__Agda__Process.Validation.Error.toString(e),
    )
  }
