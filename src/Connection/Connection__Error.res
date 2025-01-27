type t =
  // Agda related
  | Agda(Connection__Target__Agda__Error.t, string)
  // ALS related
  | ALS(Connection__Target__ALS__Error.t)
  //
  | CannotFindCommand(string, Connection__Command__Search.Error.t)
  | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
  | CannotResolvePath(string)
  | NotAgdaOrALS(string)
  | ValidationError(string, Connection__Validation.Error.t)

let toString = x =>
  switch x {
  | Agda(e, _) => Connection__Target__Agda__Error.toString(e)
  | ALS(e) => Connection__Target__ALS__Error.toString(e)
  | CannotFindCommand(name, e) => (
      "Cannot find command \"" ++ name ++ "\"",
      Connection__Command__Search.Error.toString(e),
    )
  | CannotFetchALSReleases(e) => (
      "Cannot fetch ALS releases",
      Connection__Download__GitHub.Error.toString(e),
    )
  | CannotResolvePath(path) => (
      "Cannot resolve path",
      "The path `" ++ path ++ "` cannot be resolved as a file path or a URL",
    )
  | NotAgdaOrALS(path) => (
      "Not Agda or ALS",
      "`" ++ path ++ "` doesn't seem to be an Agda executable or an Agda Language Server",
    )
  | ValidationError(_, e) => ("Error", Connection__Validation.Error.toString(e))
  }
