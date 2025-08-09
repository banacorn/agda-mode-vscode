type t =
  | NotAgdaOrALS(string) // the actual output received
  | CannotDetermineAgdaOrALS(Connection__Process__Exec.Error.t) // cannot determine if it's Agda or ALS
  | CannotHandleURLsAtTheMoment
  | CannotMakeConnectionWithALS(Connection__Endpoint__ALS__Error.t)

let toString = x =>
  switch x {
  | CannotHandleURLsAtTheMoment => // "Cannot handle URLs at the moment",
    "Cannot handle URLs at the moment, this will be supported again in the future"

  | NotAgdaOrALS(output) =>
    // "Not Agda or Agda Language Server",
    let outputInfo = if output == "" {
      "no output (empty string)"
    } else {
      "'" ++ output ++ "'"
    }
    "doesn't seem to be an Agda executable or an Agda Language Server. Output received: " ++
    outputInfo
  | CannotDetermineAgdaOrALS(e) => Connection__Process__Exec.Error.toString(e)
  | CannotMakeConnectionWithALS(e) =>
    "Cannot make connection with Agda Language Server: " ++
    snd(Connection__Endpoint__ALS__Error.toString(e))
  }
