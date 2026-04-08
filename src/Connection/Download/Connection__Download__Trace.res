type t =
  | FetchStarted(string) // url
  | FetchResponseReceived(int, option<int>) // status, content-length
  | BodyReadStarted
  | BodyProgress(int, option<int>) // downloaded bytes, total bytes if known
  | BodyReadCompleted(int) // total bytes
  | WriteStarted(VSCode.Uri.t)
  | WriteCompleted(VSCode.Uri.t)
  | Failed(string, string) // stage, message

let noop = (_: t) => ()

let toString = (t: t): string =>
  switch t {
  | FetchStarted(url) => "FetchStarted: " ++ url
  | FetchResponseReceived(status, Some(len)) =>
    "FetchResponseReceived: " ++ string_of_int(status) ++ ", " ++ string_of_int(len) ++ " bytes"
  | FetchResponseReceived(status, None) =>
    "FetchResponseReceived: " ++ string_of_int(status) ++ ", unknown size"
  | BodyReadStarted => "BodyReadStarted"
  | BodyProgress(downloaded, Some(total)) =>
    "BodyProgress: " ++ string_of_int(downloaded) ++ "/" ++ string_of_int(total)
  | BodyProgress(downloaded, None) =>
    "BodyProgress: " ++ string_of_int(downloaded) ++ "/unknown"
  | BodyReadCompleted(total) => "BodyReadCompleted: " ++ string_of_int(total) ++ " bytes"
  | WriteStarted(uri) => "WriteStarted: " ++ VSCode.Uri.fsPath(uri)
  | WriteCompleted(uri) => "WriteCompleted: " ++ VSCode.Uri.fsPath(uri)
  | Failed(stage, msg) => "Failed(" ++ stage ++ "): " ++ msg
  }
