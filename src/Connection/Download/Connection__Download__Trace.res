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
