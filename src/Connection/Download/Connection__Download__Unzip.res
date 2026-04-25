// module for unzipping downloaded files
module Unzipper = {
  @module("unzipper")
  external extract: {"path": string} => NodeJs.Fs.WriteStream.t = "Extract"
}

@send external onFinishOnce: (NodeJs.Fs.WriteStream.t, @as("finish") _, unit => unit) => NodeJs.Fs.WriteStream.t = "once"
@send external onErrorOnce: ('stream, @as("error") _, Js.Exn.t => unit) => 'stream = "once"

let run = (
  srcUri: VSCode.Uri.t,
  destUri: VSCode.Uri.t,
  ~makeStreams: option<(string, string) => (NodeJs.Fs.ReadStream.t, NodeJs.Fs.WriteStream.t)>=None,
  ~doPipe: option<(NodeJs.Fs.ReadStream.t, NodeJs.Fs.WriteStream.t) => unit>=None,
) =>
  Promise.make((resolve, reject) => {
    let srcPath = srcUri->VSCode.Uri.fsPath
    let destPath = destUri->VSCode.Uri.fsPath

    let (readStream, extractStream) = switch makeStreams {
    | Some(make) => make(srcPath, destPath)
    | None => (NodeJs.Fs.createReadStream(srcPath), Unzipper.extract({"path": destPath}))
    }

    // Handlers FIRST — before pipe, so no event emitted during/after pipe can be missed
    extractStream->onFinishOnce(resolve)->ignore
    readStream->onErrorOnce(err => reject(err->Obj.magic))->ignore
    extractStream->onErrorOnce(err => reject(err->Obj.magic))->ignore

    // Pipe AFTER handlers are attached
    switch doPipe {
    | Some(pipe) => pipe(readStream, extractStream)
    | None => readStream->NodeJs.Fs.ReadStream.pipe(extractStream)->ignore
    }
  })
