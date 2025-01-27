// module for unzipping downloaded files
module Unzipper = {
  @module("unzipper")
  external extract: {"path": string} => NodeJs.Fs.WriteStream.t = "Extract"
}

let run = (src, dest) =>
  Promise.make((resolve, _) => {
    // resolve the promise after the read stream has been closed by the Unzipper
    let readStream = NodeJs.Fs.createReadStream(src)
    readStream->NodeJs.Fs.ReadStream.onCloseOnce(resolve)->ignore

    // start unzipping the file
    readStream->NodeJs.Fs.ReadStream.pipe(Unzipper.extract({"path": dest}))->ignore
  })
