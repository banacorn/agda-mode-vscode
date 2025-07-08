@module("fs")
external readdir: (string, (Js.nullable<Js.Exn.t>, array<string>) => unit) => unit = "readdir"

let readdir = filepath => {
  Promise.make((resolve, reject) => {
    readdir(filepath, (error, files) => {
      switch Js.Nullable.toOption(error) {
      | Some(err) => reject(err)
      | None => resolve(files)
      }
    })
  })
}

// @module("fs")
// external access: (string, Js.null<Js.Exn.t> => unit) => unit = "access"

@module("fs")
external unlink: (string, Js.nullable<Js.Exn.t> => unit) => unit = "unlink"

let readFile = async filepath => {
  let fileHandle = await NodeJs.Fs.open_(filepath, NodeJs.Fs.Flag.read)
  let buffer = await NodeJs.Fs.FileHandle.readFile(fileHandle)
  await NodeJs.Fs.FileHandle.close(fileHandle)
  NodeJs.Buffer.toString(buffer)
}
