// Helper function to check for prebuilt data directory
let checkForPrebuiltDataDirectory = async (executablePath: string) => {
  // the executable needs to be accompanied by a `data` directory
  // which can be specified by the environment variable "Agda_datadir"
  // prebuilt executables on GitHub have this directory placed alongside the executable
  let prebuildDataDirPath = NodeJs.Path.join([executablePath, "..", "data"])
  let prebuildDataDirURI = VSCode.Uri.file(prebuildDataDirPath)

  switch await FS.stat(prebuildDataDirURI) {
  | Ok(_) => Some(NodeJs.Path.join([executablePath, "..", "data"]))
  | Error(_) => None
  }
}

module Module: {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(
        version,
        version,
        Connection__Transport.t,
        option<Connection__Endpoint__Protocol__LSP__Binding.executableOptions>,
      ) // ALS version, Agda version, method of IPC, LSP options

  // see if it's a Agda executable or a language server
  let probeFilepath: Connection__URI.t => promise<result<t, Connection__Endpoint__Error.t>>
} = {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(
        version,
        version,
        Connection__Transport.t,
        option<Connection__Endpoint__Protocol__LSP__Binding.executableOptions>,
      ) // ALS version, Agda version, method of IPC, LSP options

  // see if it's a Agda executable or a language server
  let probeFilepath = async uri =>
    switch uri {
    | Connection__URI.LspURI(_) => Error(Connection__Endpoint__Error.CannotHandleURLsATM)
    | FileURI(_, vscodeUri) =>
      let path = VSCode.Uri.fsPath(vscodeUri)
      let result = await Connection__Process__Exec.run(path, ["--version"])
      switch result {
      | Ok(output) =>
        // try Agda
        switch String.match(output, %re("/Agda version (.*)/")) {
        | Some([_, Some(version)]) => Ok(Agda(version, path))
        | _ =>
          // try ALS
          switch String.match(output, %re("/Agda v(.*) Language Server v(.*)/")) {
          | Some([_, Some(agdaVersion), Some(alsVersion)]) =>
            let lspOptions = switch await checkForPrebuiltDataDirectory(path) {
            | Some(assetPath) =>
              let env = Dict.fromArray([("Agda_datadir", assetPath)])
              Some({Connection__Endpoint__Protocol__LSP__Binding.env: env})
            | None => None
            }
            Ok(ALS(alsVersion, agdaVersion, Connection__Transport.ViaPipe(path, []), lspOptions))
          | _ => Error(Connection__Endpoint__Error.NotAgdaOrALS(output))
          }
        }
      | Error(error) => Error(Connection__Endpoint__Error.CannotDetermineAgdaOrALS(error))
      }
    }
}

include Module
