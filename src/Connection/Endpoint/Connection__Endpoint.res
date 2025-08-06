module IPC = Connection__Transport
module URI = Connection__URI

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

module Error = {
  type t =
    | NotAgdaOrALS(string) // the actual output received
    | SomethingWentWrong(Connection__Process__Exec.Error.t) // runtime error
    | CannotHandleURLsATM

  let toString = x =>
    switch x {
    | CannotHandleURLsATM => // "Cannot handle URLs at the moment",
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
    | SomethingWentWrong(e) => Connection__Process__Exec.Error.toString(e)
    }
}

module Module: {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(
        version,
        version,
        IPC.t,
        option<Connection__Endpoint__Protocol__LSP__Binding.executableOptions>,
      ) // ALS version, Agda version, method of IPC, LSP options

  // see if it's a Agda executable or a language server
  let probeFilepath: URI.t => promise<result<t, Error.t>>

  // from URI to Endpoint
  let fromURI: URI.t => promise<result<t, Error.t>>
  // from Endpoint to URI
  let toURI: t => URI.t

  // from String to Endpoint
  let fromRawPath: string => promise<result<t, Error.t>>
  let fromRawPaths: array<string> => promise<array<result<t, Error.t>>>

  // from VSCode.Uri.t to Endpoint
  let fromVSCodeUri: VSCode.Uri.t => promise<result<t, Error.t>>

  // configuration
  let getPicked: (Memento.t, array<Connection__URI.t>) => promise<result<t, Dict.t<Error.t>>>
  let getPickedRaw: (Memento.t, array<string>) => promise<option<string>>
  let setPicked: (Memento.t, option<t>) => promise<unit>
} = {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(
        version,
        version,
        IPC.t,
        option<Connection__Endpoint__Protocol__LSP__Binding.executableOptions>,
      ) // ALS version, Agda version, method of IPC, LSP options

  // see if it's a Agda executable or a language server
  let probeFilepath = async uri =>
    switch uri {
    | URI.LspURI(_) => Error(Error.CannotHandleURLsATM)
    | URI.FileURI(_, vscodeUri) =>
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
          | _ => Error(Error.NotAgdaOrALS(output))
          }
        }
      | Error(error) => Error(Error.SomethingWentWrong(error))
      }
    }

  let fromURI = async uri =>
    switch uri {
    | URI.LspURI(_) => Error(Error.CannotHandleURLsATM)
    | FileURI(_) =>
      switch await probeFilepath(uri) {
      | Ok(endpoint) => Ok(endpoint)
      | Error(error) => Error(error)
      }
    }

  let fromRawPath = async rawPath => {
    let uri = URI.parse(rawPath)
    await fromURI(uri)
  }

  let fromVSCodeUri = uri => uri->VSCode.Uri.toString->fromRawPath

  // plural form of `fromRawPath`
  let fromRawPaths = paths => paths->Array.map(fromRawPath)->Promise.all

  // TODO, reexamine this
  let toURI = endpoint =>
    switch endpoint {
    | Agda(_, raw) => URI.FileURI(raw, VSCode.Uri.file(raw))
    | ALS(_, _, ViaPipe(raw, _), _) => URI.FileURI(raw, VSCode.Uri.file(raw))
    | ALS(_, _, ViaTCP(raw, url), _) => URI.LspURI(raw, url)
    }

  // Try to find the previously picked connection
  // The previously picked connection is stored in the memento, if it doesn't exist
  // the first usable connection endpoint from the supplied paths is returned
  let getPicked = async (memento: Memento.t, rawSuppliedPaths: array<Connection__URI.t>) => {
    // convert raw supplied paths to endpoints
    // and filter out the invalid ones

    let errors = Dict.make()

    let suppliedEndpoints =
      await rawSuppliedPaths
      ->Array.map(async path => {
        switch await fromURI(path) {
        | Ok(endpoint) => Some(endpoint)
        | Error(error) =>
          errors->Dict.set(Connection__URI.getOriginalPath(path), error)
          None
        }
      })
      ->Promise.all
    let suppliedEndpoints = suppliedEndpoints->Array.filterMap(x => x)

    let pickFromSuppliedEndpointsInstead = switch suppliedEndpoints[0] {
    | None => Error(errors)
    | Some(endpoint) => Ok(endpoint)
    }

    switch Memento.PickedConnection.get(memento) {
    | Some(rawPathFromMemento) =>
      switch await fromRawPath(rawPathFromMemento) {
      | Error(_) =>
        // the path in the memento is invalid
        // remove it from the memento
        await Memento.PickedConnection.set(memento, None)
        pickFromSuppliedEndpointsInstead
      | Ok(endpointFromMemento) =>
        let existsInSuppliedEndpoints = suppliedEndpoints->Util.Array.includes(endpointFromMemento)
        if existsInSuppliedEndpoints {
          Ok(endpointFromMemento)
        } else {
          // the path in the memento is not in the supplied paths
          // remove it from the memento
          await Memento.PickedConnection.set(memento, None)
          pickFromSuppliedEndpointsInstead
        }
      }
    | None => pickFromSuppliedEndpointsInstead
    }
  }

  let getPickedRaw = async (memento: Memento.t, fromSystem: array<string>) => {
    switch Memento.PickedConnection.get(memento) {
    | Some(fromMemento) =>
      let existsInSuppliedPaths = fromSystem->Array.includes(fromMemento)
      if existsInSuppliedPaths {
        Some(fromMemento)
      } else {
        // the path in the memento is not in the paths provided by the system
        // remove it from the memento
        await Memento.PickedConnection.set(memento, None)
        None
      }
    | None => None
    }
  }

  let setPicked = (memento: Memento.t, endpoint) =>
    switch endpoint {
    | None => Memento.PickedConnection.set(memento, None)
    | Some(endpoint) => Memento.PickedConnection.set(memento, Some(toURI(endpoint)->URI.toString))
    }
}

include Module
