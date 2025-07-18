module IPC = Connection__Target__IPC
module URI = Connection__URI
module Process = Connection__Process

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
    | NotAgdaOrALS(URI.t, string) // URI and the actual output received
    | SomethingWentWrong(URI.t, Connection__Process__Exec.Error.t)
    | CannotHandleURLsATM(URI.t)

  let toString = x =>
    switch x {
    | CannotHandleURLsATM(uri) =>
      // "Cannot handle URLs at the moment",
      URI.toString(
        uri,
      ) ++ ": Cannot handle URLs at the moment, this will be supported again in the future"

    | NotAgdaOrALS(uri, output) =>
      // "Not Agda or Agda Language Server",
      let outputInfo = if output == "" {
        "no output (empty string)"
      } else {
        "'" ++ output ++ "'"
      }
      URI.toString(uri) ++ ": doesn't seem to be an Agda executable or an Agda Language Server. Output received: " ++ outputInfo
    | SomethingWentWrong(uri, e) =>
      URI.toString(uri) ++ ": " ++ Connection__Process__Exec.Error.toString(e)
    }
}

module Module: {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, IPC.t) // ALS version, Agda version, method of IPC

  // see if it's a Agda executable or a language server
  let probeFilepath: URI.t => promise<result<t, Error.t>>

  // from URI to Target
  let fromURI: URI.t => promise<result<t, Error.t>>
  let fromURIs: array<URI.t> => promise<array<result<t, Error.t>>>
  // from Target to URI
  let toURI: t => URI.t

  // from String to Target
  let fromRawPath: string => promise<result<t, Error.t>>
  let fromRawPaths: array<string> => promise<array<result<t, Error.t>>>

  // from VSCode.Uri.t to Target
  let fromVSCodeUri: VSCode.Uri.t => promise<result<t, Error.t>>

  // configuration
  let getPicked: (State__Memento.t, array<Connection__URI.t>) => promise<result<t, array<Error.t>>>
  let setPicked: (State__Memento.t, option<t>) => promise<unit>
} = {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, IPC.t) // ALS version, Agda version, method of IPC

  // see if it's a Agda executable or a language server
  let probeFilepath = async uri => {
    let path = URI.toString(uri)
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
            Some({Connection__Target__ALS__LSP__Binding.env: env})
          | None => None
          }
          Ok(ALS(alsVersion, agdaVersion, Connection__Target__IPC.ViaPipe(path, [], lspOptions)))
        | _ => Error(Error.NotAgdaOrALS(uri, output))
        }
      }
    | Error(error) => Error(Error.SomethingWentWrong(uri, error))
    }
  }

  let fromURI = async uri =>
    switch uri {
    | URI.URL(_) => Error(Error.CannotHandleURLsATM(uri))
    | Filepath(_) =>
      switch await probeFilepath(uri) {
      | Ok(target) => Ok(target)
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

  let fromURIs = uris => uris->Array.map(fromURI)->Promise.all

  let toURI = target =>
    switch target {
    | Agda(_, path) => URI.Filepath(path)
    | ALS(_, _, ViaPipe(path, _, _)) => URI.Filepath(path)
    | ALS(_, _, ViaTCP(url)) => URI.URL(url)
    }

  // Try to find the previously picked connection
  // The previously picked connection is stored in the memento, if it doesn't exist
  // the first usable connection target from the supplied paths is returned
  let getPicked = async (memento: State__Memento.t, rawSuppliedPaths: array<Connection__URI.t>) => {
    // convert raw supplied paths to targets
    // and filter out the invalid ones
    let (suppliedTargets, suppliedTargetsErrors) =
      (await fromURIs(rawSuppliedPaths))->Util.Result.partition

    let pickFromSuppliedTargetsInstead = switch suppliedTargets[0] {
    | None => Error(suppliedTargetsErrors)
    | Some(target) => Ok(target)
    }

    switch memento->State__Memento.get("pickedConnection") {
    | Some(rawPathFromMemento) =>
      switch await fromRawPath(rawPathFromMemento) {
      | Error(_) =>
        // the path in the memento is invalid
        // remove it from the memento
        await memento->State__Memento.set("pickedConnection", None)
        pickFromSuppliedTargetsInstead
      | Ok(targetFromMemento) =>
        let existsInSuppliedTargets = suppliedTargets->Util.Array.includes(targetFromMemento)
        if existsInSuppliedTargets {
          Ok(targetFromMemento)
        } else {
          // the path in the memento is not in the supplied paths
          // remove it from the memento
          await memento->State__Memento.set("pickedConnection", None)
          pickFromSuppliedTargetsInstead
        }
      }
    | None => pickFromSuppliedTargetsInstead
    }
  }

  let setPicked = (memento: State__Memento.t, target) =>
    switch target {
    | None => memento->State__Memento.set("pickedConnection", None)
    | Some(target) =>
      memento->State__Memento.set("pickedConnection", Some(toURI(target)->URI.toString))
    }
}

include Module
