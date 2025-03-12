module IPC = Connection__IPC
module URI = Connection__URI
module Process = Connection__Process

module Error = {
  type t =
    | NotAgdaOrALS(URI.t)
    | SomethingWentWrong(URI.t, Connection__Process__Exec.Error.t)
    | CannotHandleURLsATM(URI.t)

  let toString = x =>
    switch x {
    | CannotHandleURLsATM(uri) =>
      // "Cannot handle URLs at the moment",
      URI.toString(
        uri,
      ) ++ ": Cannot handle URLs at the moment, this will be supported again in the future"

    | NotAgdaOrALS(uri) =>
      // "Not Agda or Agda Language Server",
      URI.toString(uri) ++ ": doesn't seem to be an Agda executable or an Agda Language Server"
    | SomethingWentWrong(uri, e) =>
      URI.toString(uri) ++ ": " ++ Connection__Process__Exec.Error.toString(e)
    }
}

module Module: {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, result<IPC.t, string>) // ALS version, Agda version, method of IPC

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

  // configuration
  let getPicked: (State__Memento.t, array<Connection__URI.t>) => promise<result<t, array<Error.t>>>
  let setPicked: (State__Memento.t, option<t>) => promise<unit>
} = {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, result<IPC.t, string>) // ALS version, Agda version, method of IPC

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
          // the executable needs to be accompanied by a `data` directory
          // which can be specified by the environment variable "Agda_datadir"
          // prebuilt executables on GitHub have this directory placed alongside the executable
          let prebuildDataDirPath = NodeJs.Path.join([path, "..", "data"])
          let isPrebuilt = switch await NodeJs.Fs.access(prebuildDataDirPath) {
          | () => true
          | exception _ => false
          }
          let lspOptions = if isPrebuilt {
            let assetPath = NodeJs.Path.join([path, "..", "data"])
            let env = Dict.fromArray([("Agda_datadir", assetPath)])
            Some({
              Connection__Target__ALS__LSP__Binding.env: env,
            })
          } else {
            None
          }

          Ok(
            ALS(
              alsVersion,
              agdaVersion,
              Ok(Connection__IPC.ViaPipe(path, [], lspOptions, Connection__IPC.FromFile(path))),
            ),
          )
        | _ => Error(Error.NotAgdaOrALS(uri))
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

  // plural form of `fromRawPath`
  let fromRawPaths = paths => paths->Array.map(fromRawPath)->Promise.all

  let fromURIs = uris => uris->Array.map(fromURI)->Promise.all

  let toURI = target =>
    switch target {
    | Agda(_, path) => URI.Filepath(path)
    | ALS(_, _, Ok(ViaPipe(path, _, _, _))) => URI.Filepath(path)
    | ALS(_, _, Ok(ViaTCP(url, _))) => URI.URL(url)
    | ALS(_, _, Error(path)) => URI.Filepath(path)
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
