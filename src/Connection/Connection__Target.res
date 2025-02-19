module IPC = Connection__IPC
module Error = Connection__Error
module URI = Connection__URI

module Module: {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, result<IPC.t, string>) // ALS version, Agda version, method of IPC
  // try to find the Agda Language Server
  // let tryALS: (
  //   State__Memento.t,
  //   string,
  //   Resolver.GitHub.Download.Event.t => unit,
  // ) => promise<(option<IPC.t>, array<Resolver.Error.t>)>
  // try to find the Agda executable

  // download the Agda Language Server from GitHub
  // let downloadALS: (
  //   State__Memento.t,
  //   string,
  //   Resolver.GitHub.Download.Event.t => unit,
  // ) => promise<result<(bool, Resolver.GitHub.Target.t), Resolver.GitHub.Error.t>>

  // let afterDownloadALS: (bool, (string, 'a)) => unit => promise<unit>

  // let tryAgda: unit => promise<(option<IPC.t>, array<Resolver.Error.t>)>

  // fro URI to Target
  let fromURI: URI.t => promise<result<t, Error.t>>
  let fromURIs: array<URI.t> => promise<array<result<t, Error.t>>>
  // from Target to URI
  let toURI: t => URI.t

  // from String to Target
  let fromRawPath: string => promise<result<t, Error.t>>
  let fromRawPaths: array<string> => promise<array<result<t, Error.t>>>

  // configuration
  let getPicked: (State__Memento.t, array<Connection__URI.t>) => promise<result<t, array<Error.t>>>
  // let getPicked: (State__Memento.t, array<string>) => promise<option<t>>
  let setPicked: (State__Memento.t, option<t>) => promise<unit>
} = {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, result<IPC.t, string>) // ALS version, Agda version, method of IPC

  let fromURI = async uri =>
    switch uri {
    | URI.URL(_url) => Error(Error.CannotHandleURLsATM(URI.toString(uri)))
    | Filepath(path) =>
      // see if it's a valid Agda executable or language server
      module Process = Connection__Target__Agda__Process
      let result = await Connection__Validation.run(path, ["--version"], output => Ok(output))
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
          // Ok(ALS(alsVersion, agdaVersion, Error(path)))
          | _ => Error(Error.NotAgdaOrALS(path))
          }
        }
      | Error(error) => Error(Error.ValidationError(path, error))
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
