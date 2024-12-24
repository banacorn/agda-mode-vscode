module Resolver = Connection__Resolver
module IPC = Connection__IPC
module Error = Connection__Error

module Module: {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, result<IPC.t, string>) // ALS version, Agda version, method of IPC
  // try to find the Agda Language Server
  let tryALS: (
    string,
    Resolver.GitHub.Download.Event.t => unit,
  ) => promise<(option<IPC.t>, array<Resolver.Error.t>)>
  // try to find the Agda executable
  let tryAgda: unit => promise<(option<IPC.t>, array<Resolver.Error.t>)>

  // returns a list of paths stored in the configuration
  let getRawPathsFromConfig: unit => array<string>
  // see if the path points to a valid Agda executable or language server
  let probePath: string => promise<result<t, Error.t>>
  // extract the path from the target
  let getPath: t => string

  // interfacing
  let getAll: unit => promise<array<result<t, Error.t>>>
  let getPicked: State__Type.t => promise<option<t>>
  let setPicked: (State__Type.t, t) => promise<unit>
} = {
  type version = string
  type t =
    | Agda(version, string) // Agda version, path
    | ALS(version, version, result<IPC.t, string>) // ALS version, Agda version, method of IPC

  let afterDownload = async (isCached, (path, target)) => {
    // include "Agda_datadir" in the environment variable
    let options = {
      let assetPath = NodeJs.Path.join2(path, "data")
      let env = Dict.fromArray([("Agda_datadir", assetPath)])
      {
        Connection__Target__ALS__LSP__Binding.env: env,
      }
    }
    // chmod the executable after download
    // no need to chmod if:
    //    1. it's cached, already chmoded
    //  or
    //    2. it's on Windows
    let execPath = NodeJs.Path.join2(path, "als")
    let shouldChmod = !isCached && NodeJs.Os.platform() != "win32"
    if shouldChmod {
      let _ = await Resolver.GitHub.chmodExecutable(execPath)
    }

    Ok((execPath, [], Some(options), target))
  }

  let chooseFromReleases = (releases: array<Resolver.GitHub.Release.t>): option<
    Resolver.GitHub.Target.t,
  > => {
    let platform = switch NodeJs.Os.platform() {
    | "darwin" =>
      switch Node__OS.arch() {
      | "x64" => Some("macos-x64")
      | "arm64" => Some("macos-arm64")
      | _ => None
      }
    | "linux" => Some("ubuntu")
    | "win32" => Some("windows")
    | _ => None
    }
    switch Resolver.GitHub.Release.chooseLatest(releases) {
    | Some(release) =>
      switch platform {
      | Some(name) =>
        // TODO: do not hardcode the asset name
        let expectedAssetName = "als-Agda-2.7.0.1-" ++ name ++ ".zip"
        switch release.assets->Resolver.GitHub.Asset.chooseByName(expectedAssetName) {
        | Some(asset) =>
          Some({
            saveAsFileName: release.tag_name ++ "-" ++ name,
            release,
            asset,
          })
        | None => None
        }
      | None => None
      }
    | None => None
    }
  }

  let makeAgdaLanguageServerRepo: string => Resolver.GitHub.Repo.t = globalStoragePath => {
    username: "agda",
    repository: "agda-language-server",
    userAgent: "agda/agda-mode-vscode",
    globalStoragePath,
    cacheInvalidateExpirationSecs: 86400,
  }

  // see if it's a valid Agda executable or language server
  let probeFilepath = async path => {
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
          Ok(ALS(alsVersion, agdaVersion, Error(path)))
        | _ => Error(Error.NotAgdaOrALS(path))
        }
      }
    | Error(error) => Error(Error.ValidationError(path, error))
    }
  }

  @module external untildify: string => string = "untildify"

  type path = Filepath(string) | URL(NodeJs.Url.t)
  let resolvePath = async path => {
    // trying to parse the path as a URL
    let result = try Some(NodeJs.Url.make(path)) catch {
    | _ => None
    }
    switch result {
    | Some(url) =>
      // expect the scheme to be "lsp:"
      if url.protocol == "lsp:" {
        Some(URL(url))
      } else {
        None
      }
    | None =>
      // treat the path as a file path
      let path = untildify(path)
      let path = NodeJs.Path.normalize(path)
      Some(Filepath(path))
    }
  }

  // see if the server is available
  // priorities: TCP => Prebuilt => StdIO
  let tryALS = async (globalStoragePath, onDownload) => {
    let port = Config.Connection.getAgdaLanguageServerPort()
    let name = "als"

    let result = await Resolver.searchMany([
      // when developing ALS, use `:main -p` in GHCi to open a port on localhost
      FromTCP(port, "localhost"),
      // #71: Prefer locally installed language server binary over bundled als
      // https://github.com/banacorn/agda-mode-vscode/issues/71
      FromCommand(name),
      // download the language server from GitHub
      FromGitHub(
        makeAgdaLanguageServerRepo(globalStoragePath),
        {
          chooseFromReleases,
          onDownload,
          afterDownload,
          log: x => Js.log(x),
        },
      ),
    ])

    // add the path to the configuration
    // switch result {
    // | (None, _) => ()
    // | (Some(method), _) =>
    //   switch method {
    //   | ViaPipe(path, _, _, _) => await Config.Connection.addAgdaPath(path)
    //   | _ => ()
    //   }
    // }

    result
  }

  let tryAgda = async () => {
    let storedPaths = Config.Connection.getAgdaPaths()
    let storedName = Config.Connection.getAgdaVersion()

    let paths = storedPaths->Array.map(path => Resolver.FromFile(path))
    let result = await Resolver.searchMany(Array.flat([paths, [FromCommand(storedName)]]))

    // let _ = await Promise.all(
    //   storedPaths->Array.map(async path => {
    //     switch await resolvePath(path) {
    //     | Error(_) => Js.log("Error: " ++ path)
    //     | Ok(path) =>
    //       let result = await probe(path)
    //       Js.log(result)
    //     }
    //   }),
    // )

    // add the path to the configuration
    // switch result {
    // | (None, _) => ()
    // | (Some(method), _) =>
    //   switch method {
    //   | ViaPipe(path, _, _, _) => await Config.Connection.addAgdaPath(path)
    //   | _ => ()
    //   }
    // }

    result
  }

  let getRawPathsFromConfig = () => Config.Connection.getAgdaPaths()

  let probePath = async rawPath => {
    switch await resolvePath(rawPath) {
    | None => Error(Error.CannotResolvePath(rawPath))
    | Some(URL(url)) => Error(Error.CannotResolvePath(url.toString()))
    | Some(Filepath(path)) => await probeFilepath(path)
    }
  }

  let getPath = target =>
    switch target {
    | Agda(_, path) => path
    | ALS(_, _, Ok(ViaPipe(path, _, _, _))) => path
    | ALS(_, _, Ok(ViaTCP(port, host, _))) => "lsp://" ++ host ++ ":" ++ string_of_int(port)
    | ALS(_, _, Error(path)) => path
    }

  // returns a list of connection targets
  let getAll = () => Promise.all(getRawPathsFromConfig()->Array.map(probePath))

  // returns the first usable connection target
  let getFirstUsable = async () => {
    let targets = await getAll()
    targets->Array.reduce(None, (acc, target) =>
      switch acc {
      | Some(_) => acc
      | None =>
        switch target {
        | Ok(target) => Some(target)
        | Error(_) => None
        }
      }
    )
  }

  // returns the previously picked connection target
  let getPicked = async (state: State__Type.t) =>
    switch state.memento->State__Type.Memento.get("pickedConnection") {
    | Some(fromMemento) =>
      // see if it still exists in the configuration
      let fromConfig = await getAll()
      let stillExists = fromConfig->Array.reduce(false, (acc, target) =>
        acc ||
        switch target {
        | Ok(target) => getPath(target) == fromMemento
        | Error(_) => false
        }
      )
      if stillExists {
        switch await probePath(fromMemento) {
        | Ok(target) => Some(target)
        | Error(_) => None
        }
      } else {
        // remove the invalid path from the memento
        await state.memento->State__Type.Memento.update("pickedConnection", None)
        None
      }
    | None => await getFirstUsable()
    }

  let setPicked = (state: State__Type.t, target) => {
    state.memento->State__Type.Memento.update("pickedConnection", getPath(target))
  }
}

include Module
