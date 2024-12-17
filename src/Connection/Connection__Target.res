module Resolver = Connection__Resolver
module IPC = Connection__IPC

module Module: {
    type version = string
    type t = 
        | Agda(version, string) // version, path
        | ALS(version, IPC.t) // version, method of IPC
  // try to find the Agda Language Server
  let tryALS: (string, Resolver.GitHub.Download.Event.t => unit) => promise<(option<IPC.t>, array<Resolver.Error.t>)>
  // try to find the Agda executable
  let tryAgda: unit => promise<(option<IPC.t>, array<Resolver.Error.t>)>

  let getLocalInstallations: unit => promise<array<result<unit, (string, string)>>>
} = {
  type version = string
  type t = 
    | Agda(version, string) // version, path
    | ALS(version, IPC.t) // version, method of IPC

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

  let chooseFromReleases = (releases: array<Resolver.GitHub.Release.t>): option<Resolver.GitHub.Target.t> => {
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
        let expectedAssetName = "als-" ++ name ++ ".zip"
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

  // see if the server is available
  // priorities: TCP => Prebuilt => StdIO
  let tryALS = async (globalStoragePath, onDownload) => {
    let port = Config.Connection.getAgdaLanguageServerPort()
    let name = "als"

    await Resolver.searchMany([
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
  }

  let tryAgda = () => {
    let storedPath = Config.Connection.getAgdaPath()
    let storedName = Config.Connection.getAgdaVersion()
    Resolver.searchMany([FromFile(storedPath), FromCommand(storedName)])
  }

  let validateLocalInstallation = async path =>  {
    module Process = Connection__Target__Agda__Process
    let pathWithArgs = path ++ " --version"
    switch await Process.Validation.run(pathWithArgs, a => {
      Js.log(pathWithArgs ++ " => " ++ a)
      Ok()
    }) {
      | Ok(_) => Ok()
      | Error(error) => Error(Process.Validation.Error.toString(error)) 
    }
  }

  let getLocalInstallations = () => {
    let paths = [Config.Connection.getAgdaPath()]

    Promise.all(paths->Array.map(validateLocalInstallation))
  }



}

include Module