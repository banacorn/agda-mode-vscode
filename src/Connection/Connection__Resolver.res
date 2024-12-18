module Command = Connection__Resolver__Command
module File = Connection__Resolver__File
module TCP = Connection__Resolver__TCP
module GitHub = Connection__Resolver__GitHub
module IPC = Connection__IPC

type t =
  | FromFile(string) // path of the program
  | FromCommand(string) // name of the command
  | FromTCP(int, string) // port, host
  | FromGitHub(Connection__Resolver__GitHub.Repo.t, Connection__Resolver__GitHub.Callbacks.t)

// error from the sources
module Error = {
  type t =
    | File(string) // path of the program
    | Command(string, Command.Error.t) // name of the command, error
    | TCP(int, string, TCP.Error.t) // port, host, error
    | GitHub(GitHub.Error.t)

  let toString = error =>
    switch error {
    | File(path) => "Trying to locate \"" ++ path ++ "\" but the file does not exist"
    | Command(name, e) =>
      "Trying to find the command \"" ++ name ++ "\": " ++ Command.Error.toString(e)
    | TCP(port, host, e) =>
      "Trying to connect to " ++
      host ++
      ":" ++
      string_of_int(port) ++
      " : " ++
      TCP.Error.toString(e)
    | GitHub(e) => "Trying to download prebuilt from GitHub: " ++ GitHub.Error.toString(e)
    }
}

module Module: {
  // returns `IPC.t` if any is found, and errors of previous searches
  let search: (t, ~timeout: int=?) => promise<result<IPC.t, Error.t>>
  let searchMany: array<t> => promise<(option<IPC.t>, array<Error.t>)>
} = {
  // returns the method of IPC if successful
  let search = async (source, ~timeout=1000) =>
    switch source {
    | FromFile(path) =>
      if File.probe(path) {
        Ok(IPC.ViaPipe(path, [], None, FromFile(path)))
      } else {
        Error(Error.File(path))
      }
    | FromCommand(name) =>
      switch await Command.search(name, ~timeout) {
      | Error(e) => Error(Error.Command(name, e))
      | Ok(path) => Ok(IPC.ViaPipe(path, [], None, FromCommand(name)))
      }
    | FromTCP(port, host) =>
      switch await TCP.probe(port, host, ~timeout) {
      | Error(e) => Error(Error.TCP(port, host, e))
      | Ok() => Ok(IPC.ViaTCP(port, host, FromTCP(port, host)))
      }
    | FromGitHub(repo, callbacks) =>
      switch await GitHub.get(repo, callbacks) {
      | Error(e) => Error(Error.GitHub(e))
      | Ok((isCached, target)) =>
        let destPath = NodeJs.Path.join2(repo.globalStoragePath, target.saveAsFileName)
        switch await callbacks.afterDownload(isCached, (destPath, target)) {
        | Error(e) => Error(Error.GitHub(e))
        | Ok((path, args, options, target)) =>
          Ok(IPC.ViaPipe(path, args, options, FromGitHub(repo, target.release, target.asset)))
        }
      }
    }

  let searchMany = async sources => {
    let rec tryUntilSuccess = async (accumErrors: list<Error.t>, input) =>
      switch input {
      | list{} => (None, list{})
      | list{x} =>
        switch await search(x) {
        | Error(e) => (None, list{e})
        | Ok(v) => (Some(v), list{})
        }
      | list{x, ...xs} =>
        switch await search(x) {
        | Error(e) =>
          let (v, es) = await tryUntilSuccess(accumErrors, xs)
          (v, list{e, ...es})
        | Ok(v) => (Some(v), accumErrors)
        }
      }
    let (client, errors) = await tryUntilSuccess(list{}, sources->List.fromArray)
    (client, List.toArray(errors))
  }

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
      let _ = await GitHub.chmodExecutable(execPath)
    }

    Ok((execPath, [], Some(options), target))
  }

  let chooseFromReleases = (releases: array<GitHub.Release.t>): option<GitHub.Target.t> => {
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
    switch GitHub.Release.chooseLatest(releases) {
    | Some(release) =>
      switch platform {
      | Some(name) =>
        let expectedAssetName = "als-" ++ name ++ ".zip"
        switch release.assets->GitHub.Asset.chooseByName(expectedAssetName) {
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

  let makeAgdaLanguageServerRepo: string => GitHub.Repo.t = globalStoragePath => {
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

    await searchMany([
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
    let storedPaths = Config.Connection.getAgdaPaths()
    let storedName = Config.Connection.getAgdaVersion()

    let paths = storedPaths->Array.map(path => FromFile(path))
    searchMany(Array.flat([paths, [FromCommand(storedName)]]))
  }

  let getLocalInstallations = () => ()
}

include Module
