module Version = Util.Version
open LanguageServerMule
open Source.GitHub

let afterDownload = async (isCached, (path, target)) => {
  // include "Agda_datadir" in the environment variable
  let options = {
    let assetPath = NodeJs.Path.join2(path, "data")
    let env = Dict.fromArray([("Agda_datadir", assetPath)])
    {
      LanguageServerMule.Client__LSP__Binding.env: env,
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
    let _ = await chmodExecutable(execPath)
  }

  Ok((execPath, [], Some(options), target))
}


// see if the server is available
// priorities: TCP => Prebuilt => StdIO
let probeLSP = async (globalStoragePath, onDownload) => {
  let port = Config.Connection.getAgdaLanguageServerPort()
  let name = "als"

  await Source.Module.searchUntilSuccess([
    // when developing ALS, use `:main -p` in GHCi to open a port on localhost
    Source.FromTCP(port, "localhost"),
    // #71: Prefer locally installed language server binary over bundled als
    // https://github.com/banacorn/agda-mode-vscode/issues/71
    Source.FromCommand(name),
    Source.FromGitHub({
      username: "agda",
      repository: "agda-language-server",
      userAgent: "agda/agda-mode-vscode",
      globalStoragePath,
      chooseFromReleases: releases => {
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
        switch Release.chooseLatest(releases) {
        | Some(release) =>
          switch platform {
          | Some(name) =>
            let expectedAssetName = "als-" ++ name ++ ".zip"
            switch release.assets->Asset.chooseByName(expectedAssetName) {
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
      },
      onDownload,
      afterDownload,
      log: x => Js.log(x),
      cacheInvalidateExpirationSecs: 86400,
    }),
  ])
}

let probeEmacs = () => {
  let storedPath = Config.Connection.getAgdaPath()
  let storedName = Config.Connection.getAgdaVersion()
  Source.Module.searchUntilSuccess([Source.FromFile(storedPath), Source.FromCommand(storedName)])
}
