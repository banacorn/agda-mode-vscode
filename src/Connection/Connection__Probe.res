module Version = Util.Version
open LanguageServerMule
open Source.GitHub

// module Platform = {
//   module GetOs = {
//     type t = {"os": string, "dist": string, "codename": string, "release": string}

//     @module
//     external getos: (('e, t) => unit) => unit = "getos"

//     let runAsPromise = (): Promise.Js.t<t, 'e> => {
//       let (promise, resolve, reject) = Promise.Js.pending()
//       getos((e, os) => {
//         let e = Js.Nullable.toOption(e)
//         switch e {
//         | Some(e) => reject(e)
//         | None => resolve(os)
//         }
//       })
//       promise
//     }
//   }

//   type t = Windows | MacOS | Ubuntu | Others

//   let determine = () =>
//     switch Node_process.process["platform"] {
//     | "darwin" => Promise.resolved(MacOS)
//     | "linux" =>
//       // determine the distro
//       GetOs.runAsPromise()->Promise.map(result =>
//         switch result["dist"] {
//         | "Ubuntu" => Ubuntu
//         | _ => Others
//         }
//       )
//     | "win32" => Promise.resolved(Windows)
//     | _others => Promise.resolved(Others)
//     }
// }

// let chooseFromReleases = (platform: Platform.t, releases: array<Release.t>): option<Target.t> => {
//   let chooseRelease = (releases: array<Release.t>) => {
//     // fetch the latest release
//     let compare = (x: Release.t, y: Release.t) => {
//       let xTime = Js.Date.getTime(Js.Date.fromString(x.created_at))
//       let yTime = Js.Date.getTime(Js.Date.fromString(y.created_at))
//       compare(yTime, xTime)
//     }
//     let sorted = Js.Array.sortInPlaceWith(compare, releases)
//     sorted[0]
//   }

//   let chooseAsset = (release: Release.t) => {
//     // expected suffix of asset name
//     let expectedSuffix = switch platform {
//     | MacOS => Some("macos.zip")
//     | Ubuntu => Some("ubuntu.zip")
//     | Windows => Some("windows.zip")
//     | Others => None
//     }

//     // find the corresponding asset
//     expectedSuffix
//     ->Option.flatMap(suffix => {
//       let matched = release.assets->Array.keep(asset => Js.String2.endsWith(asset.name, suffix))
//       matched[0]
//     })
//     ->Option.map(asset => {
//       saveAsFileName: release.tag_name ++ "-" ++ Node_process.process["platform"],
//       Target.release,
//       asset,
//     })
//   }

//   chooseRelease(releases)->Option.flatMap(chooseAsset)
// }

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
      chooseFromReleases: UseLatest,
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
