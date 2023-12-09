module Version = Util.Version
open LanguageServerMule
open Source.GitHub
open Belt

module Platform = {
  module GetOs = {
    type t = {"os": string, "dist": string, "codename": string, "release": string}

    @module
    external getos: (('e, t) => unit) => unit = "getos"

    let runAsPromise = (): Promise.Js.t<t, 'e> => {
      let (promise, resolve, reject) = Promise.Js.pending()
      getos((e, os) => {
        let e = Js.Nullable.toOption(e);
        switch e {
            | Some(e) => reject(e)
            | None => resolve(os)
        }
      })
      promise
    }
  }

  type t = Windows | MacOS | Ubuntu | Others

  let determine = () =>
    switch Node_process.process["platform"] {
    | "darwin" => Promise.resolved(MacOS)
    | "linux" =>
      // determine the distro
      GetOs.runAsPromise()->Promise.map(result =>
        switch result["dist"] {
        | "Ubuntu" => Ubuntu
        | _ => Others
        }
      )
    | "win32" => Promise.resolved(Windows)
    | _others => Promise.resolved(Others)
    }
}

let chooseFromReleases = (platform: Platform.t, releases: array<Release.t>): option<Target.t> => {
  // CURRENTLY ACCEPTED RANGE OF ALS: [v0.2, v0.3)
  let chooseRelease = (releases: array<Release.t>) => {
    let lowerBound = "v0.2.0.0"
    let upperBound = "v0.3.0.0"
    let withinBound = x => {
      let lower = Version.compare(x, lowerBound)
      let upper = Version.compare(x, upperBound)
      (lower == Version.EQ || lower == Version.GT) && upper == Version.LT
    }
    let matched = releases->Array.keep(release => withinBound(release.tagName))
    let compare = (x: Release.t, y: Release.t) =>
      switch Version.compare(x.tagName, y.tagName) {
      | Version.GT => -1
      | Version.EQ => 0
      | Version.LT => 1
      }
    let sorted = Js.Array.sortInPlaceWith(compare, matched)
    sorted[0]
  }

  let chooseAsset = (release: Release.t) => {
    // expected suffix of asset name
    let expectedSuffix = switch platform {
    | MacOS => Some("macos.zip")
    | Ubuntu => Some("ubuntu.zip")
    | Windows => Some("windows.zip")
    | Others => None
    }

    // find the corresponding asset
    expectedSuffix
    ->Option.flatMap(suffix => {
      let matched = release.assets->Array.keep(asset => Js.String2.endsWith(asset.name, suffix))
      matched[0]
    })
    ->Option.map(asset => {
      Target.srcUrl: asset.url,
      fileName: release.tagName ++ "-" ++ Node_process.process["platform"],
      release: release,
      asset: asset,
    })
  }

  chooseRelease(releases)->Option.flatMap(chooseAsset)
}

let afterDownload = (fromCached, (path, target)) => {
  let execPath = Node.Path.join2(path, "als")
  // include "Agda_datadir" in the environment variable
  let options = {
    let assetPath = Node.Path.join2(path, "data")
    let env = Js.Dict.fromArray([("Agda_datadir", assetPath)])
    Client__LSP__Binding.ExecutableOptions.make(~env, ())
  }
  // because it should've been chmod'ed after download
  // because there's no need of chmod'ing on Windows
  let shouldChmod777 = !fromCached && Node_process.process["platform"] != "win32"
  if shouldChmod777 {
    chmodExecutable(execPath)->Promise.map(_ => ())
  } else {
    Promise.resolved()
  }->Promise.map(() => Ok((execPath, [], Some(options), target)))
}

// see if the server is available
// priorities: TCP => Prebuilt => StdIO
let probeLSP = (globalStoragePath, onDownload) => {
  let port = Config.Connection.getAgdaLanguageServerPort()
  let name = "als"

  Platform.determine()->Promise.flatMap(platform =>
    Source.Module.searchUntilSuccess([
      // when developing ALS, use `:main -p` in GHCi to open a port on localhost
      Source.FromTCP(port, "localhost"),
      // #71: Prefer locally installed language server binary over bundled als
      // https://github.com/banacorn/agda-mode-vscode/issues/71
      Source.FromCommand(name),
      Source.FromGitHub({
        username: "banacorn",
        repository: "agda-language-server",
        userAgent: "agda-mode-vscode",
        globalStoragePath: globalStoragePath,
        chooseFromReleases: chooseFromReleases(platform),
        onDownload: onDownload,
        afterDownload: afterDownload,
        log: Js.log,
        cacheInvalidateExpirationSecs: 86400,
      }),
    ])
  )
}

let probeEmacs = () => {
  let storedPath = Config.Connection.getAgdaPath()
  let storedName = Config.Connection.getAgdaVersion()
  Source.Module.searchUntilSuccess([Source.FromFile(storedPath), Source.FromCommand(storedName)])
}
