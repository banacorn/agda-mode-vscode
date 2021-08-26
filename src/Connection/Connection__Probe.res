module Version = Util.Version
open LanguageServerMule
open Source.GitHub
open Belt

let chooseFromReleases = (releases: array<Release.t>): option<Target.t> => {

  // CURRENT RANGE: [v0.1, v0.2)
  let chooseRelease = (releases: array<Release.t>) => {
    let lowerBound = "v0.1.0.0"
    let upperBound = "v0.2.0.0"
    let withinBound = x => {
      let lower = Version.compare(x, lowerBound)
      let upper = Version.compare(x, upperBound)
      (lower == Version.EQ || lower == Version.GT) && upper == Version.LT
    }

    Js.log2("releases", releases->Array.map(release => release.tagName))

    let matched = releases->Array.keep(release => withinBound(release.tagName))
    Js.log2("matched", matched)
    matched[0]
  }

  let chooseAsset = (release: Release.t) => {
    // expected suffix of asset name
    let os = Node_process.process["platform"]
    let expectedSuffix = switch os {
    | "darwin" => Some("macos.zip")
    | "linux" => Some("ubuntu.zip")
    | "win32" => Some("windows.zip")
    | _others => None
    }

    // find the corresponding asset
    expectedSuffix
    ->Option.flatMap(suffix => {
      let matched = release.assets->Array.keep(asset => Js.String2.endsWith(asset.name, suffix))
      matched[0]
    })
    ->Option.map(asset => {
      Target.srcUrl: asset.url,
      fileName: release.tagName ++ "-" ++ os,
      release: release,
      asset: asset,
    })
  }

  chooseRelease(releases)->Option.flatMap(chooseAsset)
}

// see if the server is available
// priorities: TCP => Prebuilt => StdIO
let probeLSP = (globalStoragePath, onDownload) => {
  let port = Config.Connection.getAgdaLanguageServerPort()
  let name = "als"

  Source.Module.searchUntilSuccess([
    Source.FromTCP(port, "localhost"),
    Source.FromGitHub({
      username: "banacorn",
      repository: "agda-language-server",
      userAgent: "agda-mode-vscode",
      globalStoragePath: globalStoragePath,
      chooseFromReleases: chooseFromReleases,
      onDownload: onDownload,
      cacheInvalidateExpirationSecs: 86400,
      cacheID: Config.version,
    }),
    Source.FromCommand(name),
  ])
}

let probeEmacs = () => {
  let storedPath = Config.Connection.getAgdaPath()
  Source.Module.searchUntilSuccess([Source.FromFile(storedPath), Source.FromCommand("agda")])
}
