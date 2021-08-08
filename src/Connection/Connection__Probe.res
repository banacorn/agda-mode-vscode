open LanguageServerMule
open Source.GitHub
open Belt

let chooseFromReleases = (releases: array<Release.t>): option<Target.t> => {
  let chooseRelease = (releases: array<Release.t>) => {
    let matched = releases->Array.keep(release => release.tagName == Config.version)
    matched[0]
  }

  let toFileName = (release: Release.t, asset: Asset.t) => {
    // take the "macos" part from names like "gcl-macos.zip"
    let osName = Js.String2.slice(asset.name, ~from=4, ~to_=-4)
    // the file name of the language server
    release.tagName ++ "-" ++ osName
  }

  let chooseAsset = (release: Release.t) => {
    // expected asset name
    let os = Node_process.process["platform"]
    let expectedName = switch os {
    | "darwin" => Some("gcl-macos.zip")
    | "linux" => Some("gcl-ubuntu.zip")
    | "win32" => Some("gcl-windows.zip")
    | _others => None
    }

    // find the corresponding asset
    expectedName
    ->Option.flatMap(name => {
      let matched = release.assets->Array.keep(asset => asset.name == name)
      matched[0]
    })
    ->Option.map(asset => {
      Target.srcUrl: asset.url,
      fileName: toFileName(release, asset),
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
  Source.Module.searchUntilSuccess([
    Source.FromFile(storedPath),
    Source.FromCommand("agda"),
  ])
}
