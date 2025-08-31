// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommand = Connection__Command.search

  let alreadyDownloaded = Connection__Download.alreadyDownloaded

  let resolveDownloadOrder = (
    order: Connection__Download.DownloadOrderAbstract.t,
    useCache,
  ) => async (memento, globalStorageUri, platform) => {
    let repo = switch order {
    | LatestALS => Connection__LatestALS.makeRepo(globalStorageUri)
    | DevALS => Connection__DevALS.makeRepo(globalStorageUri)
    | DevWASMALS => Connection__DevWASMALS.makeRepo(globalStorageUri)
    }

    let toDownloadOrder = switch order {
    | LatestALS => Connection__LatestALS.toDownloadOrder(_, platform)
    | DevALS => Connection__DevALS.toDownloadOrder(_, platform)
    | DevWASMALS => Connection__DevWASMALS.toDownloadOrder(_)
    }

    switch await Connection__Download.getReleaseManifestFromGitHub(memento, repo, ~useCache) {
    | Error(error) => Error(error)
    | Ok(releases) => toDownloadOrder(releases)
    }
  }
  let download = Connection__Download.download

  let askUserAboutDownloadPolicy = async () => {
    let messageOptions = {
      VSCode.MessageOptions.modal: true,
      detail: "Do you want to download and install the latest Agda Language Server?",
    }
    let result = await VSCode.Window.showWarningMessageWithOptions(
      "Cannot find Agda or Agda Language Server",
      messageOptions,
      [
        Config.Connection.DownloadPolicy.toString(Yes),
        Config.Connection.DownloadPolicy.toString(No),
      ],
    ) // 📺

    // parse the result
    result->Option.mapOr(
      Config.Connection.DownloadPolicy.No,
      Config.Connection.DownloadPolicy.fromString,
    )
  }

  let openFolder = async uri => {
    let _ = await VSCode.Env.openExternal(uri)
  }
}

// Create platform dependencies for desktop environment
let make = (): Platform.t => module(Desktop)

// this function is the entry point for the desktop extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
