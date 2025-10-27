// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommand = Connection__Command.search

  let alreadyDownloaded = async (globalStorageUri, order) => {
    switch order {
    | Connection__Download.DownloadOrderAbstract.LatestALS => {
        let uri = VSCode.Uri.joinPath(globalStorageUri, ["latest-als", "als"])
        switch await FS.stat(uri) {
        | Ok(_) => Some(uri->VSCode.Uri.fsPath)
        | Error(_) => None
        }
      }
    | Connection__Download.DownloadOrderAbstract.DevALS => {
        // Desktop: Only check for native binary (als or als.exe), not WASM
        let alsUri = VSCode.Uri.joinPath(globalStorageUri, ["dev-als", "als"])
        switch await FS.stat(alsUri) {
        | Ok(_) => Some(alsUri->VSCode.Uri.fsPath)
        | Error(_) => None
        }
      }
    }
  }

  let resolveDownloadOrder = (
    order: Connection__Download.DownloadOrderAbstract.t,
    useCache,
  ) => async (memento, globalStorageUri, platform) => {
    let repo = switch order {
    | LatestALS => Connection__LatestALS.makeRepo(globalStorageUri)
    | DevALS => Connection__DevALS.makeRepo(globalStorageUri)
    }

    let toDownloadOrder = switch order {
    | LatestALS => Connection__LatestALS.toDownloadOrder(_, platform)
    | DevALS => Connection__DevALS.toDownloadOrder(_, platform)
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
}

// Create platform dependencies for desktop environment
let make = (): Platform.t => module(Desktop)

// this function is the entry point for the desktop extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
