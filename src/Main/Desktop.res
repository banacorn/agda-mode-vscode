// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommand = Connection__Command.search

  let alreadyDownloaded = async (globalStorageUri, channel) => {
    switch channel {
    | Connection__Download.Channel.LatestALS => {
        let uri = VSCode.Uri.joinPath(globalStorageUri, ["latest-als", "als"])
        switch await FS.stat(uri) {
        | Ok(_) =>
          Util.log("[ debug ] alreadyDownloaded LatestALS: found", uri->VSCode.Uri.fsPath)
          Some(uri->VSCode.Uri.fsPath)
        | Error(_) =>
          Util.log("[ debug ] alreadyDownloaded LatestALS: not found", uri->VSCode.Uri.fsPath)
          None
        }
      }
    | Connection__Download.Channel.DevALS => {
        let result = switch await Connection__Download__Platform.determine() {
        | Ok(platform) =>
          await Connection__Download.findReleaseManagedDownloadedForDesktopPlatform(
            globalStorageUri,
            platform,
          )
        | Error(_) =>
          await Connection__Download.findReleaseManagedDownloaded(
            globalStorageUri,
            artifact =>
              Connection__Download.DownloadArtifact.Platform.isWasm(artifact.platform),
            uri => VSCode.Uri.toString(uri),
          )
        }
        switch result {
        | Some(path) =>
          Util.log("[ debug ] alreadyDownloaded DevALS: release-managed found", path)
        | None =>
          Util.log("[ debug ] alreadyDownloaded DevALS: release-managed not found", "")
        }
        result
      }
    }
  }

  let resolveDownloadChannel = (
    channel: Connection__Download.Channel.t,
    useCache,
  ) => async (memento, globalStorageUri, platform) => {
    switch channel {
    | LatestALS | DevALS =>
      let repo = switch channel {
      | LatestALS => Connection__LatestALS.makeRepo(globalStorageUri)
      | DevALS => Connection__DevALS.makeRepo(globalStorageUri)
      }

      let toDownloadOrder = switch channel {
      | LatestALS => Connection__LatestALS.toDownloadOrder(_, platform)
      | DevALS => Connection__DevALS.toDownloadOrder(_, platform)
      }

      switch await Connection__Download.getReleaseManifestFromGitHub(memento, repo, ~useCache) {
      | Error(error) => Error(error)
      | Ok(releases) => toDownloadOrder(releases)
      }
    }
  }
  let download = (globalStorageUri, channel, ~trace=Connection__Download__Trace.noop) =>
    Connection__Download.download(globalStorageUri, channel, ~trace)

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
