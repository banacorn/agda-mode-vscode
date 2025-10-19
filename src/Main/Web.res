// Web-specific entry point that creates Platform.makeWeb() dependencies
// This file will be used for the web bundle

// Web implementation - provides mock/limited functionality for browser environment
module Web: Platform.PlatformOps = {
  let determinePlatform = () => {
    Promise.resolve(Ok(Connection__Download__Platform.Web))
  }

  let findCommand = (_command, ~timeout as _timeout=1000) =>
    Promise.resolve(Error(Connection__Command.Error.NotFound))

  let alreadyDownloaded = Connection__Download.alreadyDownloaded

  let resolveDownloadOrder = (order: Connection__Download.DownloadOrderAbstract.t, useCache) => async (
    memento,
    globalStorageUri,
    platform,
  ) => {
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

  // Always download, because there are no alternatives in web environment
  let askUserAboutDownloadPolicy = () => Promise.resolve(Config.Connection.DownloadPolicy.Yes)

  let openFolder = _uri => Promise.resolve()
}

// Create platform dependencies for web environment
let make = (): Platform.t => module(Web)

// this function is the entry point for the web extension bundle
let activate = context => {
  // Delegate to common activation logic
  Main.activate(make(), context)
}

let deactivate = Main.deactivate
