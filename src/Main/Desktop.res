// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommand = Connection__Command.search

  let alreadyDownloaded = Connection__LatestALS.alreadyDownloaded

  let downloadLatestALS = (logChannel, memento, globalStorageUri) => async platform => {
    switch await Connection__LatestALS.download(logChannel, memento, globalStorageUri)(platform) {
    | Ok(endpoint) => Ok(endpoint)
    | Error(error) => Error(error)
    }
  }


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
