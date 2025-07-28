// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommands = Connection__Command.findCommands

  let alreadyDownloaded = globalStorageUri =>
    Connection__LatestALS.alreadyDownloaded(globalStorageUri)

  let downloadLatestALS = (memento, globalStorageUri) =>
    Connection__LatestALS.download(memento, globalStorageUri)

  let getInstalledEndpointsAndPersistThem = async (globalStorageUri: VSCode.Uri.t) => {
    // 1. Get all installed endpoints from:
    //    * `agdaMode.connection.paths` in the settings
    //    * `agda` and `als` from the PATH
    //    * `agda` and `als` from the download folder
    // 2. Persist them in the settings
    // async (findCommands, globalStorageUri: VSCode.Uri.t) => {
    // from `agdaMode.connection.paths` in the settings
    let dict = {
      let pairs =
        await Config.Connection.getAgdaPaths()
        ->Array.map(async uri => {
          let target = await Connection__Endpoint.fromURI(uri)
          (Connection__URI.toString(uri), target)
        })
        ->Promise.all

      Dict.fromArray(pairs)
    }

    // add `agda` and `als` from the PATH
    switch await findCommands(["agda"]) {
    | Ok(agda) =>
      let uri = agda->Connection__Endpoint.toURI
      await Config.Connection.addAgdaPath(uri)
      dict->Dict.set(uri->Connection__URI.toString, Ok(agda))
    | Error(_) => ()
    }
    switch await findCommands(["als"]) {
    | Ok(als) =>
      let uri = als->Connection__Endpoint.toURI
      await Config.Connection.addAgdaPath(uri)
      dict->Dict.set(uri->Connection__URI.toString, Ok(als))
    | Error(_) => ()
    }

    // assuming that all `als` and `agda` executables are placed in folders one level below the download folder
    // add `agda` and `als` from the download folder
    let addAgdaOrALS = async (folderURI, fileName) => {
      let executablePath = VSCode.Uri.joinPath(folderURI, [fileName])
      switch await Connection__Endpoint.fromRawPath(VSCode.Uri.fsPath(executablePath)) {
      | Ok(target) =>
        let uri = target->Connection__Endpoint.toURI
        await Config.Connection.addAgdaPath(uri)
        dict->Dict.set(uri->Connection__URI.toString, Ok(target))
      | Error(_) => ()
      }
    }

    // handle files in the folders in the global storage
    let handleFile = folderPath => async ((filename, _fileType)) =>
      switch filename {
      | "agda" | "agda.exe" | "als" | "als.exe " => await addAgdaOrALS(folderPath, filename)
      | _ => ()
      }

    // handle folders in the global storage
    let handleFolder = async ((filename, _fileType)) => {
      let uri = VSCode.Uri.joinPath(globalStorageUri, [filename])
      switch await FS.readDirectory(uri) {
      | Ok(files) =>
        let _ = await files->Array.map(handleFile(uri))->Promise.all
      | Error(_) => ()
      }
    }

    // read the global storage directory and handle each folder
    switch await FS.readDirectory(globalStorageUri) {
    | Ok(folders) =>
      let _ = await folders->Array.map(handleFolder)->Promise.all
    | Error(_) => ()
    }

    dict
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
