// Desktop-specific entry point that creates Platform.makeDesktop() dependencies
// This file will be used for the desktop bundle

// Desktop implementation - uses actual platform-specific code
module Desktop: Platform.PlatformOps = {
  let determinePlatform = Connection__Download__Platform.determine

  let findCommand = Connection__Command.search

  let alreadyDownloaded = Connection__LatestALS.alreadyDownloaded

  let downloadLatestALS = (memento, globalStorageUri) => async platform => {
    switch await Connection__LatestALS.download(memento, globalStorageUri)(platform) {
    | Ok(endpoint) => Ok(endpoint)
    | Error(error) => Error(error)
    }
  }

  let getInstalledEndpointsAndPersistThem = async (globalStorageUri: VSCode.Uri.t) => {
    // Get all paths of Agda or ALS from:
    //    * `agdaMode.connection.paths` in the settings
    //    * `agda` and `als` from the PATH
    //    * `agda` and `als` from the download folder

    let endpoints = Dict.make()

    // Helper function to infer endpoint type from filename
    let inferEndpointType = (filename: string) => {
      let baseName = filename->String.toLowerCase->NodeJs.Path.basename
      // Remove common executable extensions
      let cleanName =
        baseName
        ->String.replace(".exe", "")
        ->String.replace(".cmd", "")
        ->String.replace(".bat", "")

      if cleanName == "agda" || cleanName->String.startsWith("agda-") {
        Memento.Endpoints.Agda(None)
      } else if cleanName == "als" || cleanName->String.startsWith("als-") {
        Memento.Endpoints.ALS(None)
      } else {
        Memento.Endpoints.Unknown
      }
    }

    // Add paths from settings with inference
    Config.Connection.getAgdaPaths()->Array.forEach(path => {
      let filename = NodeJs.Path.basename(path)
      let endpoint = inferEndpointType(filename)
      endpoints->Dict.set(path, endpoint)
    })

    // add `agda` and `als` from the PATH
    switch await Connection__Command.search("agda") {
    | Ok(path) => endpoints->Dict.set(path, Memento.Endpoints.Agda(None))
    | Error(_) => ()
    }
    switch await Connection__Command.search("als") {
    | Ok(path) => endpoints->Dict.set(path, Memento.Endpoints.ALS(None))
    | Error(_) => ()
    }

    // assuming that all `als` and `agda` executables are placed in folders one level below the download folder
    // add `agda` and `als` from the download folder
    let addAgdaOrALS = async (folderURI, fileName) => {
      let executablePath = VSCode.Uri.joinPath(folderURI, [fileName])
      let path = VSCode.Uri.fsPath(executablePath)
      let endpoint = inferEndpointType(fileName)
      endpoints->Dict.set(path, endpoint)
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

    endpoints
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
