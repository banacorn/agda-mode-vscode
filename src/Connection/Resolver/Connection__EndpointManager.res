// 1. Get all installed endpoints from:
//    * `agdaMode.connection.paths` in the settings
//    * `agda` and `als` from the PATH
//    * `agda` and `als` from the download folder
// 2. Persist them in the settings
let getInstalledEndpointsAndPersistThem = async (findCommands, globalStorageUri: VSCode.Uri.t) => {
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