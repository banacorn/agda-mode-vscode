type t = {
  cleanedDirectories: array<VSCode.Uri.t>,
  failedUris: array<VSCode.Uri.t>,
}

let run = async (
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
  logChannel: Chan.t<Log.t>,
): t => {
  let cleanedDirectories = ref([])
  let failedUris = ref([])
  let deleteRoot = async (uri: VSCode.Uri.t) => {
    Util.log(
      "[ debug ] delete downloads: deleting managed directory",
      VSCode.Uri.toString(uri),
    )
    switch await FS.deleteRecursive(uri) {
    | Ok() =>
      Util.log(
        "[ debug ] delete downloads: deleted managed directory",
        VSCode.Uri.toString(uri),
      )
      cleanedDirectories := Array.concat(cleanedDirectories.contents, [uri])
    | Error(error) =>
      Util.log(
        "[ debug ] delete downloads: deleteRecursive failed",
        VSCode.Uri.toString(uri) ++ ": " ++ error,
      )
      switch await FS.stat(uri) {
      | Ok(_) =>
        Util.log(
          "[ debug ] delete downloads: directory still exists after failed delete",
          VSCode.Uri.toString(uri),
        )
        failedUris := Array.concat(failedUris.contents, [uri])
      | Error(_) =>
        Util.log(
          "[ debug ] delete downloads: directory already missing, treating as cleaned",
          VSCode.Uri.toString(uri),
        )
        cleanedDirectories := Array.concat(cleanedDirectories.contents, [uri])
      }
    }
  }

  let managedRoots = Connection__Download.managedDeleteRoots(globalStorageUri)
  Util.log("[ debug ] delete downloads: globalStorageUri", VSCode.Uri.toString(globalStorageUri))
  Util.log(
    "[ debug ] delete downloads: managed roots",
    managedRoots->Array.map(root => VSCode.Uri.toString(root))->Array.join(" | "),
  )
  let rec deleteAllRoots = async (index: int): unit => {
    if index < Array.length(managedRoots) {
      await deleteRoot(Belt.Array.getExn(managedRoots, index))
      await deleteAllRoots(index + 1)
    }
  }
  await deleteAllRoots(0)

  let inFlightUri = VSCode.Uri.joinPath(globalStorageUri, ["in-flight.download"])
  let inFlightZipUri = VSCode.Uri.joinPath(globalStorageUri, ["in-flight.download.zip"])
  let _ = await FS.delete(inFlightUri)
  let _ = await FS.delete(inFlightZipUri)

  await Memento.ALSReleaseCache.clear(memento, "agda", "agda-language-server")
  await Memento.ALSReleaseCache.clear(memento, "banacorn", "agda-language-server")
  Util.log(
    "[ debug ] delete downloads: cleaned directories",
    cleanedDirectories.contents->Array.map(dir => VSCode.Uri.toString(dir))->Array.join(" | "),
  )
  Util.log(
    "[ debug ] delete downloads: failed uris",
    failedUris.contents->Array.map(uri => VSCode.Uri.toString(uri))->Array.join(", "),
  )
  await Memento.ResolvedMetadata.clearUnderDirectories(memento, cleanedDirectories.contents)

  let currentPaths = Config.Connection.getAgdaPaths()
  Util.log("[ debug ] delete downloads: current connection.paths", currentPaths->Array.join(" | "))
  let filteredPaths = currentPaths->Array.filter(candidate => {
    let matchedDirectory = cleanedDirectories.contents->Array.reduce(None, (found, dirUri) =>
      switch found {
      | Some(_) => found
      | None =>
        if Connection__Candidate.isUnderDirectory(Connection__Candidate.make(candidate), dirUri) {
          Some(dirUri)
        } else {
          None
        }
      }
    )
    switch matchedDirectory {
    | Some(dirUri) =>
      Util.log(
        "[ debug ] delete downloads: removing connection.path candidate",
        candidate ++ " under " ++ VSCode.Uri.toString(dirUri),
      )
      false
    | None =>
      Util.log("[ debug ] delete downloads: preserving connection.path candidate", candidate)
      true
    }
  })
  Util.log(
    "[ debug ] delete downloads: filtered connection.paths",
    filteredPaths->Array.join(" | "),
  )
  await Config.Connection.setAgdaPaths(logChannel, filteredPaths)

  {cleanedDirectories: cleanedDirectories.contents, failedUris: failedUris.contents}
}
