type t = {
  cleanedDirectories: array<VSCode.Uri.t>,
  failedUris: array<VSCode.Uri.t>,
  deletedInFlightFiles: array<VSCode.Uri.t>,
  failedInFlightFiles: array<VSCode.Uri.t>,
}

let run = async (globalStorageUri: VSCode.Uri.t): t => {
  let cleanedDirectories = ref([])
  let failedUris = ref([])
  let deletedInFlightFiles = ref([])
  let failedInFlightFiles = ref([])

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

  let deleteFileIfPresent = async (uri: VSCode.Uri.t) => {
    switch await FS.delete(uri) {
    | Ok() =>
      deletedInFlightFiles := Array.concat(deletedInFlightFiles.contents, [uri])
    | Error(error) =>
      Util.log(
        "[ debug ] delete downloads: delete failed",
        VSCode.Uri.toString(uri) ++ ": " ++ error,
      )
      switch await FS.stat(uri) {
      | Ok(_) => failedInFlightFiles := Array.concat(failedInFlightFiles.contents, [uri])
      | Error(_) => ()
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
  await deleteFileIfPresent(inFlightUri)
  await deleteFileIfPresent(inFlightZipUri)

  Util.log(
    "[ debug ] delete downloads: cleaned directories",
    cleanedDirectories.contents->Array.map(dir => VSCode.Uri.toString(dir))->Array.join(" | "),
  )
  Util.log(
    "[ debug ] delete downloads: failed uris",
    failedUris.contents->Array.map(uri => VSCode.Uri.toString(uri))->Array.join(", "),
  )

  {
    cleanedDirectories: cleanedDirectories.contents,
    failedUris: failedUris.contents,
    deletedInFlightFiles: deletedInFlightFiles.contents,
    failedInFlightFiles: failedInFlightFiles.contents,
  }
}
