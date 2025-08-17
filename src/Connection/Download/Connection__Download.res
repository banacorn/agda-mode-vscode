// External binding for URL parsing
@module("url")
external parseUrl: string => {"hostname": string, "pathname": string, "search": option<string>} =
  "parse"

module Error = {
  type t =
    | OptedNotToDownload
    | PlatformNotSupported(Connection__Download__Platform.raw)
    | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
    | CannotDownloadALS(Connection__Download__GitHub.Error.t)
    | CannotFindCompatibleALSRelease
    | CannotDownloadFromURL(Connection__Download__GitHub.Error.t)

  let toString = x =>
    switch x {
    | OptedNotToDownload => "Opted not to download the Agda Language Server"
    | PlatformNotSupported(platform) =>
      "The platform `" ++
      platform["os"] ++
      "/" ++
      platform["dist"] ++ "` is not supported for downloading the Agda Language Server.\n"
    | CannotFetchALSReleases(e) =>
      "Cannot fetch releases of Agda Language Server: " ++
      Connection__Download__GitHub.Error.toString(e)

    | CannotFindCompatibleALSRelease => "Cannot find compatible Agda Language Server release for download. Prebuilts are only available for download on Ubuntu, Windows, and macOS (arm64, x64).\nPlease build from source if you are on a different platform. \nSee https://github.com/agda/agda-language-server for more information."
    | CannotDownloadALS(e) =>
      "Failed to download the Agda Language Server: " ++
      Connection__Download__GitHub.Error.toString(e)
    | CannotDownloadFromURL(e) =>
      "Failed to download from URL: " ++ Connection__Download__GitHub.Error.toString(e)
    }
}

let makeRepo: (Memento.t, VSCode.Uri.t) => Connection__Download__GitHub.Repo.t = (
  memento,
  globalStorageUri,
) => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "agda/agda-mode-vscode",
  memento,
  globalStorageUri,
  cacheInvalidateExpirationSecs: 86400,
}

let getReleaseManifest = async (memento, globalStorageUri) => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetch(
    makeRepo(memento, globalStorageUri),
  ) {
  | (Error(error), _) => Error(Error.CannotFetchALSReleases(error))
  | (Ok(manifest), _) => Ok(manifest)
  }
}

// Download the given FetchSpec and return the path of the downloaded file
let download = async (logChannel, memento, globalStorageUri, fetchSpec) => {
  let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
  switch await Connection__Download__GitHub.download(
    fetchSpec,
    memento,
    globalStorageUri,
    reportProgress,
  ) {
  | Error(error) => Error(Error.CannotDownloadALS(error))
  | Ok(_isCached) =>
    // add the path of the downloaded file to the config
    let destUri = VSCode.Uri.joinPath(globalStorageUri, [fetchSpec.saveAsFileName, "als"])
    let destPath = VSCode.Uri.fsPath(destUri)
    await Config.Connection.addAgdaPath(logChannel, destPath)
    Ok(destPath)
  }
}

// Download directly from a URL without GitHub release metadata and return the path of the downloaded file
let downloadFromURL = async (logChannel, globalStorageUri, url, saveAsFileName, displayName) => {
  let reportProgress = await Connection__Download__Util.Progress.report(displayName)

  // Create directory if it doesn't exist using URI operations
  let destDirUri = VSCode.Uri.joinPath(globalStorageUri, [saveAsFileName])
  switch await FS.stat(destDirUri) {
  | Error(_) =>
    // Directory doesn't exist, create it
    let _ = await FS.createDirectory(destDirUri)
  | Ok(_) => () // Directory already exists
  }

  // Check if already downloaded
  let execPathUri = VSCode.Uri.joinPath(destDirUri, ["als"])
  switch await FS.stat(execPathUri) {
  | Ok(_) => {
      let path = VSCode.Uri.fsPath(execPathUri)
      await Config.Connection.addAgdaPath(logChannel, path)
      Ok(path)
    }
  | Error(_) =>
    // Parse URL and create HTTP options
    try {
      // Parse URL to extract host and path using Node.js url module
      let urlObj = parseUrl(url)
      let host = urlObj["hostname"]
      let path = urlObj["pathname"] ++ urlObj["search"]->Option.getOr("")

      let tempFileUri = VSCode.Uri.joinPath(destDirUri, ["download.tmp"])

      // Download file using existing utilities
      let httpOptions = {
        "host": host,
        "path": path,
        "headers": {
          "User-Agent": "agda/agda-mode-vscode",
        },
      }

      switch await Connection__Download__Util.asFile(httpOptions, tempFileUri, reportProgress) {
      | Error(error) =>
        // Convert Connection__Download__Util.Error.t to Connection__Download__GitHub.Error.t
        let convertedError = switch error {
        | Connection__Download__Util.Error.ServerResponseError(exn) =>
          Connection__Download__GitHub.Error.CannotReadFile(exn)
        | Connection__Download__Util.Error.NoRedirectLocation =>
          Connection__Download__GitHub.Error.CannotReadFile(
            Obj.magic({"message": "No redirect location"}),
          )
        | Connection__Download__Util.Error.Timeout(ms) =>
          Connection__Download__GitHub.Error.CannotReadFile(
            Obj.magic({"message": `Timeout after ${string_of_int(ms)}ms`}),
          )
        | Connection__Download__Util.Error.JsonParseError(raw) =>
          Connection__Download__GitHub.Error.CannotReadFile(
            Obj.magic({"message": `JSON parse error: ${raw}`}),
          )
        | Connection__Download__Util.Error.CannotWriteFile(exn) =>
          Connection__Download__GitHub.Error.CannotReadFile(exn)
        }
        Error(Error.CannotDownloadFromURL(convertedError))
      | Ok() =>
        // Validate that we got a ZIP file, not HTML by checking first 4 bytes
        let readResult = await FS.readFile(tempFileUri)
        let isPKHeader = switch readResult {
        | Error(_) => false
        | Ok(uint8Array) =>
          // Check if file starts with ZIP signature (first 4 bytes should be PK\x03\x04)
          // Check for ZIP signature: bytes 0x50, 0x4B, 0x03, 0x04 (which is "PK\x03\x04")
          if TypedArray.length(uint8Array) >= 4 {
            TypedArray.get(uint8Array, 0)->Option.getOr(0) == 80 &&
            // 0x50
            TypedArray.get(uint8Array, 1)->Option.getOr(0) == 75 &&
            // 0x4B
            TypedArray.get(uint8Array, 2)->Option.getOr(0) == 3 &&
            // 0x03
            TypedArray.get(uint8Array, 3)->Option.getOr(0) == 4 // 0x04
          } else {
            false
          }
        }
        if !isPKHeader {
          // Not a ZIP file signature
          // Not a ZIP file - likely HTML error page
          let _ = await FS.delete(tempFileUri)
          let genericError = Obj.magic({
            "message": "Downloaded file is not a ZIP file. The URL may require authentication or may not be a direct download link.",
          })
          Error(
            Error.CannotDownloadFromURL(
              Connection__Download__GitHub.Error.CannotReadFile(genericError),
            ),
          )
        } else {
          // Proceed with extraction
          let zipFileUri = VSCode.Uri.joinPath(destDirUri, ["download.zip"])
          let _ = await FS.rename(tempFileUri, zipFileUri)

          // Extract ZIP file
          await Connection__Download__GitHub.Unzip.run(zipFileUri, destDirUri)

          // Remove ZIP file after extraction
          let _ = await FS.delete(zipFileUri)

          // Add the path of the downloaded file to the config
          Ok(VSCode.Uri.fsPath(execPathUri))
        }
      }
    } catch {
    | Exn.Error(obj) =>
      Error(Error.CannotDownloadFromURL(Connection__Download__GitHub.Error.CannotReadFile(obj)))
    | _ =>
      let genericError = Obj.magic({"message": "Invalid URL"})
      Error(
        Error.CannotDownloadFromURL(
          Connection__Download__GitHub.Error.CannotReadFile(genericError),
        ),
      )
    }
  }
}
