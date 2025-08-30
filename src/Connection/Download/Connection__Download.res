// External binding for URL parsing
@module("url")
external parseUrl: string => {"hostname": string, "pathname": string, "search": option<string>} =
  "parse"

module DownloadOrderAbstract = {
  type t =
    | LatestALS
    | DevALS
    | DevWASMALS

  let toString = order =>
    switch order {
    | LatestALS => "Latest Agda Language Server"
    | DevALS => "Development Agda Language Server"
    | DevWASMALS => "Development Agda Language Server (WASM)"
    }
}

module DownloadOrderConcrete = {
  type t = FromGitHub(DownloadOrderAbstract.t, Connection__Download__GitHub.DownloadDescriptor.t)

  let toString = order =>
    switch order {
    | FromGitHub(abstractOrder, descriptor) =>
      DownloadOrderAbstract.toString(abstractOrder) ++
      Connection__Download__GitHub.DownloadDescriptor.toString(descriptor)
    }
}

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

// Get release manifest from cache if available, otherwise fetch from GitHub
let getReleaseManifestFromGitHub = async (memento, repo, ~useCache=true) => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetch(memento, repo, ~useCache) {
  | (Error(error), _) => Error(Error.CannotFetchALSReleases(error))
  | (Ok(manifest), _) => Ok(manifest)
  }
}

// Download the given DownloadDescriptor and return the path of the downloaded file
let download = async (globalStorageUri, order) =>
  switch order {
  | DownloadOrderConcrete.FromGitHub(_, downloadDescriptor) =>
    let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
    switch await Connection__Download__GitHub.download(
      downloadDescriptor,
      globalStorageUri,
      reportProgress,
    ) {
    | Error(error) => Error(Error.CannotDownloadALS(error))
    | Ok(_isCached) =>
      // For WASM, the extracted file should be als.wasm instead of als
      let fileName = if downloadDescriptor.saveAsFileName == "dev-wasm-als" {
        "als.wasm"
      } else {
        "als"
      }
      let destUri = VSCode.Uri.joinPath(
        globalStorageUri,
        [downloadDescriptor.saveAsFileName, fileName],
      )
      let destPath = VSCode.Uri.fsPath(destUri)
      Ok(destPath)
    }
  }

// Download directly from a URL without GitHub release metadata and return the path of the downloaded file
let downloadFromURL = async (globalStorageUri, url, saveAsFileName, displayName) => {
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
        // For WASM, skip ZIP validation and extraction - it's a raw binary
        if saveAsFileName == "dev-wasm-als" {
          // WASM file - save directly as als.wasm
          let wasmExecUri = VSCode.Uri.joinPath(destDirUri, ["als.wasm"])
          let _ = await FS.rename(tempFileUri, wasmExecUri)
          Ok(VSCode.Uri.fsPath(wasmExecUri))
        } else {
          // Regular ZIP file processing
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

// Check if something is already downloaded
let alreadyDownloaded = async (globalStorageUri, order) => {
  let paths = switch order {
  | DownloadOrderAbstract.LatestALS => ["latest-als", "als"]
  | DownloadOrderAbstract.DevALS => ["dev-als", "als"]
  | DownloadOrderAbstract.DevWASMALS => ["dev-wasm-als", "als.wasm"]
  }
  let uri = VSCode.Uri.joinPath(globalStorageUri, paths)
  switch await FS.stat(uri) {
  | Ok(_) => Some(uri->VSCode.Uri.fsPath)
  | Error(_) => None
  }
}
