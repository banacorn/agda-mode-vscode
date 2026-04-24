// On desktop (file:// scheme), return fsPath; on web (vscode-userdata: etc.), return URI string
let uriToPath = (uri: VSCode.Uri.t): string =>
  if VSCode.Uri.scheme(uri) == "file" {
    VSCode.Uri.fsPath(uri)
  } else {
    VSCode.Uri.toString(uri)
  }

let sortDirectoryEntries = entries =>
  entries->Array.toSorted(((a, _), (b, _)) =>
    if a > b {
      -1.
    } else if a < b {
      1.
    } else {
      0.
    }
  )

let managedDeleteRoots = (globalStorageUri: VSCode.Uri.t): array<VSCode.Uri.t> =>
  [VSCode.Uri.joinPath(globalStorageUri, ["releases"])]

let downloadDestinationForDescriptor = (
  descriptor: Connection__Download__GitHub.DownloadDescriptor.t,
): Connection__Download__GitHub.downloadDestination =>
  switch Connection__Download__DownloadArtifact.parseName(descriptor.asset.name) {
  | Some(artifact) => {
      directoryParts: Connection__Download__DownloadArtifact.directoryParts(artifact),
      executableName: Connection__Download__DownloadArtifact.executableName(artifact),
      isWasm: Connection__Download__DownloadArtifact.Platform.isWasm(artifact.platform),
    }
  | None => Connection__Download__GitHub.downloadDestination(descriptor)
  }

let expectedUriForSource = (
  globalStorageUri: VSCode.Uri.t,
  source: Connection__Download__Source.t,
): VSCode.Uri.t => {
  let destinationParts = switch source {
  | Connection__Download__Source.FromGitHub(_, descriptor) =>
    let destination = downloadDestinationForDescriptor(descriptor)
    Array.concat(destination.directoryParts, [destination.executableName])
  | Connection__Download__Source.FromURL(_, url, saveAsFileName) =>
    let executableName = if url->String.endsWith(".wasm") { "als.wasm" } else { "als" }
    [saveAsFileName, executableName]
  }

  VSCode.Uri.joinPath(globalStorageUri, destinationParts)
}

let expectedPathForSource = (
  globalStorageUri: VSCode.Uri.t,
  source: Connection__Download__Source.t,
): string =>
  expectedUriForSource(globalStorageUri, source)->uriToPath

// Get release manifest from cache if available, otherwise fetch from GitHub
let getReleaseManifestFromGitHub = async (memento, repo, ~useCache=true) => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetch(memento, repo, ~useCache) {
  | (Error(error), _) => Error(Connection__Download__Error.CannotFetchALSReleases(error))
  | (Ok(manifest), _) => Ok(manifest)
  }
}

// Download directly from a URL without GitHub release metadata and return the path of the downloaded file
let downloadFromURL = async (globalStorageUri, url, saveAsFileName, displayName, ~trace=Connection__Download__Trace.noop, ~fetch=?) => {
  let reportProgress = await Connection__Download__Util.Progress.report(displayName)

  // Create directory if it doesn't exist using URI operations
  let destDirUri = VSCode.Uri.joinPath(globalStorageUri, [saveAsFileName])
  switch await FS.stat(destDirUri) {
  | Error(_) =>
    // Directory doesn't exist, create it
    let _ = await FS.createDirectory(destDirUri)
  | Ok(_) => () // Directory already exists
  }

  // Determine if this is a WASM file by checking the URL
  let isWasm = url->String.endsWith(".wasm")
  let execFileName = if isWasm { "als.wasm" } else { "als" }

  // Check if already downloaded
  let execPathUri = VSCode.Uri.joinPath(destDirUri, [execFileName])
  switch await FS.stat(execPathUri) {
  | Ok(_) => {
      Util.log("[ debug ] downloadFromURL: already cached, returning without chmod", uriToPath(execPathUri))
      Ok(uriToPath(execPathUri))
    }
  | Error(_) =>
    Util.log("[ debug ] downloadFromURL: not cached, downloading from", url)
    // Parse URL and create HTTP options
    try {
      // Parse URL using global WHATWG URL to support both Node and web
      let urlObj = URL.make(url)
      let host = urlObj->URL.host
      let path = urlObj->URL.pathname ++ urlObj->URL.search

      let tempFileUri = VSCode.Uri.joinPath(destDirUri, ["download.tmp"])

      // Download file using existing utilities
      let httpOptions = {
        "host": host,
        "path": path,
        "headers": {
          "User-Agent": "agda/agda-mode-vscode",
          "Accept": "*/*",
        },
      }

      switch await Connection__Download__Util.asFile(httpOptions, tempFileUri, reportProgress, ~trace, ~fetch?) {
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
        Error(Connection__Download__Error.CannotDownloadFromURL(convertedError))
      | Ok() =>
        // For WASM, skip ZIP validation and extraction - it's a raw binary
        if isWasm {
          // WASM file - save directly as als.wasm
          let _ = await FS.rename(tempFileUri, execPathUri)
          Ok(uriToPath(execPathUri))
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
              Connection__Download__Error.CannotDownloadFromURL(
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

            // chmod the executable after extraction (Unix only)
            if OS.onUnix {
              let _ = await Connection__Download__GitHub.chmodExecutable(VSCode.Uri.fsPath(execPathUri))
            }

            // Add the path of the downloaded file to the config
            Ok(VSCode.Uri.fsPath(execPathUri))
          }
        }
      }
    } catch {
    | Exn.Error(obj) =>
      Error(
        Connection__Download__Error.CannotDownloadFromURL(
          Connection__Download__GitHub.Error.CannotReadFile(obj),
        ),
      )
    | _ =>
      let genericError = Obj.magic({"message": "Invalid URL"})
      Error(
        Connection__Download__Error.CannotDownloadFromURL(
          Connection__Download__GitHub.Error.CannotReadFile(genericError),
        ),
      )
    }
  }
}

// Download the given DownloadDescriptor and return the path of the downloaded file
let download = async (
  globalStorageUri,
  channel,
  ~trace=Connection__Download__Trace.noop,
  ~fetchFile: option<Connection__Download__GitHub.fetchFile>=None,
  ~fetch=?,
) =>
  switch channel {
  | Connection__Download__Source.FromGitHub(_, downloadDescriptor) =>
    let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
    switch await Connection__Download__GitHub.download(
      downloadDescriptor,
      globalStorageUri,
      reportProgress,
      ~trace,
      ~fetchFile,
      ~downloadDestinationForDescriptor=downloadDestinationForDescriptor,
    ) {
    | Error(error) => Error(Connection__Download__Error.CannotDownloadALS(error))
    | Ok(_isCached) =>
      // For WASM assets (detected by filename), use als.wasm, otherwise als
      Ok(expectedPathForSource(globalStorageUri, channel))
    }
  | Connection__Download__Source.FromURL(_, url, saveAsFileName) =>
    await downloadFromURL(
      globalStorageUri,
      url,
      saveAsFileName,
      "Agda Language Server",
      ~trace,
      ~fetch?,
    )
  }
