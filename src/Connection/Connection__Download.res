// External binding for URL parsing
@module("url") external parseUrl: string => {"hostname": string, "pathname": string, "search": option<string>} = "parse"

module Error = {
  type t =
    | CannotFetchALSReleases(Connection__Download__GitHub.Error.t)
    | CannotDownloadALS(Connection__Download__GitHub.Error.t)
    | CannotConnectToALS(Connection__Target.Error.t)
    | CannotFindCompatibleALSRelease
    | CannotDownloadFromURL(Connection__Download__GitHub.Error.t)

  let toString = x =>
    switch x {
    | CannotFetchALSReleases(e) =>
      "Cannot fetch releases of Agda Language Server: " ++
      Connection__Download__GitHub.Error.toString(e)

    | CannotFindCompatibleALSRelease => "Cannot find compatible Agda Language Server release for download. Prebuilts are only available for download on Ubuntu, Windows, and macOS (arm64, x64).\nPlease build from source if you are on a different platform. \nSee https://github.com/agda/agda-language-server for more information."
    | CannotConnectToALS(e) => Connection__Target.Error.toString(e)
    | CannotDownloadALS(e) =>
      "Failed to download the Agda Language Server: " ++ Connection__Download__GitHub.Error.toString(e)
    | CannotDownloadFromURL(e) =>
      "Failed to download from URL: " ++ Connection__Download__GitHub.Error.toString(e)
    }
}

let makeRepo: (State__Memento.t, VSCode.Uri.t) => Connection__Download__GitHub.Repo.t = (
  memento,
  globalStorageUri,
) => {
  username: "agda",
  repository: "agda-language-server",
  userAgent: "agda/agda-mode-vscode",
  memento,
  globalStoragePath: VSCode.Uri.fsPath(globalStorageUri),
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
let download = async (memento, globalStorageUri, fetchSpec) => {
  let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
  let globalStoragePath = VSCode.Uri.fsPath(globalStorageUri)
  switch await Connection__Download__GitHub.download(
    fetchSpec,
    memento,
    globalStoragePath,
    reportProgress,
  ) {
  | Error(error) => Error(Error.CannotDownloadALS(error))
  | Ok(_isCached) =>
    // add the path of the downloaded file to the config
    let destPath = Connection__URI.parse(
      NodeJs.Path.join([globalStoragePath, fetchSpec.saveAsFileName, "als"]),
    )
    await Config.Connection.addAgdaPath(destPath)
    switch await Connection__Target.fromURI(destPath) {
    | Error(e) => Error(Error.CannotConnectToALS(e))
    | Ok(target) => Ok(target)
    }
  }
}

// Download directly from a URL without GitHub release metadata
let downloadFromURL = async (globalStorageUri, url, saveAsFileName, displayName) => {
  let reportProgress = await Connection__Download__Util.Progress.report(displayName)
  let globalStoragePath = VSCode.Uri.fsPath(globalStorageUri)
  
  // Create directory if it doesn't exist
  let destDir = NodeJs.Path.join([globalStoragePath, saveAsFileName])
  if !NodeJs.Fs.existsSync(destDir) {
    await NodeJs.Fs.mkdir(destDir, {recursive: true, mode: 0o777})
  }
  
  // Check if already downloaded
  let execPath = NodeJs.Path.join([destDir, "als"])
  if NodeJs.Fs.existsSync(execPath) {
    let destPath = Connection__URI.parse(execPath)
    await Config.Connection.addAgdaPath(destPath)
    switch await Connection__Target.fromURI(destPath) {
    | Error(e) => Error(Error.CannotConnectToALS(e))
    | Ok(target) => Ok(target)
    }
  } else {
    // Parse URL and create HTTP options 
    try {
      // Parse URL to extract host and path using Node.js url module
      let urlObj = parseUrl(url)
      let host = urlObj["hostname"]
      let path = urlObj["pathname"] ++ (urlObj["search"]->Option.getOr(""))
      
      let tempFilePath = NodeJs.Path.join([destDir, "download.tmp"])
      
      // Download file using existing utilities
      let httpOptions = {
        "host": host,
        "path": path,
        "headers": {
          "User-Agent": "agda/agda-mode-vscode",
        },
      }
      
      switch await Connection__Download__Util.asFile(httpOptions, tempFilePath, reportProgress) {
      | Error(error) => 
        // Convert Connection__Download__Util.Error.t to Connection__Download__GitHub.Error.t
        let convertedError = switch error {
        | Connection__Download__Util.Error.ServerResponseError(exn) => Connection__Download__GitHub.Error.CannotReadFile(exn)
        | Connection__Download__Util.Error.NoRedirectLocation => Connection__Download__GitHub.Error.CannotReadFile(Obj.magic({"message": "No redirect location"}))
        | Connection__Download__Util.Error.Timeout(ms) => Connection__Download__GitHub.Error.CannotReadFile(Obj.magic({"message": `Timeout after ${string_of_int(ms)}ms`}))
        | Connection__Download__Util.Error.JsonParseError(raw) => Connection__Download__GitHub.Error.CannotReadFile(Obj.magic({"message": `JSON parse error: ${raw}`}))
        | Connection__Download__Util.Error.CannotWriteFile(exn) => Connection__Download__GitHub.Error.CannotReadFile(exn)
        }
        Error(Error.CannotDownloadFromURL(convertedError))
      | Ok() =>
        // Validate that we got a ZIP file, not HTML by checking first 4 bytes
        let fileHandle = await NodeJs.Fs.open_(tempFilePath, NodeJs.Fs.Flag.read)
        let buffer = await NodeJs.Fs.FileHandle.readFile(fileHandle)
        await NodeJs.Fs.FileHandle.close(fileHandle)
        
        // Check if file starts with ZIP signature (first 4 bytes should be PK\x03\x04)
        // Check for ZIP signature: bytes 0x50, 0x4B, 0x03, 0x04 (which is "PK\x03\x04")
        let isPKHeader = if NodeJs.Buffer.length(buffer) >= 4 {
          NodeJs.Buffer.readUint8(buffer, ~offset=0) == 80. && // 0x50
          NodeJs.Buffer.readUint8(buffer, ~offset=1) == 75. && // 0x4B
          NodeJs.Buffer.readUint8(buffer, ~offset=2) == 3. &&  // 0x03
          NodeJs.Buffer.readUint8(buffer, ~offset=3) == 4.     // 0x04
        } else {
          false
        }
        if !isPKHeader { // Not a ZIP file signature
          // Not a ZIP file - likely HTML error page
          let _ = await Connection__Download__GitHub.Nd.Fs.unlink(tempFilePath)
          let genericError = Obj.magic({"message": "Downloaded file is not a ZIP file. The URL may require authentication or may not be a direct download link."})
          Error(Error.CannotDownloadFromURL(Connection__Download__GitHub.Error.CannotReadFile(genericError)))
        } else {
          // Proceed with extraction
          let zipFilePath = NodeJs.Path.join([destDir, "download.zip"])
          let _ = await Connection__Download__GitHub.Nd.Fs.rename(tempFilePath, zipFilePath)
          
          // Extract ZIP file
          await Connection__Download__GitHub.Unzip.run(zipFilePath, destDir)
          
          // Remove ZIP file after extraction
          let _ = await Connection__Download__GitHub.Nd.Fs.unlink(zipFilePath)
          
          // Add the path of the downloaded file to the config
          let destPath = Connection__URI.parse(execPath)
          await Config.Connection.addAgdaPath(destPath)
          switch await Connection__Target.fromURI(destPath) {
          | Error(e) => Error(Error.CannotConnectToALS(e))
          | Ok(target) => Ok(target)
          }
        }
      }
    } catch {
    | Exn.Error(obj) => Error(Error.CannotDownloadFromURL(Connection__Download__GitHub.Error.CannotReadFile(obj)))
    | _ => 
      let genericError = Obj.magic({"message": "Invalid URL"})
      Error(Error.CannotDownloadFromURL(Connection__Download__GitHub.Error.CannotReadFile(genericError)))
    }
  }
}
