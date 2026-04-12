module Channel = {
  type t =
    | LatestALS
    | DevALS
    | Hardcoded

  let toString = channel =>
    switch channel {
    | LatestALS => "Latest Agda Language Server"
    | DevALS => "Development Agda Language Server"
    | Hardcoded => "Hardcoded Agda Language Server"
  }
}

module DownloadArtifact = {
  module Platform = {
    type t =
      | Wasm
      | Ubuntu
      | MacOSArm64
      | MacOSX64
      | Windows

    let parse = tag =>
      switch tag {
      | "wasm" => Some(Wasm)
      | "ubuntu" => Some(Ubuntu)
      | "macos-arm64" => Some(MacOSArm64)
      | "macos-x64" => Some(MacOSX64)
      | "windows" => Some(Windows)
      | _ => None
      }

    let toAssetTag = platform =>
      switch platform {
      | Wasm => "wasm"
      | Ubuntu => "ubuntu"
      | MacOSArm64 => "macos-arm64"
      | MacOSX64 => "macos-x64"
      | Windows => "windows"
      }

    let isWasm = platform =>
      switch platform {
      | Wasm => true
      | _ => false
      }

    let matchesDownloadPlatform = (artifactPlatform, downloadPlatform) =>
      switch (artifactPlatform, downloadPlatform) {
      | (Windows, Connection__Download__Platform.Windows) => true
      | (Ubuntu, Connection__Download__Platform.Ubuntu) => true
      | (MacOSArm64, Connection__Download__Platform.MacOS_Arm) => true
      | (MacOSX64, Connection__Download__Platform.MacOS_Intel) => true
      | (Wasm, Connection__Download__Platform.Web) => true
      | _ => false
      }
  }

  type t = {
    releaseTag: string,
    agdaVersion: string,
    platform: Platform.t,
  }

  let extensionMatchesPlatform = (platform, extension) =>
    switch (platform, extension) {
    | (_, None) => true
    | (Platform.Wasm, Some("wasm")) => true
    | (Platform.Wasm, Some(_)) => false
    | (_, Some("zip")) => true
    | (_, Some(_)) => false
    }

  let parseName = (raw: string): option<t> =>
    switch String.match(
      raw,
      %re("/^als-([A-Za-z0-9.]+)-Agda-([0-9]+(?:\.[0-9]+)*)-(wasm|ubuntu|macos-arm64|macos-x64|windows)(?:\.(wasm|zip))?$/"),
    ) {
    | Some([_, Some(releaseTag), Some(agdaVersion), Some(platformTag), extension]) =>
      switch Platform.parse(platformTag) {
      | Some(platform) if extensionMatchesPlatform(platform, extension) =>
        Some({releaseTag, agdaVersion, platform})
      | _ => None
      }
    | _ => None
    }

  let cacheName = artifact =>
    "als-" ++
    artifact.releaseTag ++
    "-Agda-" ++ artifact.agdaVersion ++ "-" ++ Platform.toAssetTag(artifact.platform)

  let directoryParts = artifact => ["releases", artifact.releaseTag, cacheName(artifact)]

  let executableName = artifact => Platform.isWasm(artifact.platform) ? "als.wasm" : "als"

  let versionLabel = releaseTag =>
    releaseTag->String.startsWith("v") ? releaseTag : "v" ++ releaseTag

  let managedExecutableUri = (globalStorageUri: VSCode.Uri.t, artifact: t): VSCode.Uri.t => {
    VSCode.Uri.joinPath(globalStorageUri, Array.concat(directoryParts(artifact), [
      executableName(artifact),
    ]))
  }
}

module Source = {
  type t =
    | FromGitHub(Channel.t, Connection__Download__GitHub.DownloadDescriptor.t)
    | FromURL(Channel.t, string, string) // (channel, url, saveAsFileName)

  let toString = channel =>
    switch channel {
    | FromGitHub(abstractChannel, descriptor) =>
      Channel.toString(abstractChannel) ++
      Connection__Download__GitHub.DownloadDescriptor.toString(descriptor)
    | FromURL(abstractChannel, url, _) =>
      Channel.toString(abstractChannel) ++ " from " ++ url
    }

  let toVersionString = channel =>
    switch channel {
    | FromGitHub(abstractChannel, descriptor) =>
      let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
        switch abstractChannel {
        | Channel.LatestALS =>
          switch DownloadArtifact.parseName(asset.name) {
          | Some(artifact) => artifact.agdaVersion
          | None =>
            asset.name
            ->String.replaceRegExp(%re("/als-Agda-/"), "")
            ->String.replaceRegExp(%re("/-.*/"), "")
          }
        | Channel.DevALS =>
          switch DownloadArtifact.parseName(asset.name) {
          | Some(artifact) => artifact.agdaVersion
          | None =>
            asset.name
            ->String.replaceRegExp(%re("/als-dev-Agda-/"), "")
            ->String.replaceRegExp(%re("/-.*/"), "")
          }
        | Channel.Hardcoded =>
          asset.name
          ->String.replaceRegExp(%re("/als-Agda-/"), "")
          ->String.replaceRegExp(%re("/-.*/"), "")
        }

      let agdaVersion = getAgdaVersion(descriptor.asset)

      switch abstractChannel {
      | Channel.LatestALS =>
        let alsVersion =
          switch DownloadArtifact.parseName(descriptor.asset.name) {
          | Some(artifact) => artifact.releaseTag
          | None =>
            descriptor.release.name
            ->String.split(".")
            ->Array.last
            ->Option.getOr(descriptor.release.name)
          }
        "Agda v" ++ agdaVersion ++ " Language Server " ++ DownloadArtifact.versionLabel(
          alsVersion,
        )
      | Channel.DevALS =>
        "Agda v" ++ agdaVersion ++ " Language Server (dev build)"
      | Channel.Hardcoded =>
        let alsVersion =
          descriptor.release.name
          ->String.split(".")
          ->Array.last
          ->Option.getOr(descriptor.release.name)
        "Agda v" ++ agdaVersion ++ " Language Server " ++ DownloadArtifact.versionLabel(
          alsVersion,
        )
      }
    | FromURL(abstractChannel, url, _) =>
      switch abstractChannel {
      | Channel.Hardcoded =>
        if url->String.endsWith(".wasm") {
          "Agda v" ++ Connection__Hardcoded.wasmAgdaVersion ++ " Language Server (WASM)"
        } else {
          "Agda v" ++ Connection__Hardcoded.agdaVersion ++ " Language Server v" ++ Connection__Hardcoded.alsVersion
        }
      | _ =>
        Channel.toString(abstractChannel)
      }
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

let findReleaseManagedDownloaded = async (
  globalStorageUri: VSCode.Uri.t,
  acceptArtifact,
  toPath,
): option<string> => {
  let releasesUri = VSCode.Uri.joinPath(globalStorageUri, ["releases"])

  let rec findInArtifacts = async (releaseName, artifacts, i) => {
    if i >= Array.length(artifacts) {
      None
    } else {
      let (artifactName, fileType) = Belt.Array.getExn(artifacts, i)
      if fileType != VSCode.FileType.Directory {
        await findInArtifacts(releaseName, artifacts, i + 1)
      } else {
        switch DownloadArtifact.parseName(artifactName) {
        | Some(artifact) if artifact.releaseTag == releaseName && acceptArtifact(artifact) =>
          let executableUri = VSCode.Uri.joinPath(releasesUri, [
            releaseName,
            artifactName,
            DownloadArtifact.executableName(artifact),
          ])
          switch await FS.stat(executableUri) {
          | Ok(_) => Some(toPath(executableUri))
          | Error(_) => await findInArtifacts(releaseName, artifacts, i + 1)
          }
        | _ => await findInArtifacts(releaseName, artifacts, i + 1)
        }
      }
    }
  }

  let rec findInReleases = async (releases, i) => {
    if i >= Array.length(releases) {
      None
    } else {
      let (releaseName, fileType) = Belt.Array.getExn(releases, i)
      if fileType != VSCode.FileType.Directory {
        await findInReleases(releases, i + 1)
      } else {
        let releaseUri = VSCode.Uri.joinPath(releasesUri, [releaseName])
        switch await FS.readDirectory(releaseUri) {
        | Error(_) => await findInReleases(releases, i + 1)
        | Ok(artifacts) =>
          switch await findInArtifacts(releaseName, sortDirectoryEntries(artifacts), 0) {
          | Some(path) => Some(path)
          | None => await findInReleases(releases, i + 1)
          }
        }
      }
    }
  }

  switch await FS.readDirectory(releasesUri) {
  | Error(_) => None
  | Ok(releases) => await findInReleases(sortDirectoryEntries(releases), 0)
  }
}

let findReleaseManagedDownloadedForDesktopPlatform = async (
  globalStorageUri: VSCode.Uri.t,
  platform: Connection__Download__Platform.t,
): option<string> => {
  switch platform {
  | Web =>
    await findReleaseManagedDownloaded(
      globalStorageUri,
      artifact => DownloadArtifact.Platform.isWasm(artifact.platform),
      uri => VSCode.Uri.toString(uri),
    )
  | _ =>
    switch await findReleaseManagedDownloaded(
      globalStorageUri,
      artifact => DownloadArtifact.Platform.matchesDownloadPlatform(artifact.platform, platform),
      uri => VSCode.Uri.fsPath(uri),
    ) {
    | Some(path) => Some(path)
    | None =>
      await findReleaseManagedDownloaded(
        globalStorageUri,
        artifact => DownloadArtifact.Platform.isWasm(artifact.platform),
        uri => VSCode.Uri.toString(uri),
      )
    }
  }
}

let managedDeleteRoots = (globalStorageUri: VSCode.Uri.t): array<VSCode.Uri.t> =>
  [VSCode.Uri.joinPath(globalStorageUri, ["releases"])]

let downloadDestinationForDescriptor = (
  descriptor: Connection__Download__GitHub.DownloadDescriptor.t,
): Connection__Download__GitHub.downloadDestination =>
  switch DownloadArtifact.parseName(descriptor.asset.name) {
  | Some(artifact) => {
      directoryParts: DownloadArtifact.directoryParts(artifact),
      executableName: DownloadArtifact.executableName(artifact),
      isWasm: DownloadArtifact.Platform.isWasm(artifact.platform),
    }
  | None => Connection__Download__GitHub.downloadDestination(descriptor)
  }

let expectedUriForSource = (globalStorageUri: VSCode.Uri.t, source: Source.t): VSCode.Uri.t => {
  let destinationParts = switch source {
  | Source.FromGitHub(_, descriptor) =>
    let destination = downloadDestinationForDescriptor(descriptor)
    Array.concat(destination.directoryParts, [destination.executableName])
  | Source.FromURL(_, url, saveAsFileName) =>
    let executableName = if url->String.endsWith(".wasm") { "als.wasm" } else { "als" }
    [saveAsFileName, executableName]
  }

  VSCode.Uri.joinPath(globalStorageUri, destinationParts)
}

let expectedPathForSource = (globalStorageUri: VSCode.Uri.t, source: Source.t): string =>
  expectedUriForSource(globalStorageUri, source)->uriToPath

// Get release manifest from cache if available, otherwise fetch from GitHub
let getReleaseManifestFromGitHub = async (memento, repo, ~useCache=true) => {
  switch await Connection__Download__GitHub.ReleaseManifest.fetch(memento, repo, ~useCache) {
  | (Error(error), _) => Error(Error.CannotFetchALSReleases(error))
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
        Error(Error.CannotDownloadFromURL(convertedError))
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

// Download the given DownloadDescriptor and return the path of the downloaded file
let download = async (
  globalStorageUri,
  channel,
  ~trace=Connection__Download__Trace.noop,
  ~fetchFile: option<Connection__Download__GitHub.fetchFile>=None,
  ~fetch=?,
) =>
  switch channel {
  | Source.FromGitHub(_, downloadDescriptor) =>
    let reportProgress = await Connection__Download__Util.Progress.report("Agda Language Server") // 📺
    switch await Connection__Download__GitHub.download(
      downloadDescriptor,
      globalStorageUri,
      reportProgress,
      ~trace,
      ~fetchFile,
      ~downloadDestinationForDescriptor=downloadDestinationForDescriptor,
    ) {
    | Error(error) => Error(Error.CannotDownloadALS(error))
    | Ok(_isCached) =>
      // For WASM assets (detected by filename), use als.wasm, otherwise als
      Ok(expectedPathForSource(globalStorageUri, channel))
    }
  | Source.FromURL(_, url, saveAsFileName) =>
    await downloadFromURL(
      globalStorageUri,
      url,
      saveAsFileName,
      "Agda Language Server",
      ~trace,
      ~fetch?,
    )
  }

let alreadyDownloaded = async (globalStorageUri, channel) => {
  switch channel {
  | Channel.LatestALS => {
      let uri = VSCode.Uri.joinPath(globalStorageUri, ["latest-als", "als"])
      switch await FS.stat(uri) {
      | Ok(_) => Some(uri->VSCode.Uri.fsPath)
      | Error(_) => None
      }
    }
  | Channel.DevALS => {
      await findReleaseManagedDownloaded(globalStorageUri, _ => true, uriToPath)
    }
  | Channel.Hardcoded => {
      // Check for native binary first
      let alsUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als"])
      switch await FS.stat(alsUri) {
      | Ok(_) => Some(alsUri->VSCode.Uri.fsPath)
      | Error(_) =>
        // Check for WASM
        let wasmUri = VSCode.Uri.joinPath(globalStorageUri, ["hardcoded-als", "als.wasm"])
        switch await FS.stat(wasmUri) {
        | Ok(_) => Some(VSCode.Uri.toString(wasmUri))
        | Error(_) => None
        }
      }
    }
  }
}
