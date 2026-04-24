module ResolvedMetadata = Memento.ResolvedMetadata
module Candidate = Connection__Candidate

// Download string constants
let agdaVersionPrefix = "Agda "
let alsWithSquirrel = "$(squirrel)  Agda "
let downloadNativeALS = "$(cloud-download)  Download Agda Language Server (native)"
let downloadWasmALS = "$(cloud-download)  Download Agda Language Server (WASM)"
let selectOtherChannels = "$(tag)  Select other channels"
let downloadUnavailable = "Not available for this platform"
let checkingAvailability = "Checking availability..."
let deleteDownloads = "$(trash)  Delete downloads"
let downloadedAndInstalled = "Downloaded and installed"

// Candidate display formatting
let candidateDisplayInfo = (raw: string, entry: ResolvedMetadata.entry): (string, option<string>) => {
  let filename = switch Candidate.make(raw) {
  | Candidate.Command(command) => command
  | Candidate.Resource(uri) => VSCode.Uri.path(uri)->NodeJs.Path.basename
  }
  switch (entry.kind, entry.error) {
  | (Agda(Some(version)), _) => (agdaVersionPrefix ++ version, None)
  | (Agda(None), _) => ("Agda (version unknown)", None)
  | (ALS(Native, Some((alsVersion, agdaVersion, _))), _) =>
    (
      alsVersion == "dev"
        ? alsWithSquirrel ++ agdaVersion ++ " Language Server (dev build)"
        : alsWithSquirrel ++
          agdaVersion ++
          " Language Server " ++ Connection__Download__DownloadArtifact.versionLabel(alsVersion),
      None,
    )
  | (ALS(WASM, Some((alsVersion, agdaVersion, _))), _) =>
    (
      alsVersion == "dev"
        ? alsWithSquirrel ++ agdaVersion ++ " Language Server (dev build) WASM"
        : alsWithSquirrel ++
          agdaVersion ++
          " Language Server " ++
          Connection__Download__DownloadArtifact.versionLabel(alsVersion) ++ " WASM",
      None,
    )
  | (ALS(Native, None), _) => (alsWithSquirrel ++ "Language Server (version unknown)", None)
  | (ALS(WASM, None), _) => (alsWithSquirrel ++ "Language Server (version unknown) WASM", None)
  | (Unknown, Some(error)) => ("$(error) " ++ filename, Some("Error: " ++ error))
  | (Unknown, None) => ("$(question) " ++ filename, Some("Unknown executable"))
  }
}

// Channel display text
let channelLabel = (channel: Connection__Download__Channel.t): string =>
  switch channel {
  | Connection__Download__Channel.LatestALS => "Latest"
  | Connection__Download__Channel.DevALS => "Development"
  }

let channelDetail = (channel: Connection__Download__Channel.t): string =>
  switch channel {
  | Connection__Download__Channel.LatestALS => "Tracks the latest stable release"
  | Connection__Download__Channel.DevALS => "Tracks the latest commit of the master branch"
  }

// Download section header
let downloadHeader = (channel: Connection__Download__Channel.t): string =>
  "Download (" ++ channelLabel(channel) ++ ")"

// Item-level labels
let candidateSelected = "selected"
let downloadFallbackLabel = "$(cloud-download)  Download Agda Language Server"
let deleteDownloadsDescription = "Delete all downloaded files and clear cached release metadata"
let noInstallationsLabel = "$(info) No installations found"
let noInstallationsDescription = "Try installing Agda or ALS first"
let noInstallationsDetail = "No executable paths detected"
