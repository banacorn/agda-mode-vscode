module Candidate = Connection__Candidate
module ResolvedMetadata = Memento.ResolvedMetadata

module Constants = {
  let agdaVersionPrefix = "Agda "
  let alsWithSquirrel = "$(squirrel)  Agda "
  let downloadNativeALS = "$(cloud-download)  Download Agda Language Server (native)"
  let downloadWasmALS = "$(cloud-download)  Download Agda Language Server (WASM)"
  let selectOtherChannels = "$(tag)  Select other channels"
  let downloadUnavailable = "Not available for this platform"
  let checkingAvailability = "Checking availability..."
  let deleteDownloads = "$(trash)  Delete downloads"
  let downloadedAndInstalled = "Downloaded and installed"
}

type t =
  | Candidate(string, string, ResolvedMetadata.entry, bool)
  | DownloadAction(bool, string, string)
  | SelectOtherChannels
  | DeleteDownloads
  | NoInstallations
  | Separator(string)

let toString = item =>
  switch item {
  | Candidate(path, detail, entry, isSelected) =>
    "Candidate: " ++
    path ++
    " -> " ++
    detail ++
    ", " ++
    ResolvedMetadata.kindToString(entry.kind) ++ if isSelected {
      ", selected"
    } else {
      ""
    }
  | DownloadAction(downloaded, versionString, downloadType) =>
    "DownloadAction: downloaded=" ++
    string_of_bool(downloaded) ++
    ", versionString=" ++
    versionString ++
    ", type=" ++
    downloadType
  | SelectOtherChannels => "SelectOtherChannels"
  | DeleteDownloads => "DeleteDownloads"
  | NoInstallations => "NoInstallations"
  | Separator(label) => "Separator: " ++ label
  }

let shouldCandidateHaveIcon = (kind: ResolvedMetadata.kind): bool => {
  switch kind {
  | Agda(_) => true
  | _ => false
  }
}

let variantToTag = (variant: Connection__Download.SelectionVariant.t): string =>
  switch variant {
  | Native => "native"
  | WASM => "wasm"
  }

let getCandidateDisplayInfo = (raw: string, entry: ResolvedMetadata.entry): (
  string,
  option<string>,
) => {
  let filename = switch Candidate.make(raw) {
  | Candidate.Command(command) => command
  | Candidate.Resource(uri) => VSCode.Uri.path(uri)->NodeJs.Path.basename
  }
  switch (entry.kind, entry.error) {
  | (Agda(Some(version)), _) => (Constants.agdaVersionPrefix ++ version, None)
  | (Agda(None), _) => ("Agda (version unknown)", None)
  | (ALS(Native, Some((alsVersion, agdaVersion, _))), _) =>
    (
      alsVersion == "dev"
        ? Constants.alsWithSquirrel ++ agdaVersion ++ " Language Server (dev build)"
        : Constants.alsWithSquirrel ++
          agdaVersion ++
          " Language Server " ++ Connection__Download.DownloadArtifact.versionLabel(alsVersion),
      None,
    )
  | (ALS(WASM, Some((alsVersion, agdaVersion, _))), _) =>
    (
      alsVersion == "dev"
        ? Constants.alsWithSquirrel ++ agdaVersion ++ " Language Server (dev build) WASM"
        : Constants.alsWithSquirrel ++
          agdaVersion ++
          " Language Server " ++
          Connection__Download.DownloadArtifact.versionLabel(alsVersion) ++ " WASM",
      None,
    )
  | (ALS(Native, None), _) => ("$(squirrel)  Agda Language Server (version unknown)", None)
  | (ALS(WASM, None), _) => ("$(squirrel)  Agda Language Server (version unknown) WASM", None)
  | (Unknown, Some(error)) => ("$(error) " ++ filename, Some("Error: " ++ error))
  | (Unknown, None) => ("$(question) " ++ filename, Some("Unknown executable"))
  }
}

let entriesToItemData = (
  entries: array<(string, string, ResolvedMetadata.entry)>,
  pickedPath: option<string>,
  downloadItems: array<Connection__Download__Availability.availableDownload>,
  ~downloadHeader: string="Download (Development)",
): array<t> => {
  let hasCandidates = Array.length(entries) > 0

  let candidateItems = switch pickedPath {
  | Some(picked) =>
    let pickedCandidate = Candidate.make(picked)
    let matched = ref(false)
    entries->Array.map(((path, detail, entry)) => {
      let isSelected =
        !matched.contents && Candidate.equal(pickedCandidate, Candidate.make(path))
      if isSelected {
        matched := true
      }
      Candidate(path, detail, entry, isSelected)
    })
  | None =>
    entries->Array.map(((path, detail, entry)) => Candidate(path, detail, entry, false))
  }

  let candidateSection = if hasCandidates {
    Array.concat([Separator("Candidates")], candidateItems)
  } else {
    []
  }

  let downloadSectionItems = downloadItems->Array.map(download =>
    DownloadAction(
      download.downloaded,
      download.versionString,
      variantToTag(download.variant),
    )
  )

  let channelItems = [SelectOtherChannels]

  let downloadSection =
    Array.concat(
      [Separator(downloadHeader)],
      Array.concat(downloadSectionItems, Array.concat(channelItems, [DeleteDownloads])),
    )

  Array.concat(candidateSection, downloadSection)
}
