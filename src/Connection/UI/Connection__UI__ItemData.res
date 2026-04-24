module Candidate = Connection__Candidate
module ResolvedMetadata = Memento.ResolvedMetadata

type t =
  | Candidate(string, string, ResolvedMetadata.entry, bool)
  | DownloadAction(bool, string, Connection__Download__DownloadArtifact.Platform.t)
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
  | DownloadAction(downloaded, versionString, platform) =>
    "DownloadAction: downloaded=" ++
    string_of_bool(downloaded) ++
    ", versionString=" ++
    versionString ++
    ", type=" ++
    (Connection__Download__DownloadArtifact.Platform.isWasm(platform) ? "wasm" : "native")
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
      download.platform,
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
