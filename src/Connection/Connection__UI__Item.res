module ItemData = Connection__UI__ItemData

type t = {
  label: string,
  description?: string,
  detail?: string,
  iconPath?: VSCode.IconPath.t,
  kind?: VSCode.QuickPickItemKind.t,
  data: ItemData.t,
}

let createQuickPickItem = (
  label: string,
  description: option<string>,
  detail: option<string>,
  data: ItemData.t,
): t => {
  switch (description, detail) {
  | (Some(desc), Some(det)) => {label, description: desc, detail: det, data}
  | (Some(desc), None) => {label, description: desc, data}
  | (None, Some(det)) => {label, detail: det, data}
  | (None, None) => {label, data}
  }
}

let fromItemData = (itemData: ItemData.t, extensionUri: VSCode.Uri.t): t => {
  let (label, description, detail): (string, option<string>, option<string>) = {
    switch itemData {
    | Candidate(path, detail, entry, isSelected) => {
        let (label, errorDescription) = ItemData.getCandidateDisplayInfo(path, entry)
        let description = switch (isSelected, errorDescription) {
        | (true, None) => "selected"
        | (false, None) => ""
        | (_, Some(error)) => error
        }
        (label, Some(description), Some(detail))
      }
    | DownloadAction(downloaded, versionString, variant) => {
        let label = switch variant {
        | "native" => ItemData.Constants.downloadNativeALS
        | "wasm" => ItemData.Constants.downloadWasmALS
        | _ => "$(cloud-download)  Download Agda Language Server"
        }
        let description = downloaded ? ItemData.Constants.downloadedAndInstalled : ""
        (label, Some(description), Some(versionString))
      }
    | SelectOtherChannels => (ItemData.Constants.selectOtherChannels, None, None)
    | DeleteDownloads => {
        let label = ItemData.Constants.deleteDownloads
        let description = "Delete all downloaded files and clear cached release metadata"
        (label, Some(description), None)
      }
    | NoInstallations => {
        let label = "$(info) No installations found"
        let description = "Try installing Agda or ALS first"
        let detail = "No executable paths detected"
        (label, Some(description), Some(detail))
      }
    | Separator(label) => (label, None, None)
    }
  }

  switch itemData {
  | Separator(_) => {
      label,
      kind: VSCode.QuickPickItemKind.Separator,
      data: itemData,
    }
  | Candidate(_, _, entry, _) => {
      let baseItem = createQuickPickItem(label, description, detail, itemData)
      if ItemData.shouldCandidateHaveIcon(entry.kind) {
        {
          ...baseItem,
          iconPath: VSCode.IconPath.fromDarkAndLight({
            "dark": VSCode.Uri.joinPath(extensionUri, ["asset/dark.png"]),
            "light": VSCode.Uri.joinPath(extensionUri, ["asset/light.png"]),
          }),
        }
      } else {
        baseItem
      }
    }
  | _ => createQuickPickItem(label, description, detail, itemData)
  }
}

let fromItemDataArray = (itemDataArray: array<ItemData.t>, extensionUri: VSCode.Uri.t): array<t> => {
  itemDataArray->Array.map(itemData => fromItemData(itemData, extensionUri))
}
