open Connection__Download

module Picker = Connection__UI__Picker

type downloadItems = array<Connection__Download__Availability.availableDownload>

let logDownloadItems = (downloadItems: downloadItems): array<(bool, string, string)> =>
  downloadItems->Array.map(download => {
    let platformTag = DownloadArtifact.Platform.isWasm(download.platform) ? "wasm" : "native"
    (download.downloaded, download.versionString, platformTag)
  })

let handleDownload = async (
  state: State.t,
  platformDeps: Platform.t,
  platform: DownloadArtifact.Platform.t,
  downloaded: bool,
  versionString: string,
  ~channel: Channel.t=DevALS,
  ~refreshUI: option<unit => promise<unit>>=None,
) => {
  state.channels.log->Chan.emit(
    Log.SwitchVersionUI(Log.SwitchVersion.SelectedDownloadAction(downloaded, versionString)),
  )

  if downloaded {
    switch await Connection__Download__ManagedStorage.findCandidateForSelection(
      state.globalStorageUri,
      ~channel,
      ~platform,
      ~versionString,
    ) {
    | None => ()
    | Some(downloadedPath) =>
      await Config.Connection.addAgdaPath(state.channels.log, downloadedPath)
      VSCode.Window.showInformationMessage(
        versionString ++ " is already downloaded",
        [],
      )->Promise.done
    }
  } else {
    module PlatformOps = unpack(platformDeps)
    let onTrace = event => state.channels.log->Chan.emit(Log.DownloadTrace(event))
    let downloadResult = switch await Connection__Download__Flow.sourceForSelection(
      state.memento,
      state.globalStorageUri,
      platformDeps,
      ~channel,
      ~platform,
      ~versionString,
    ) {
    | Error(error) => Error(error)
    | Ok(source) => await PlatformOps.download(state.globalStorageUri, source, ~trace=onTrace)
    }

    switch downloadResult {
    | Error(error) =>
      VSCode.Window.showErrorMessage(
        AgdaModeVscode.Connection__Download.Error.toString(error),
        [],
      )->Promise.done
    | Ok(downloadedPath) =>
      await Config.Connection.addAgdaPath(state.channels.log, downloadedPath)
      switch refreshUI {
      | Some(refreshFn) => await refreshFn()
      | None => ()
      }
      VSCode.Window.showInformationMessage(
        versionString ++ " successfully downloaded",
        [],
      )->Promise.done
    }
  }
  state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted))
}

let handleChannelSwitch = async (
  state: State.t,
  selectedChannel: ref<Channel.t>,
  channel: Channel.t,
  updateUI: downloadItems => promise<unit>,
  ~getDownloadItems: Channel.t => promise<downloadItems>,
) => {
  selectedChannel := channel
  await Memento.SelectedChannel.set(
    state.memento,
    Channel.toString(channel),
  )
  let newDownloadItems = await getDownloadItems(channel)
  await updateUI(newDownloadItems)
}

let onSelection = (
  state: State.t,
  platformDeps: Platform.t,
  selectedChannel: ref<Channel.t>,
  updateUI: downloadItems => promise<unit>,
  view: Picker.t,
  selectedItems: array<Connection__UI__Item.t>,
  ~hasSelectionChanged: string => bool,
  ~switchCandidate: string => promise<unit>,
  ~getDownloadItems: Channel.t => promise<downloadItems>,
  ~showChannelPicker: (
    array<Connection__UI__Channel.pickerItem>,
    string,
  ) => promise<option<string>>=async (items, placeHolder) => {
    let result = await VSCode.Window.showQuickPickWithItems(
      VSCode.PromiseOr.make(Others(items)),
      {placeHolder, canPickMany: false},
      None,
    )
    result->Option.map(item => item.value)
  },
  ~deleteDownloads: unit => promise<Connection__Download__Delete.t>=() =>
    Promise.resolve({
      Connection__Download__Delete.cleanedDirectories: [],
      failedUris: [],
      deletedInFlightFiles: [],
      failedInFlightFiles: [],
    }),
) => {
  let _ = (
    async () => {
      switch selectedItems[0] {
      | Some(selectedItem) =>
        Util.log("[ debug ] user selected item: " ++ selectedItem.label, "")
        switch selectedItem.data {
        | DownloadAction(_, versionString, _)
            when versionString == Connection__UI__Labels.checkingAvailability =>
          ()
        | SelectOtherChannels =>
          let pickerItems = Connection__UI__Channel.pickerItems(~selectedChannel=selectedChannel.contents)
          view.pendingHides = view.pendingHides + 1
          try {
            let channelResult = await showChannelPicker(pickerItems, "Select download channel")
            switch channelResult {
            | Some(value) =>
              switch Channel.fromString(value) {
              | Some(channel) =>
                await handleChannelSwitch(
                  state,
                  selectedChannel,
                  channel,
                  updateUI,
                  ~getDownloadItems,
                )
                view->Picker.show
              | None => view->Picker.show
              }
            | None => view->Picker.show
            }
          } catch {
          | exn =>
            view.pendingHides = 0
            let msg = switch exn {
            | Exn.Error(jsExn) => Exn.message(jsExn)->Option.getOr("unknown error")
            | _ => "unknown error"
            }
            Util.log("[ debug ] channel picker failed", msg)
            view->Picker.show
          }
        | DeleteDownloads =>
          Util.log("[ debug ] user clicked: Delete Downloads", "")
          view->Picker.destroy
          let result = await deleteDownloads()
          state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted))
          if result.failedUris->Array.length == 0 {
            VSCode.Window.showInformationMessage(
              "All downloads and cache deleted",
              [],
            )->Promise.done
          } else {
            VSCode.Window.showWarningMessage(
              "Some downloads could not be deleted: " ++
                result.failedUris->Array.map(uri => VSCode.Uri.toString(uri))->Array.join(", "),
              [],
            )->Promise.done
          }
        | DownloadAction(downloaded, versionString, platform) =>
          Util.log("[ debug ] user clicked: download button = " ++ selectedItem.label, "")
          view->Picker.destroy
          await handleDownload(
            state,
            platformDeps,
            platform,
            downloaded,
            versionString,
            ~channel=selectedChannel.contents,
          )
        | Candidate(selectedPath, _detail, entry, _) =>
          Util.log("[ debug ] user selected kind: " ++ selectedPath, "")
          view->Picker.destroy
          if hasSelectionChanged(selectedPath) {
            state.channels.log->Chan.emit(
              Log.SwitchVersionUI(Log.SwitchVersion.SelectedCandidate(selectedPath, entry, true)),
            )
            await switchCandidate(selectedPath)
            state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted))
          } else {
            state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted))
          }
        | NoInstallations | Separator(_) =>
          state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted))
        }
      | None =>
        view->Picker.destroy
        state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted))
      }
    }
  )()
}

let onHide = (view: Picker.t) => {
  if view.pendingHides > 0 {
    view.pendingHides = view.pendingHides - 1
  } else {
    Util.log("[ debug ] QuickPick hidden/cancelled by user", "")
    view->Picker.destroy
  }
}

let backgroundUpdateFailureFallback = async (
  platformDeps: Platform.t,
  updateUI: downloadItems => promise<unit>,
) => {
  module PlatformOps = unpack(platformDeps)
  let fallback: downloadItems = switch await PlatformOps.determinePlatform() {
  | Error(_) => [{
      downloaded: false,
      versionString: Connection__UI__Labels.downloadUnavailable,
      platform: DownloadArtifact.Platform.Wasm,
    }]
  | Ok(Connection__Download__Platform.Web) => [{
      downloaded: false,
      versionString: Connection__UI__Labels.downloadUnavailable,
      platform: DownloadArtifact.Platform.Wasm,
    }]
  | Ok(downloadPlatform) => [
      {
        downloaded: false,
        versionString: Connection__UI__Labels.downloadUnavailable,
        platform: DownloadArtifact.Platform.fromDownloadPlatform(downloadPlatform),
      },
      {
        downloaded: false,
        versionString: Connection__UI__Labels.downloadUnavailable,
        platform: DownloadArtifact.Platform.Wasm,
      },
    ]
  }
  try {
    await updateUI(fallback)
  } catch {
  | _ => ()
  }
}

let runBackgroundUpdate = async (
  downloadItemsPromise: promise<downloadItems>,
  platformDeps: Platform.t,
  ~probeVersions: unit => promise<bool>,
  updateUI: downloadItems => promise<unit>,
) => {
  try {
    let downloadItems = await downloadItemsPromise
    let phase3Changed = await probeVersions()
    if phase3Changed {
      await updateUI(downloadItems)
    }
    if !phase3Changed {
      await updateUI(downloadItems)
    }
  } catch {
  | _exn => await backgroundUpdateFailureFallback(platformDeps, updateUI)
  }
}

let restoreSelectedChannel = (memento: Memento.t): Channel.t =>
  switch Memento.SelectedChannel.get(memento) {
  | Some(label) => Channel.fromString(label)->Option.getOr(Channel.DevALS)
  | None => Channel.DevALS
  }

let onActivate = async (
  state: State.t,
  platformDeps: Platform.t,
  ~getPlaceholderDownloadItems: unit => promise<downloadItems>,
  ~getDownloadItems: Channel.t => promise<downloadItems>,
  ~getItemData: (downloadItems, ~downloadHeader:string) => promise<array<Connection__UI__ItemData.t>>,
  ~probeVersions: unit => promise<bool>,
  ~hasSelectionChanged: string => bool,
  ~switchCandidate: string => promise<unit>,
  ~deleteDownloads: unit => promise<Connection__Download__Delete.t>,
  ~downloadItemsPromiseOverride: option<promise<downloadItems>>=None,
) => {
  let view = Picker.make(state.channels.log)
  let selectedChannel = ref(restoreSelectedChannel(state.memento))

  let updateUI = async (downloadItems: downloadItems): unit => {
    let downloadHeader =
      "Download (" ++
        Connection__UI__Channel.pickerItem(
          selectedChannel.contents,
          ~selectedChannel=selectedChannel.contents,
        ).label ++ ")"
    let itemData = await getItemData(downloadItems, ~downloadHeader)
    let candidateItemDatas = itemData->Array.filterMap(item =>
      switch item {
      | Candidate(path, _, entry, isSelected) => Some(path, entry.kind, entry.error, isSelected)
      | _ => None
      }
    )
    state.channels.log->Chan.emit(
      Log.SwitchVersionUI(Log.SwitchVersion.UpdatedCandidates(candidateItemDatas)),
    )
    state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.Others(downloadHeader)))
    state.channels.log->Chan.emit(
      Log.SwitchVersionUI(Log.SwitchVersion.UpdatedDownloadItems(logDownloadItems(downloadItems))),
    )

    let items = Connection__UI__Item.fromItemDataArray(itemData, state.extensionUri)
    view->Picker.updateItems(items)
  }

  view->Picker.setPlaceholder("Switch Agda Version")

  if !(Channel.all->Array.includes(selectedChannel.contents)) {
    selectedChannel := Channel.all->Array.get(0)->Option.getOr(Channel.DevALS)
  }

  await updateUI(await getPlaceholderDownloadItems())
  view->Picker.show
  state.channels.log->Chan.emit(Log.SwitchVersionUI(Log.SwitchVersion.Others("QuickPick shown")))

  let downloadItemsPromise = switch downloadItemsPromiseOverride {
  | Some(override) => override
  | None => getDownloadItems(selectedChannel.contents)
  }

  view->Picker.onSelection(selectedItems =>
    onSelection(
      state,
      platformDeps,
      selectedChannel,
      updateUI,
      view,
      selectedItems,
      ~hasSelectionChanged,
      ~switchCandidate,
      ~getDownloadItems,
      ~deleteDownloads,
    )
  )
  view->Picker.onHide(() => onHide(view))

  let _ = runBackgroundUpdate(downloadItemsPromise, platformDeps, ~probeVersions, updateUI)
}
