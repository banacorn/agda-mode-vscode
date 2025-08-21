// Constants for reused UI strings
module Constants = {
  let agdaVersionPrefix = "Agda v"
  let alsWithSquirrel = "$(squirrel)  ALS v"
  let downloadLatestALS = "$(cloud-download)  Download the latest Agda Language Server"
  let openDownloadFolder = "$(folder-opened)  Open download folder"
  let downloadedAndInstalled = "Downloaded and installed"
}

module ItemData = {
  // UI item types
  type t =
    | Endpoint(string, Memento.Endpoints.entry, bool) // path, entry, is selected
    | DownloadAction(bool, string) // downloaded, versionString
    | OpenFolder(string) // folderPath
    | NoInstallations
    | Separator(string) // label

  let toString = item =>
    switch item {
    | Endpoint(path, entry, isSelected) =>
      "Endpoint: " ++
      path ++
      ", " ++
      Memento.Endpoints.endpointToString(entry.endpoint) ++ if isSelected {
        ", selected"
      } else {
        ""
      }
    | DownloadAction(downloaded, versionString) =>
      "DownloadAction: downloaded=" ++
      string_of_bool(downloaded) ++
      ", versionString=" ++
      versionString
    | OpenFolder(folderPath) => "OpenFolder: " ++ folderPath
    | NoInstallations => "NoInstallations"
    | Separator(label) => "Separator: " ++ label
    }

  // UI display logic
  let shouldEndpointHaveIcon = (endpoint: Memento.Endpoints.endpoint): bool => {
    switch endpoint {
    | Agda(_) => true
    | _ => false
    }
  }

  let getEndpointDisplayInfo = (filename: string, entry: Memento.Endpoints.entry): (
    string,
    option<string>,
  ) => {
    switch (entry.endpoint, entry.error) {
    | (Agda(Some(version)), _) => (Constants.agdaVersionPrefix ++ version, None)
    | (Agda(None), _) => ("Agda (version unknown)", None)
    | (ALS(Some((alsVersion, agdaVersion, _))), _) => (
        Constants.alsWithSquirrel ++
        alsVersion ++
        ", " ++
        Constants.agdaVersionPrefix ++
        agdaVersion,
        None,
      )
    | (ALS(None), _) => (Constants.alsWithSquirrel ++ "(version unknown)", None)
    | (Unknown, Some(error)) => ("$(error) " ++ filename, Some("Error: " ++ error))
    | (Unknown, None) => ("$(question) " ++ filename, Some("Unknown executable"))
    }
  }

  // Convert entries to item data
  let entriesToItemData = (
    entries: Dict.t<Memento.Endpoints.entry>,
    pickedPath: option<string>,
    downloadItem: option<(bool, string)>,
    folderPath: string,
  ): array<t> => {
    // Pre-convert to array once for better performance
    let entriesArray = entries->Dict.toArray
    let hasEndpoints = Array.length(entriesArray) > 0

    // Build endpoint items with optimized selection check
    let endpointItems = switch pickedPath {
    | Some(picked) =>
      // Only do string comparison when there's a picked path
      entriesArray->Array.map(((path, entry)) => Endpoint(path, entry, picked == path))
    | None =>
      // Avoid unnecessary comparisons when no path is picked
      entriesArray->Array.map(((path, entry)) => Endpoint(path, entry, false))
    }

    // Build sections efficiently without Array.flat
    // Add installed section
    let sectionsWithInstalled = if hasEndpoints {
      Array.concat([Separator("Installed")], endpointItems)
    } else {
      [NoInstallations]
    }

    // Add download section if present
    let sectionsWithDownload = switch downloadItem {
    | Some((downloaded, versionString)) =>
      Array.concat(
        sectionsWithInstalled,
        [Separator("Download"), DownloadAction(downloaded, versionString)],
      )
    | None => sectionsWithInstalled
    }

    // Add misc section
    Array.concat(sectionsWithDownload, [Separator("Misc"), OpenFolder(folderPath)])
  }
}

module Item = {
  // Helper function to create QuickPickItem from label, description, and detail
  let createQuickPickItem = (
    label: string,
    description: option<string>,
    detail: option<string>,
  ): VSCode.QuickPickItem.t => {
    switch (description, detail) {
    | (Some(desc), Some(det)) => {label, description: desc, detail: det}
    | (Some(desc), None) => {label, description: desc}
    | (None, Some(det)) => {label, detail: det}
    | (None, None) => {label: label}
    }
  }

  // Convert item data to VSCode QuickPick item
  let fromItemData = (itemData: ItemData.t, extensionUri: VSCode.Uri.t): VSCode.QuickPickItem.t => {
    // Convert item data to UI display information
    let (label, description, detail): (string, option<string>, option<string>) = {
      switch itemData {
      | Endpoint(path, entry, isSelected) => {
          let filename = NodeJs.Path.basename(path)
          let (label, errorDescription) = ItemData.getEndpointDisplayInfo(filename, entry)
          let description = switch (isSelected, errorDescription) {
          | (true, None) => "Selected"
          | (false, None) => ""
          | (_, Some(error)) => error
          }
          (label, Some(description), Some(path))
        }
      | DownloadAction(downloaded, versionString) => {
          let label = Constants.downloadLatestALS
          let description = downloaded ? Constants.downloadedAndInstalled : ""
          (label, Some(description), Some(versionString))
        }
      | OpenFolder(folderPath) => {
          let label = Constants.openDownloadFolder
          let description = "Where the language servers are downloaded to"
          (label, Some(description), Some(folderPath))
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
      }
    | Endpoint(_, entry, _) => {
        let baseItem = createQuickPickItem(label, description, detail)
        if ItemData.shouldEndpointHaveIcon(entry.endpoint) {
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
    | _ => createQuickPickItem(label, description, detail)
    }
  }

  // Convert array of item data to VSCode QuickPick items
  let fromItemDataArray = (itemDataArray: array<ItemData.t>, extensionUri: VSCode.Uri.t): array<
    VSCode.QuickPickItem.t,
  > => {
    itemDataArray->Array.map(itemData => fromItemData(itemData, extensionUri))
  }
}

module View = {
  type t = {
    log: Chan.t<Log.t>,
    quickPick: VSCode.QuickPick.t<VSCode.QuickPickItem.t>,
    subscriptions: array<VSCode.Disposable.t>,
    mutable items: array<VSCode.QuickPickItem.t>,
  }

  let make = (log): t => {
    log,
    quickPick: VSCode.Window.createQuickPick(),
    subscriptions: [],
    items: [],
  }

  let setPlaceholder = (self: t, placeholder: string): unit => {
    self.quickPick->VSCode.QuickPick.setPlaceholder(placeholder)
  }

  let updateItems = (self: t, items: array<VSCode.QuickPickItem.t>): unit => {
    self.items = items
    self.quickPick->VSCode.QuickPick.setItems(items)
  }

  let show = (self: t): unit => {
    self.quickPick->VSCode.QuickPick.show
  }

  let onSelection = (self: t, handler: array<VSCode.QuickPickItem.t> => unit): unit => {
    self.quickPick
    ->VSCode.QuickPick.onDidChangeSelection(handler)
    ->Util.Disposable.add(self.subscriptions)
  }

  let onHide = (self: t, handler: unit => unit): unit => {
    self.quickPick
    ->VSCode.QuickPick.onDidHide(handler)
    ->Util.Disposable.add(self.subscriptions)
  }

  let destroy = (self: t): unit => {
    self.quickPick->VSCode.QuickPick.dispose
    self.subscriptions->Array.forEach(sub => sub->VSCode.Disposable.dispose)
    self.log->Chan.emit(SwitchVersionUI(Destroyed))
  }
}

module VersionDisplay = {
  // Format version strings for display
  let formatAgdaVersion = (version: string): string => "Agda v" ++ version

  let formatALSVersion = (alsVersion: string, agdaVersion: string): string =>
    "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion

  let formatSwitchingMessage = (version: string): string => "Switching to " ++ version

  let formatSwitchedMessage = (version: string): string => "Switched to " ++ version
}

module SwitchVersionManager = {
  type t = {
    mutable entries: Dict.t<Memento.Endpoints.entry>,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
  }

  let make = (state: State.t): t => {
    entries: Memento.Endpoints.entries(state.memento),
    memento: state.memento,
    globalStorageUri: state.globalStorageUri,
  }

  // Get current items as data
  let getItemData = async (self: t, state: State.t, downloadInfo: option<(bool, string)>): array<
    ItemData.t,
  > => {
    // Always check current connection to ensure UI reflects actual state
    let storedPath = Memento.PickedConnection.get(self.memento)

    let pickedPath = switch storedPath {
    | Some(path) =>
      // Convert URI format to filesystem path for comparison
      if String.startsWith(path, "file://") {
        Some(VSCode.Uri.parse(path)->VSCode.Uri.fsPath)
      } else {
        Some(path)
      }
    | None =>
      // Fresh install: try to infer active connection from current state
      switch state.connection {
      | Some(connection) =>
        // Extract path from connection
        let connectionPath = switch connection {
        | Agda(_, path, _) => Some(path)
        | ALS(_, path, _) => Some(path)
        }
        connectionPath
      | None => None
      }
    }
    let folderPath = VSCode.Uri.fsPath(self.globalStorageUri)
    ItemData.entriesToItemData(self.entries, pickedPath, downloadInfo, folderPath)
  }

  // Update entries and return if changed
  let refreshFromMemento = (self: t): bool => {
    let newEntries = Memento.Endpoints.entries(self.memento)
    let changed = newEntries !== self.entries
    if changed {
      self.entries = newEntries
    }
    changed
  }

  // Filesystem sync (Phase 2)
  let syncWithFilesystem = async (self: t, platformDeps: Platform.t): bool => {
    module PlatformOps = unpack(platformDeps)

    let discoveredEndpoints = await PlatformOps.getInstalledEndpoints(self.globalStorageUri)

    await Memento.Endpoints.syncWithPaths(self.memento, discoveredEndpoints)
    refreshFromMemento(self)
  }

  // Version probing (Phase 3)
  let probeVersions = async (self: t): bool => {
    let pathsToProbe =
      self.entries
      ->Dict.toArray
      ->Array.filterMap(((path, entry)) => {
        switch entry.endpoint {
        | Agda(None) | ALS(None) => Some(path)
        | _ => None
        }
      })

    if Array.length(pathsToProbe) == 0 {
      false
    } else {
      let probePromises = pathsToProbe->Array.map(async path => {
        switch await Connection.probeFilepath(path) {
        | Ok(_, Ok(agdaVersion)) =>
          await Memento.Endpoints.setVersion(self.memento, path, Agda(Some(agdaVersion)))
          Some(path)
        | Ok(_, Error(alsVersion, agdaVersion, lspOptions)) =>
          await Memento.Endpoints.setVersion(
            self.memento,
            path,
            ALS(Some((alsVersion, agdaVersion, lspOptions))),
          )
          Some(path)
        | Error(error) =>
          await Memento.Endpoints.setError(
            self.memento,
            path,
            Connection__Error.Probe.toString(error),
          )
          Some(path)
        }
      })

      let updateResults = await Promise.all(probePromises)
      let updatedPaths = updateResults->Array.filterMap(x => x)

      if Array.length(updatedPaths) > 0 {
        let _ = refreshFromMemento(self)
        true
      } else {
        false
      }
    }
  }
}

// Connection switching logic
let switchAgdaVersion = async (state: State.t) => {
  // Skip the initial Connection.Endpoint.getPicked() call since it might clear our memento
  // Just show a generic "switching..." message
  await State__View.Panel.displayStatus(state, "")
  await State__View.Panel.display(
    state,
    AgdaModeVscode.View.Header.Plain("Switching connection..."),
    [],
  )

  // stop the old connection
  let _ = await Connection.destroy(state.connection, state.channels.log)

  // start with the new connection
  // Get the selected path from memento and ensure it's included in the supplied paths
  let configPaths = Config.Connection.getAgdaPaths()
  let storedPathBeforeConnection = Memento.PickedConnection.get(state.memento)

  let pathsFromSystem = switch storedPathBeforeConnection {
  | Some(selectedPath) => {
      // Convert path to URI and add to config paths if not already present
      let pathExists = configPaths->Array.includes(selectedPath)
      if pathExists {
        configPaths
      } else {
        // Put the selected path FIRST so it gets priority in getPicked()
        Array.concat([selectedPath], configPaths)
      }
    }
  | None => configPaths
  }

  switch await Connection.make(
    state.platformDeps,
    state.memento,
    state.globalStorageUri,
    pathsFromSystem,
    ["als", "agda"],
    state.channels.log,
  ) {
  | Ok(conn) =>
    state.connection = Some(conn)

    switch conn {
    | Agda(_, _, version) =>
      let formattedVersion = VersionDisplay.formatAgdaVersion(version)
      await State__View.Panel.displayStatus(state, formattedVersion)
      await State__View.Panel.display(
        state,
        AgdaModeVscode.View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
        [],
      )
    | ALS(_, _, (alsVersion, agdaVersion, _)) =>
      let formattedVersion = VersionDisplay.formatALSVersion(alsVersion, agdaVersion)
      await State__View.Panel.displayStatus(state, formattedVersion)
      await State__View.Panel.display(
        state,
        AgdaModeVscode.View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
        [],
      )
    }

  // Use the same pathsFromSystem we used for Connection.make() to ensure consistency
  // switch await Connection.Endpoint.getPicked(state.memento, pathsFromSystem) {
  // | None => ()
  // // | Some(Agda(version, _)) => {
  // //     let formattedVersion = VersionDisplay.formatAgdaVersion(version)
  // //     await State__View.Panel.displayStatus(state, formattedVersion)
  // //     await State__View.Panel.display(
  // //       state,
  // //       AgdaModeVscode.View.Header.Success(
  // //         VersionDisplay.formatSwitchedMessage(formattedVersion),
  // //       ),
  // //       [],
  // //     )
  // //   }
  // // | Some(ALS(alsVersion, agdaVersion, _, _)) => {
  // //     let formattedVersion = VersionDisplay.formatALSVersion(
  // //       alsVersion,
  // //       agdaVersion,
  // //     )
  // //     await State__View.Panel.displayStatus(state, formattedVersion)
  // //     await State__View.Panel.display(
  // //       state,
  // //       AgdaModeVscode.View.Header.Success(
  // //         VersionDisplay.formatSwitchedMessage(formattedVersion),
  // //       ),
  // //       [],
  // //     )
  // //   }
  // }

  | Error(error) => {
      let (errorHeader, errorBody) = Connection.Error.toString(error)
      let header = AgdaModeVscode.View.Header.Error(
        "Failed to switch to a different installation: " ++ errorHeader,
      )
      let body = [AgdaModeVscode.Item.plainText(errorBody)]
      await State__View.Panel.display(state, header, body)
    }
  }
}

// Download module - handles download-related business logic
module Download = {
  // Get available download info: (downloaded, versionString)
  let getAvailableDownload = async (state: State.t, platformDeps: Platform.t): option<(
    bool,
    string,
  )> => {
    module PlatformOps = unpack(platformDeps)

    // Check if we can download ALS for this platform
    switch await PlatformOps.determinePlatform() {
    | Error(_) => None
    | Ok(platform) =>
      switch await Connection__LatestALS.getFetchSpec(
        state.memento,
        state.globalStorageUri,
        platform,
      ) {
      | Error(_) => None
      | Ok(fetchSpec) =>
        // Check if already downloaded
        let filepath = VSCode.Uri.joinPath(
          state.globalStorageUri,
          [fetchSpec.saveAsFileName, "als"],
        )
        let downloaded = switch await PlatformOps.alreadyDownloaded(filepath)() {
        | Some(_) => true
        | None => false
        }

        // Format version string for display
        let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
          asset.name
          ->String.replaceRegExp(%re("/als-Agda-/"), "")
          ->String.replaceRegExp(%re("/-.*/"), "")
        let agdaVersion = getAgdaVersion(fetchSpec.asset)
        let alsVersion =
          fetchSpec.release.name
          ->String.split(".")
          ->Array.last
          ->Option.getOr(fetchSpec.release.name)

        let versionString = VersionDisplay.formatALSVersion(alsVersion, agdaVersion)

        Some((downloaded, versionString))
      }
    }
  }
}

// Event handlers module for testability and debugging
module Handler = {
  let onSelection = (
    state: State.t,
    platformDeps: Platform.t,
    manager: SwitchVersionManager.t,
    updateUI,
    view: View.t,
    selectedItems: array<VSCode.QuickPickItem.t>,
  ) => {
    view->View.destroy
    let _ = (
      async () => {
        switch selectedItems[0] {
        | Some(selectedItem) =>
          // Check if this is the open folder item
          if selectedItem.label == Constants.openDownloadFolder {
            let globalStorageUriAsFile = state.globalStorageUri->VSCode.Uri.fsPath->VSCode.Uri.file
            state.channels.log->Chan.emit(
              Log.SwitchVersionUI(SelectedOpenFolder(state.globalStorageUri->VSCode.Uri.fsPath)),
            )
            let _ = await VSCode.Env.openExternal(globalStorageUriAsFile)
            state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
          } else if selectedItem.label == Constants.downloadLatestALS {
            // Check if already downloaded using our corrected logic
            switch await Download.getAvailableDownload(state, platformDeps) {
            | Some((downloaded, versionString)) =>
              let alreadyDownloaded = downloaded

              state.channels.log->Chan.emit(
                Log.SwitchVersionUI(SelectedDownloadAction(downloaded, versionString)),
              )

              if alreadyDownloaded {
                // Show "already downloaded" message
                VSCode.Window.showInformationMessage(
                  versionString ++ " is already downloaded",
                  [],
                )->Promise.done
                state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
              } else {
                module PlatformOps = unpack(platformDeps)
                switch await PlatformOps.determinePlatform() {
                | Error(_) =>
                  VSCode.Window.showErrorMessage(
                    "Failed to determine the platform for downloading the Agda Language Server",
                    [],
                  )->Promise.done
                  state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                | Ok(platform) =>
                  switch await PlatformOps.downloadLatestALS(
                    state.channels.log,
                    state.memento,
                    state.globalStorageUri,
                  )(platform) {
                  | Error(error) =>
                    VSCode.Window.showErrorMessage(
                      AgdaModeVscode.Connection__Download.Error.toString(error),
                      [],
                    )->Promise.done
                    state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                  | Ok(_) =>
                    // Refresh the UI to show new status
                    let newDownloadInfo = await Download.getAvailableDownload(state, platformDeps)
                    let _ = SwitchVersionManager.refreshFromMemento(manager)
                    await updateUI(newDownloadInfo)
                    VSCode.Window.showInformationMessage(
                      versionString ++ " successfully downloaded",
                      [],
                    )->Promise.done // although we give no options for users to select, the promise won't resolve until the message is dismissed, so we are not waiting for it
                    state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                  }
                }
              }
            | None =>
              let _ = await VSCode.Window.showErrorMessage(
                "Download not available for this platform",
                [],
              )
              state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
            }
          } else {
            // Regular endpoint selection - check if selection changed
            switch selectedItem.detail {
            | Some(selectedPath) =>
              let changed = switch Memento.PickedConnection.get(manager.memento) {
              | Some(path) => selectedPath !== path
              | None => true // If no previous selection, treat as changed
              }
              if changed {
                // Log the endpoint selection before processing it
                switch manager.entries->Dict.get(selectedPath) {
                | Some(entry) =>
                  state.channels.log->Chan.emit(
                    Log.SwitchVersionUI(SelectedEndpoint(selectedPath, entry, true)),
                  )
                | None => ()
                }

                let uri = Connection.URI.parse(selectedPath)
                switch uri {
                | FileURI(_, vsCodeUri) =>
                  let path = VSCode.Uri.fsPath(vsCodeUri)
                  await Memento.PickedConnection.set(state.memento, Some(path))
                  await Config.Connection.addAgdaPath(state.channels.log, path)
                  await switchAgdaVersion(state)
                  state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                | LspURI(_, _) =>
                  state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                }
              } else {
                // No change in selection - still emit completion
                state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
              }
            | None => state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
            }
          }
        | None => state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
        }
      }
    )()
  }

  let onHide = (view: View.t) => {
    // QuickPick was hidden/cancelled by user - clean up
    view->View.destroy
  }

  let onActivate = async (state: State.t, platformDeps: Platform.t) => {
    let manager = SwitchVersionManager.make(state)
    let view = View.make(state.channels.log)

    // No initialization needed since we removed getPicked() -
    // just use whatever is stored in memento directly

    // Helper function to update UI with current state
    let updateUI = async (downloadInfo: option<(bool, string)>): unit => {
      let itemData = await SwitchVersionManager.getItemData(manager, state, downloadInfo)

      // Log selection marking for testing observability
      let endpointItemDatas = itemData->Array.filterMap(item =>
        switch item {
        | Endpoint(path, entry, isSelected) => Some(path, entry.endpoint, entry.error, isSelected)
        | _ => None
        }
      )
      state.channels.log->Chan.emit(Log.SwitchVersionUI(UpdatedEndpoints(endpointItemDatas)))

      let items = Item.fromItemDataArray(itemData, state.extensionUri)
      view->View.updateItems(items)
    }

    // Setup quickpick
    view->View.setPlaceholder("Switch Agda Version")

    // PHASE 1: Show cached items immediately with visual marking
    await updateUI(None)
    view->View.show
    state.channels.log->Chan.emit(SwitchVersionUI(Others("QuickPick shown")))

    // Get download info asynchronously in background
    let downloadInfoPromise = Download.getAvailableDownload(state, platformDeps)

    // Setup event handlers
    view->View.onSelection(selectedItems =>
      onSelection(state, platformDeps, manager, updateUI, view, selectedItems)
    )
    view->View.onHide(() => onHide(view))

    // Background update process (sequential phases)
    let backgroundUpdate = async () => {
      try {
        // Get download info
        let downloadInfo = await downloadInfoPromise

        // PHASE 2: Sync with filesystem (discover new paths)
        let phase2Changed = await SwitchVersionManager.syncWithFilesystem(manager, platformDeps)
        if phase2Changed {
          await updateUI(downloadInfo)
        }

        // PHASE 3: Probe version information
        let phase3Changed = await SwitchVersionManager.probeVersions(manager)
        if phase3Changed {
          await updateUI(downloadInfo)
        }

        // Update UI with download item even if no other changes
        if !phase2Changed && !phase3Changed {
          await updateUI(downloadInfo)
        }
      } catch {
      | _exn => () // Ignore background update errors
      }
    }

    // Start background update
    let _ = backgroundUpdate()
  }
}

// Main entry point
let activate = Handler.onActivate
