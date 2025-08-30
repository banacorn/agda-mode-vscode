// Constants for reused UI strings
module Constants = {
  let agdaVersionPrefix = "Agda v"
  let alsWithSquirrel = "$(squirrel)  ALS v"
  let downloadLatestALS = "$(cloud-download)  Download the latest Agda Language Server"
  let openDownloadFolder = "$(folder-opened)  Open download folder"
  let downloadedAndInstalled = "Downloaded and installed"
  let otherDownloads = "$(chevron-right)  Other Downloads"
  let otherDownloadsDesc = "Browse other versions available for download"
}

module ItemData = {
  // UI item types
  type t =
    | Endpoint(string, Memento.Endpoints.entry, bool) // path, entry, is selected
    | DownloadAction(bool, string, string) // downloaded, versionString, downloadType
    | OtherVersionsSubmenu // submenu button for other versions downloads
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
    | DownloadAction(downloaded, versionString, downloadType) =>
      "DownloadAction: downloaded=" ++
      string_of_bool(downloaded) ++
      ", versionString=" ++
      versionString ++
      ", type=" ++
      downloadType
    | OtherVersionsSubmenu => "OtherVersionsSubmenu"
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
    downloadItems: array<(bool, string, string)>, // (downloaded, versionString, downloadType)
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
    let sectionsWithDownload = if Array.length(downloadItems) > 0 {
      // Separate dev and non-dev downloads
      let devDownloads =
        downloadItems->Array.filter(((_, _, downloadType)) => downloadType == "dev")
      let otherDownloads =
        downloadItems->Array.filter(((_, _, downloadType)) => downloadType != "dev")

      // Create download section items
      let downloadSectionItems = Array.concat(
        // Add other download actions first (latest ALS)
        otherDownloads->Array.map(((downloaded, versionString, downloadType)) => DownloadAction(
          downloaded,
          versionString,
          downloadType,
        )),
        // Add OtherVersionsSubmenu after latest ALS if there are dev downloads
        if Array.length(devDownloads) > 0 {
          [OtherVersionsSubmenu]
        } else {
          []
        },
      )

      if Array.length(downloadSectionItems) > 0 {
        Array.concat(
          sectionsWithInstalled,
          Array.concat([Separator("Download")], downloadSectionItems),
        )
      } else {
        sectionsWithInstalled
      }
    } else {
      sectionsWithInstalled
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
      | DownloadAction(downloaded, versionString, downloadType) => {
          let label =
            downloadType == "dev"
              ? "$(cloud-download)  Download the dev Agda Language Server"
              : Constants.downloadLatestALS
          let description = downloaded ? Constants.downloadedAndInstalled : ""
          (label, Some(description), Some(versionString))
        }
      | OtherVersionsSubmenu => {
          let label = Constants.otherDownloads
          let description = Constants.otherDownloadsDesc
          (label, Some(description), None)
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

  let formatDevALSVersion = (agdaVersion: string): string =>
    "Agda v" ++ agdaVersion ++ " Language Server (dev build)"

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
  let getItemData = async (
    self: t,
    state: State.t,
    downloadItems: array<(bool, string, string)>,
  ): array<ItemData.t> => {
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
    ItemData.entriesToItemData(self.entries, pickedPath, downloadItems, folderPath)
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

  // Helper function to infer endpoint type from filename
  let inferEndpointType = (filename: string) => {
    let baseName = filename->String.toLowerCase->NodeJs.Path.basename
    // Remove common executable extensions
    let cleanName =
      baseName
      ->String.replace(".exe", "")
      ->String.replace(".cmd", "")
      ->String.replace(".bat", "")

    if cleanName == "agda" || cleanName->String.startsWith("agda-") {
      Memento.Endpoints.Agda(None)
    } else if cleanName == "als" || cleanName->String.startsWith("als-") {
      Memento.Endpoints.ALS(None)
    } else {
      Memento.Endpoints.Unknown
    }
  }

  // Filesystem sync (Phase 2)
  let syncWithFilesystem = async (self: t, platformDeps: Platform.t): bool => {
    module PlatformOps = unpack(platformDeps)

    let endpoints = Dict.make()

    // Add paths from user config
    Config.Connection.getAgdaPaths()->Array.forEach(path => {
      let filename = NodeJs.Path.basename(path)
      let endpoint = inferEndpointType(filename)
      endpoints->Dict.set(path, endpoint)
    })

    // Add agda and als from PATH
    switch await PlatformOps.findCommand("agda") {
    | Ok(path) => endpoints->Dict.set(path, Memento.Endpoints.Agda(None))
    | Error(_) => ()
    }
    switch await PlatformOps.findCommand("als") {
    | Ok(path) => endpoints->Dict.set(path, Memento.Endpoints.ALS(None))
    | Error(_) => ()
    }

    let discoveredEndpoints = endpoints

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
        | Ok(_, IsAgda(agdaVersion)) =>
          await Memento.Endpoints.setVersion(self.memento, path, Agda(Some(agdaVersion)))
          Some(path)
        | Ok(_, IsALS(alsVersion, agdaVersion, lspOptions)) =>
          await Memento.Endpoints.setVersion(
            self.memento,
            path,
            ALS(Some((alsVersion, agdaVersion, lspOptions))),
          )
          Some(path)
        | Ok(_, IsALSOfUnknownVersion(_)) =>
          await Memento.Endpoints.setVersion(self.memento, path, ALS(None))
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

// Helper function to combine `PlatformOps.determinePlatform` + `PlatformOps.getDownloadDescriptor`
let getDownloadDescriptorWithPlatform = async (
  platformOps: Platform.t,
  target: Connection__Download.DownloadOrderAbstract.t,
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
) => {
  module PlatformOps = unpack(platformOps)
  switch await PlatformOps.determinePlatform() {
  | Error(_error) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
  | Ok(platform) =>
    await PlatformOps.getDownloadDescriptor(target, true)(memento, globalStorageUri, platform)
  }
}

// Connection switching logic
let switchAgdaVersion = async (state: State.t, uri) => {
  // convert URI to string path
  let path = switch uri {
  | Connection__URI.FileURI(_, vsCodeUri) => VSCode.Uri.fsPath(vsCodeUri)
  | LspURI(raw, _) => raw
  }

  // Skip the initial Connection.Endpoint.getPicked() call since it might clear our memento
  // Just show a generic "switching..." message
  await State__View.Panel.displayStatus(state, "")
  await State__View.Panel.display(
    state,
    AgdaModeVscode.View.Header.Plain("Switching connection..."),
    [],
  )

  switch await Connection.make(path) {
  | Ok(conn) =>
    // stop the old connection
    let _ = await Connection.destroy(state.connection, state.channels.log)

    // update state
    state.connection = Some(conn)
    await Memento.PickedConnection.set(state.memento, Some(path))
    await Config.Connection.addAgdaPath(state.channels.log, path)

    // update memento with discovered version information and display success message
    switch conn {
    | Agda(_, _, version) =>
      await Memento.Endpoints.setVersion(state.memento, path, Memento.Endpoints.Agda(Some(version)))
      let formattedVersion = VersionDisplay.formatAgdaVersion(version)
      await State__View.Panel.displayStatus(state, formattedVersion)
      await State__View.Panel.display(
        state,
        AgdaModeVscode.View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
        [],
      )
    | ALS(_, _, Some(alsVersion, agdaVersion, lspOptions)) =>
      await Memento.Endpoints.setVersion(
        state.memento,
        path,
        Memento.Endpoints.ALS(Some(alsVersion, agdaVersion, lspOptions)),
      )
      let formattedVersion = VersionDisplay.formatALSVersion(alsVersion, agdaVersion)
      await State__View.Panel.displayStatus(state, formattedVersion)
      await State__View.Panel.display(
        state,
        AgdaModeVscode.View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
        [],
      )
    | ALS(_, _, None) =>
      // version still unknown, don't update memento
      let formattedVersion = "Agda Language Server of unknown version"
      await State__View.Panel.displayStatus(state, formattedVersion)
      await State__View.Panel.display(
        state,
        AgdaModeVscode.View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
        [],
      )
    }
  | Error(error) => {
      let (errorHeader, errorBody) = Connection.Error.toString(Establish(error))
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
  // Get available download info for latest ALS: (downloaded, versionString, downloadType)
  let getAvailableLatestDownload = async (state: State.t, platformDeps: Platform.t): option<(
    bool,
    string,
    string,
  )> => {
    module PlatformOps = unpack(platformDeps)

    // Check if we can download ALS for this platform
    switch await getDownloadDescriptorWithPlatform(
      platformDeps,
      LatestALS,
      state.memento,
      state.globalStorageUri,
    ) {
    | Error(_) => None
    | Ok(downloadDescriptor) =>
      // Check if already downloaded
      let downloaded = switch await PlatformOps.alreadyDownloaded(
        state.globalStorageUri,
        LatestALS,
      ) {
      | Some(_) => true
      | None => false
      }

      // Format version string for display
      let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
        asset.name
        ->String.replaceRegExp(%re("/als-Agda-/"), "")
        ->String.replaceRegExp(%re("/-.*/"), "")
      let agdaVersion = getAgdaVersion(downloadDescriptor.asset)
      let alsVersion =
        downloadDescriptor.release.name
        ->String.split(".")
        ->Array.last
        ->Option.getOr(downloadDescriptor.release.name)

      let versionString = VersionDisplay.formatALSVersion(alsVersion, agdaVersion)

      Some((downloaded, versionString, "latest"))
    }
  }

  // Get available download info for dev ALS: (downloaded, versionString, downloadType)
  let getAvailableDevDownload = async (state: State.t, platformDeps: Platform.t): option<(
    bool,
    string,
    string,
  )> => {
    module PlatformOps = unpack(platformDeps)

    // Check if we can download dev ALS for this platform
    switch await getDownloadDescriptorWithPlatform(
      platformDeps,
      DevALS,
      state.memento,
      state.globalStorageUri,
    ) {
    | Error(_error) => None
    | Ok(downloadDescriptor) =>
      // Check if already downloaded
      let downloaded = switch await PlatformOps.alreadyDownloaded(state.globalStorageUri, DevALS) {
      | Some(_) => true
      | None => false
      }

      // Format version string for display
      let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
        asset.name
        ->String.replaceRegExp(%re("/als-dev-Agda-/"), "")
        ->String.replaceRegExp(%re("/-.*/"), "")
      let agdaVersion = getAgdaVersion(downloadDescriptor.asset)

      let versionString = VersionDisplay.formatDevALSVersion(agdaVersion)
      Some((downloaded, versionString, "dev"))
    }
  }

  // Get available download info for dev WASM ALS: (downloaded, versionString, downloadType)
  let getAvailableDevWASMDownload = async (state: State.t, platformDeps: Platform.t): option<(
    bool,
    string,
    string,
  )> => {
    module PlatformOps = unpack(platformDeps)

    // Check if we can download dev WASM ALS for this platform
    switch await getDownloadDescriptorWithPlatform(
      platformDeps,
      DevWASMALS,
      state.memento,
      state.globalStorageUri,
    ) {
    | Error(_error) => None
    | Ok(_) =>
      // Check if already downloaded
      let downloaded = switch await PlatformOps.alreadyDownloaded(
        state.globalStorageUri,
        DevWASMALS,
      ) {
      | Some(_) => true
      | None => false
      }

      // Format version string for display
      let versionString = "WASM Language Server (dev build)"

      Some((downloaded, versionString, "wasm"))
    }
  }

  // Create placeholder download items to prevent UI jitter
  let getPlaceholderDownloadItems = (): array<(bool, string, string)> => {
    let items = [(false, "Checking availability...", "latest")]

    // Add dev placeholder if dev mode is enabled
    if Config.DevMode.get() {
      Array.concat(items, [(false, "Checking availability...", "dev")])
    } else {
      items
    }
  }

  // Get all available downloads
  let getAllAvailableDownloads = async (state: State.t, platformDeps: Platform.t): array<(
    bool,
    string,
    string,
  )> => {
    let latestPromise = getAvailableLatestDownload(state, platformDeps)

    // Always maintain fixed structure - replace placeholders with actual data or keep placeholders
    let latestItem = switch await latestPromise {
    | Some(item) => item
    | None => (false, "Not available for this platform", "latest")
    }

    // Only include dev ALS when dev mode is enabled
    let devItem = if Config.DevMode.get() {
      switch await getAvailableDevDownload(state, platformDeps) {
      | Some(item) => [item]
      | None => [(false, "Not available for this platform", "dev")]
      }
    } else {
      []
    }

    [latestItem, ...devItem]
  }
}

// Event handlers module for testability and debugging
module Handler = {
  // Unified function to handle all download logic (DevALS, DevWASMALS, and LatestALS)
  let handleDownload = async (
    state: State.t,
    platformDeps: Platform.t,
    target: Connection__Download.DownloadOrderAbstract.t,
    downloaded: bool,
    versionString: string,
    ~refreshUI: option<unit => promise<unit>>=None,
  ) => {
    state.channels.log->Chan.emit(
      Log.SwitchVersionUI(SelectedDownloadAction(downloaded, versionString)),
    )

    if downloaded {
      // Add already downloaded path to config
      module PlatformOps = unpack(platformDeps)
      switch await getDownloadDescriptorWithPlatform(
        platformDeps,
        target,
        state.memento,
        state.globalStorageUri,
      ) {
      | Ok(downloadDescriptor) =>
        // Determine file extension based on target
        let fileName = switch target {
        | DevWASMALS => "als.wasm"
        | DevALS | LatestALS => "als"
        }
        let downloadedPath =
          VSCode.Uri.joinPath(
            state.globalStorageUri,
            [downloadDescriptor.saveAsFileName, fileName],
          )->VSCode.Uri.fsPath
        await Config.Connection.addAgdaPath(state.channels.log, downloadedPath)
      | Error(_) => ()
      }
      VSCode.Window.showInformationMessage(
        versionString ++ " is already downloaded",
        [],
      )->Promise.done
    } else {
      // Perform download
      module PlatformOps = unpack(platformDeps)
      let downloadResult = switch await getDownloadDescriptorWithPlatform(
        platformDeps,
        target,
        state.memento,
        state.globalStorageUri,
      ) {
      | Error(error) => Error(error)
      | Ok(downloadDescriptor) =>
        await PlatformOps.download(state.globalStorageUri, downloadDescriptor)
      }

      switch downloadResult {
      | Error(error) =>
        VSCode.Window.showErrorMessage(
          AgdaModeVscode.Connection__Download.Error.toString(error),
          [],
        )->Promise.done
      | Ok(downloadedPath) =>
        await Config.Connection.addAgdaPath(state.channels.log, downloadedPath)

        // Optional UI refresh after successful download
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
    state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
  }

  // Submenu for other versions downloads
  let showOtherVersionsSubmenu = async (state: State.t, platformDeps: Platform.t): unit => {
    let submenuView = View.make(state.channels.log)

    // Get dev download options
    let devDownloadInfo = await Download.getAvailableDevDownload(state, platformDeps)
    let wasmDownloadInfo = await Download.getAvailableDevWASMDownload(state, platformDeps)

    let submenuItems = {
      let devItems = switch devDownloadInfo {
      | Some((downloaded, versionString, _)) => {
          let downloadLabel = "$(cloud-download)  Download dev ALS"
          let description = downloaded ? Constants.downloadedAndInstalled : ""
          let downloadItem = Item.createQuickPickItem(
            downloadLabel,
            Some(description),
            Some(versionString),
          )
          [downloadItem]
        }
      | None => []
      }

      // Add WASM build option with dynamic status
      let wasmItems = switch wasmDownloadInfo {
      | Some((downloaded, versionString, _)) => {
          let downloadLabel = "$(cloud-download)  Download WASM build"
          let description = downloaded ? Constants.downloadedAndInstalled : ""
          let wasmItem = Item.createQuickPickItem(
            downloadLabel,
            Some(description),
            Some(versionString),
          )
          [wasmItem]
        }
      | None => []
      }

      let allItems = Array.concat(devItems, wasmItems)

      if Array.length(allItems) == 0 {
        let noDownloadItem = Item.createQuickPickItem(
          "$(info)  No dev downloads available",
          Some("Dev downloads not available"),
          None,
        )
        [noDownloadItem]
      } else {
        allItems
      }
    }

    submenuView->View.setPlaceholder(Constants.otherDownloadsDesc)
    submenuView->View.updateItems(submenuItems)
    submenuView->View.show

    // Handle submenu selection
    submenuView->View.onSelection(selectedItems => {
      submenuView->View.destroy
      let _ = (
        async () => {
          switch selectedItems[0] {
          | Some(selectedItem) =>
            if String.startsWith(selectedItem.label, "$(cloud-download)") {
              // Check if WASM build was selected
              if String.includes(selectedItem.label, "WASM") {
                // Handle dev WASM ALS download action
                switch wasmDownloadInfo {
                | Some((downloaded, versionString, _)) =>
                  await handleDownload(state, platformDeps, DevWASMALS, downloaded, versionString)
                | None =>
                  VSCode.Window.showErrorMessage(
                    "Download not available for this platform",
                    [],
                  )->Promise.done
                  state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                }
              } else {
                // Handle dev ALS download action
                switch devDownloadInfo {
                | Some((downloaded, versionString, _)) =>
                  await handleDownload(state, platformDeps, DevALS, downloaded, versionString)
                | None =>
                  VSCode.Window.showErrorMessage(
                    "Download not available for this platform",
                    [],
                  )->ignore
                  state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
                }
              }
            }
          | None => state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
          }
        }
      )()
    })

    // Handle submenu hide/cancel
    submenuView->View.onHide(() => {
      submenuView->View.destroy
    })
  }
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
          // Check if this is the other versions submenu item
          if selectedItem.label == Constants.otherDownloads {
            await showOtherVersionsSubmenu(state, platformDeps)
          } else if selectedItem.label == Constants.openDownloadFolder {
            let globalStorageUriAsFile = state.globalStorageUri->VSCode.Uri.fsPath->VSCode.Uri.file
            state.channels.log->Chan.emit(
              Log.SwitchVersionUI(SelectedOpenFolder(state.globalStorageUri->VSCode.Uri.fsPath)),
            )
            module PlatformOps = unpack(platformDeps)
            await PlatformOps.openFolder(globalStorageUriAsFile)
            state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
          } else if selectedItem.label == Constants.downloadLatestALS {
            // Handle latest ALS download
            let downloadInfoResult = await Download.getAvailableLatestDownload(state, platformDeps)

            switch downloadInfoResult {
            | Some((downloaded, versionString, _)) =>
              await handleDownload(
                state,
                platformDeps,
                LatestALS,
                downloaded,
                versionString,
                ~refreshUI=Some(
                  async () => {
                    let newDownloadItems = await Download.getAllAvailableDownloads(
                      state,
                      platformDeps,
                    )
                    let _ = SwitchVersionManager.refreshFromMemento(manager)
                    await updateUI(newDownloadItems)
                  },
                ),
              )
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
                await switchAgdaVersion(state, uri)
                state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
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
    let updateUI = async (downloadItems: array<(bool, string, string)>): unit => {
      let itemData = await SwitchVersionManager.getItemData(manager, state, downloadItems)

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

    // PHASE 1: Show cached items immediately with placeholders to prevent jitter
    await updateUI(Download.getPlaceholderDownloadItems())
    view->View.show
    state.channels.log->Chan.emit(SwitchVersionUI(Others("QuickPick shown")))

    // Get download info asynchronously in background
    let downloadItemsPromise = Download.getAllAvailableDownloads(state, platformDeps)

    // Setup event handlers
    view->View.onSelection(selectedItems =>
      onSelection(state, platformDeps, manager, updateUI, view, selectedItems)
    )
    view->View.onHide(() => onHide(view))

    // Background update process (sequential phases)
    let backgroundUpdate = async () => {
      try {
        // Get download info
        let downloadItems = await downloadItemsPromise

        // PHASE 2: Sync with filesystem (discover new paths)
        let phase2Changed = await SwitchVersionManager.syncWithFilesystem(manager, platformDeps)
        if phase2Changed {
          await updateUI(downloadItems)
        }

        // PHASE 3: Probe version information
        let phase3Changed = await SwitchVersionManager.probeVersions(manager)
        if phase3Changed {
          await updateUI(downloadItems)
        }

        // Update UI with download item even if no other changes
        if !phase2Changed && !phase3Changed {
          await updateUI(downloadItems)
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
