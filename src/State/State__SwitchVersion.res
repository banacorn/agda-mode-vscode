module Candidate = Connection__Candidate
module ResolvedMetadata = Memento.ResolvedMetadata

// Constants for reused UI strings
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

module ItemData = {
  // UI item types
  type t =
    | Candidate(string, string, ResolvedMetadata.entry, bool) // raw candidate, detail, entry, is selected
    | DownloadAction(bool, string, string) // downloaded, versionString, variant ("native" | "wasm")
    | SelectOtherChannels
    | DeleteDownloads // delete downloads and clear cache
    | NoInstallations
    | Separator(string) // label

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

  // UI display logic
  let shouldCandidateHaveIcon = (kind: ResolvedMetadata.kind): bool => {
    switch kind {
    | Agda(_) => true
    | _ => false
    }
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

  // Convert entries to item data
  let entriesToItemData = (
    entries: array<(string, string, ResolvedMetadata.entry)>,
    pickedPath: option<string>,
    downloadItems: array<(bool, string, string)>, // (downloaded, versionString, variant)
    ~downloadHeader: string="Download (channel: DevALS)",
  ): array<t> => {
    let hasCandidates = Array.length(entries) > 0

    // Build candidate items with selection check (candidate-aware, at most one selected)
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

    let downloadSectionItems =
      downloadItems->Array.map(((downloaded, versionString, variant)) => DownloadAction(
        downloaded,
        versionString,
        variant,
      ))

    let channelItems = [SelectOtherChannels]

    let downloadSection =
      Array.concat(
        [Separator(downloadHeader)],
        Array.concat(downloadSectionItems, Array.concat(channelItems, [DeleteDownloads])),
      )

    Array.concat(candidateSection, downloadSection)
  }
}

module Item = {
  type t = {
    label: string,
    description?: string,
    detail?: string,
    iconPath?: VSCode.IconPath.t,
    kind?: VSCode.QuickPickItemKind.t,
    data: ItemData.t,
  }

  // Helper function to create typed QuickPick item from label, description, detail, and payload
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

  // Convert item data to typed QuickPick item
  let fromItemData = (itemData: ItemData.t, extensionUri: VSCode.Uri.t): t => {
    // Convert item data to UI display information
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
          | "native" => Constants.downloadNativeALS
          | "wasm" => Constants.downloadWasmALS
          | _ => "$(cloud-download)  Download Agda Language Server"
          }
          let description = downloaded ? Constants.downloadedAndInstalled : ""
          (label, Some(description), Some(versionString))
        }
      | SelectOtherChannels => (Constants.selectOtherChannels, None, None)
      | DeleteDownloads => {
          let label = Constants.deleteDownloads
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

  // Convert array of item data to typed QuickPick items
  let fromItemDataArray = (itemDataArray: array<ItemData.t>, extensionUri: VSCode.Uri.t): array<t> => {
    itemDataArray->Array.map(itemData => fromItemData(itemData, extensionUri))
  }
}

module View = {
  type t = {
    log: Chan.t<Log.t>,
    quickPick: VSCode.QuickPick.t<Item.t>,
    subscriptions: array<VSCode.Disposable.t>,
    mutable items: array<Item.t>,
    mutable suppressHide: bool,
  }

  let make = (log): t => {
    let quickPick: VSCode.QuickPick.t<Item.t> = VSCode.Window.createQuickPick()
    {
      log,
      quickPick,
      subscriptions: [],
      items: [],
      suppressHide: false,
    }
  }

  let setPlaceholder = (self: t, placeholder: string): unit => {
    self.quickPick->VSCode.QuickPick.setPlaceholder(placeholder)
  }

  let updateItems = (self: t, items: array<Item.t>): unit => {
    self.items = items
    self.quickPick->VSCode.QuickPick.setItems(items)
  }

  let show = (self: t): unit => {
    self.quickPick->VSCode.QuickPick.show
  }

  let onSelection = (self: t, handler: array<Item.t> => unit): unit => {
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

module SwitchVersionManager = {
  type t = {
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
  }

  // Helper function to infer candidate type from filename
  let inferCandidateKind = (raw: string) => {
    let (baseName, parentDirName, releaseDirName, isUnderManagedReleases) = switch Candidate.make(
      raw,
    ) {
    | Candidate.Command(command) => (command->String.toLowerCase, None, None, false)
    | Candidate.Resource(uri) =>
      let path = VSCode.Uri.path(uri)
      let parentPath = path->NodeJs.Path.dirname
      let releasePath = parentPath->NodeJs.Path.dirname
      let releasesPath = releasePath->NodeJs.Path.dirname
      (
        path->NodeJs.Path.basename->String.toLowerCase,
        Some(parentPath->NodeJs.Path.basename),
        Some(releasePath->NodeJs.Path.basename),
        releasesPath->NodeJs.Path.basename == "releases",
      )
    }
    // Remove common executable extensions
    let isWasm = baseName->String.endsWith(".wasm")
    let cleanName =
      baseName
      ->String.replace(".exe", "")
      ->String.replace(".cmd", "")
      ->String.replace(".bat", "")
      ->String.replace(".wasm", "")

    if cleanName == "agda" || cleanName->String.startsWith("agda-") {
      ResolvedMetadata.Agda(None)
    } else if cleanName == "als" || cleanName->String.startsWith("als-") {
      switch parentDirName {
      | Some(dirName) =>
        switch Connection__Download.DownloadArtifact.parseName(dirName) {
        | Some(artifact) =>
          switch releaseDirName {
          | Some(releaseDirName)
              if isUnderManagedReleases && releaseDirName != artifact.releaseTag =>
            ResolvedMetadata.Unknown
          | _ =>
            ResolvedMetadata.ALS(
              isWasm ? WASM : Native,
              Some((artifact.releaseTag, artifact.agdaVersion, None)),
            )
          }
        | None =>
          isUnderManagedReleases
            ? ResolvedMetadata.Unknown
            : ResolvedMetadata.ALS(isWasm ? WASM : Native, None)
        }
      | _ => ResolvedMetadata.ALS(isWasm ? WASM : Native, None)
      }
    } else {
      ResolvedMetadata.Unknown
    }
  }

  let make = (state: State.t): t => {
    memento: state.memento,
    globalStorageUri: state.globalStorageUri,
  }

  // Get current items as data
  let getItemData = async (
    self: t,
    downloadItems: array<(bool, string, string)>,
    ~downloadHeader: string="Download (channel: DevALS)",
    ~platformDeps: option<Platform.t>=None,
  ): array<ItemData.t> => {
    // Always check current connection to ensure UI reflects actual state
    let storedPath = Memento.PreferredCandidate.get(self.memento)

    let pickedPath = switch storedPath {
    | Some(path) => Some(path)
    | None =>
      // Fresh install: try to infer active connection from current registry state
      switch Registry__Connection.status.contents {
      | Active(resource) => Some(Connection.getPath(resource.connection))
      | _ => None
      }
    }

    let detailForCandidate = async (path: string, candidate: Candidate.t): string =>
      switch (candidate, platformDeps) {
      | (Candidate.Command(command), Some(platformDeps)) =>
        module PlatformOps = unpack(platformDeps)
        switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
        | Ok(resolved) =>
          let resolvedPath =
            if VSCode.Uri.scheme(resolved.resource) == "file" {
              VSCode.Uri.fsPath(resolved.resource)
            } else {
              VSCode.Uri.toString(resolved.resource)
            }
          command ++ " (" ++ resolvedPath ++ ")"
        | Error(_) => command
        }
      | (Candidate.Command(command), None) => command
      | (Candidate.Resource(_), _) => path
      }

    let candidateEntries =
      await Promise.all(
        Config.Connection.getAgdaPaths()
        ->Array.toReversed
        ->Array.map(async path => {
          let candidate = Candidate.make(path)
          let detail = await detailForCandidate(path, candidate)
          let resolvedMetadata = switch (candidate, platformDeps) {
          | (Candidate.Resource(uri), _) =>
            let resolved: Candidate.Resolved.t = {original: candidate, resource: uri}
            Memento.ResolvedMetadata.get(self.memento, resolved)
          | (Candidate.Command(_), Some(platformDeps)) =>
            module PlatformOps = unpack(platformDeps)
            switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
            | Ok(resolved) => Memento.ResolvedMetadata.get(self.memento, resolved)
            | Error(_) => None
            }
          | (Candidate.Command(_), None) => None
          }
          let entry = switch resolvedMetadata {
          | Some(entry) => entry
          | None => {
              kind: inferCandidateKind(path),
              timestamp: Date.make(),
              error: None,
            }
          }
          (path, detail, entry)
        }),
      )

    ItemData.entriesToItemData(
      candidateEntries,
      pickedPath,
      downloadItems,
      ~downloadHeader,
    )
  }

  // Version probing (Phase 3)
  let probeVersions = async (self: t, platformDeps: Platform.t): bool => {
    let pathsToProbe = Config.Connection.getAgdaPaths()

    if Array.length(pathsToProbe) == 0 {
      false
    } else {
      module PlatformOps = unpack(platformDeps)
      let probePromises = pathsToProbe->Array.map(async path => {
        let candidate = Candidate.make(path)
        switch await Connection.probeCandidate(platformDeps, candidate) {
        | Ok((resolved, IsAgda(agdaVersion))) =>
          await Memento.ResolvedMetadata.setKind(
            self.memento,
            resolved,
            Agda(Some(agdaVersion)),
          )
          Some(path)
        | Ok((resolved, IsALS(alsVersion, agdaVersion, lspOptions))) =>
          await Memento.ResolvedMetadata.setKind(
            self.memento,
            resolved,
            ALS(Native, Some((alsVersion, agdaVersion, lspOptions))),
          )
          Some(path)
        | Ok((resolved, IsALSWASM(_))) =>
          let kind = switch Memento.ResolvedMetadata.get(self.memento, resolved) {
          | Some({kind: ResolvedMetadata.ALS(WASM, Some(versionInfo))}) =>
            ResolvedMetadata.ALS(WASM, Some(versionInfo))
          | _ =>
            switch inferCandidateKind(path) {
            | ResolvedMetadata.ALS(WASM, Some(versionInfo)) =>
              ResolvedMetadata.ALS(WASM, Some(versionInfo))
            | _ => ResolvedMetadata.ALS(WASM, None)
            }
          }
          await Memento.ResolvedMetadata.setKind(self.memento, resolved, kind)
          Some(path)
        | Error(error) =>
          let (_, errorBody) = Connection__Error.toString(Establish(error))
          switch await Candidate.resolve(PlatformOps.findCommand, candidate) {
          | Ok(resolved) =>
            await Memento.ResolvedMetadata.setError(self.memento, resolved, errorBody)
          | Error(_) => ()
          }
          Some(path)
        }
      })

      let updateResults = await Promise.all(probePromises)
      let updatedPaths = updateResults->Array.filterMap(x => x)

      Array.length(updatedPaths) > 0
    }
  }
}

// Connection switching logic
let switchAgdaVersion = async (state: State.t, selectedPath: string) => {
  let resolvedFromConnectionPath = (path: string): Connection__Candidate.Resolved.t => ({
    original: Candidate.make(selectedPath),
    resource: switch Connection__URI.parse(path) {
    | FileURI(_, uri) => uri
    },
  })

  // Show a generic "switching..." status before probing the selected candidate.
  await State__View.Panel.displayConnectionStatus(state, None)
  await State__View.Panel.display(
    state,
    AgdaModeVscode.View.Header.Plain("Switching connection..."),
    [],
  )

  switch await Connection.fromPathsOrCommands(
    state.platformDeps,
    [(selectedPath, Connection.Error.Establish.FromConfig)],
  ) {
  | Ok(conn) =>
    Util.log("[ debug ] switchAgdaVersion: connection succeeded", Connection.toString(conn))
    // Tear down the existing shared connection only after the target is proven connectable.
    await Registry__Connection.shutdown()

    // Persist explicit user selection; active connection is managed by Registry__Connection.
    await Memento.PreferredCandidate.set(state.memento, Some(selectedPath))

    // update displayed connection status
    await State__View.Panel.displayConnectionStatus(state, Some(conn))
    await State__View.Panel.display(
      state,
      AgdaModeVscode.View.Header.Success("Switched to " ++ Connection.toString(conn)),
      [],
    )
    // update memento with discovered version information and display success message
    switch conn {
    | Agda(_, path, version) =>
      let resolved = resolvedFromConnectionPath(path)
      await Memento.ResolvedMetadata.setKind(
        state.memento,
        resolved,
        ResolvedMetadata.Agda(Some(version)),
      )
    | ALS(_, path, {alsVersion: Some(v), agdaVersion, lspOptions}) =>
      let resolved = resolvedFromConnectionPath(path)
      await Memento.ResolvedMetadata.setKind(
        state.memento,
        resolved,
        ResolvedMetadata.ALS(Native, Some(v, agdaVersion, lspOptions)),
      )
    | ALS(_, _, {alsVersion: None}) => () // ALS version unknown, don't update memento
    | ALSWASM(_, _, path, {alsVersion: Some(v), agdaVersion, lspOptions}) =>
      let resolved = resolvedFromConnectionPath(path)
      await Memento.ResolvedMetadata.setKind(
        state.memento,
        resolved,
        ResolvedMetadata.ALS(WASM, Some(v, agdaVersion, lspOptions)),
      )
    | ALSWASM(_, _, _, {alsVersion: None}) => () // ALS version unknown, don't update memento
    }
    // Final cleanup: destroy the temporary connection used for version probing/switching
    // The next command will re-acquire via Registry
    let _ = await Connection.destroy(Some(conn), state.channels.log)
  | Error(error) => {
      let (errorHeader, errorBody) = Connection.Error.toString(Establish(error))
      Util.log("[ debug ] switchAgdaVersion: connection failed", errorHeader ++ " | " ++ errorBody)
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
  type variant =
    | Native
    | WASM

  let variantToTag = variant =>
    switch variant {
    | Native => "native"
    | WASM => "wasm"
    }

  let variantFromTag = tag =>
    switch tag {
    | "native" => Some(Native)
    | "wasm" => Some(WASM)
    | _ => None
    }

  let variantDisplayName = variant =>
    switch variant {
    | Native => "Agda Language Server (native)"
    | WASM => "Agda Language Server (WASM)"
    }

  let variantFileName = variant =>
    switch variant {
    | Native => "als"
    | WASM => "als.wasm"
    }

  let expectedPathForSource = (
    globalStorageUri: VSCode.Uri.t,
    source: Connection__Download.Source.t,
  ): string => Connection__Download.expectedPathForSource(globalStorageUri, source)

  let isDownloadedSource = async (
    globalStorageUri: VSCode.Uri.t,
    source: Connection__Download.Source.t,
  ): bool =>
    switch await FS.stat(Connection__Download.expectedUriForSource(globalStorageUri, source)) {
    | Ok(_) => true
    | Error(_) => false
    }

  type sourceDownloadItem = {
    downloaded: bool,
    versionString: string,
    variantTag: string,
    source: option<Connection__Download.Source.t>,
  }

  let suppressManagedVariants = async (
    globalStorageUri: VSCode.Uri.t,
    configPaths: array<string>,
    downloadItems: array<sourceDownloadItem>,
  ): array<sourceDownloadItem> => {
    let shouldKeep = async item => {
      switch item.source {
      | None => true
      | Some(source) =>
        let expectedPath = expectedPathForSource(globalStorageUri, source)
        let expectedCandidate = Candidate.make(expectedPath)
        let inConfig = configPaths->Array.some(configPath =>
          Candidate.equal(Candidate.make(configPath), expectedCandidate))
        if !inConfig {
          true
        } else {
          let fileExists =
            (await FS.stat(Connection__Download.expectedUriForSource(globalStorageUri, source)))
            ->Result.isOk
          !fileExists
        }
      }
    }
    let keeps = await Promise.all(downloadItems->Array.map(shouldKeep))
    downloadItems->Array.filterWithIndex((_, i) => Belt.Array.getExn(keeps, i))
  }

  let makeDownloadItem = async (
    state: State.t,
    variant: variant,
    source: Connection__Download.Source.t,
  ): sourceDownloadItem => {
    let downloaded = await isDownloadedSource(state.globalStorageUri, source)
    {
      downloaded,
      versionString: Connection__Download.Source.toVersionString(source),
      variantTag: variantToTag(variant),
      source: Some(source),
    }
  }

  let unavailableSourceItem = (variant: variant): sourceDownloadItem => {
    downloaded: false,
    versionString: Constants.downloadUnavailable,
    variantTag: variantToTag(variant),
    source: None,
  }

  let unavailableItem = (variant: variant): (bool, string, string) => (
    false,
    Constants.downloadUnavailable,
    variantToTag(variant),
  )

  let channelToLabel = (channel: Connection__Download.Channel.t): string =>
    switch channel {
    | LatestALS => "LatestALS"
    | DevALS => "DevALS"
    }

  let channelFromLabel = (label: string): option<Connection__Download.Channel.t> =>
    switch label {
    | "LatestALS" => Some(LatestALS)
    | "DevALS" => Some(DevALS)
    | _ => None
    }

  let getAvailableChannels = async (_platformDeps: Platform.t): array<
    Connection__Download.Channel.t,
  > => {
    [DevALS]
  }

  // Create placeholder download items to prevent UI jitter.
  let getPlaceholderDownloadItems = async (platformDeps: Platform.t): array<(
    bool,
    string,
    string,
  )> => {
    module PlatformOps = unpack(platformDeps)
    switch await PlatformOps.determinePlatform() {
    | Ok(Connection__Download__Platform.Web) => [
        (false, Constants.checkingAvailability, variantToTag(WASM)),
      ]
    | _ => [
        (false, Constants.checkingAvailability, variantToTag(Native)),
        (false, Constants.checkingAvailability, variantToTag(WASM)),
      ]
    }
  }

  // Get all available downloads
  let getAllAvailableDownloads = async (
    state: State.t,
    platformDeps: Platform.t,
    ~channel: Connection__Download.Channel.t=DevALS,
  ): array<(bool, string, string)> => {
    module PlatformOps = unpack(platformDeps)

    let allItems = switch await PlatformOps.determinePlatform() {
    | Error(_) => [unavailableSourceItem(Native), unavailableSourceItem(WASM)]
    | Ok(Connection__Download__Platform.Web) =>
      let resolver = PlatformOps.resolveDownloadChannel(channel, true)
      switch await resolver(state.memento, state.globalStorageUri, Connection__Download__Platform.Web) {
      | Error(_) => [unavailableSourceItem(WASM)]
      | Ok(Connection__Download.Source.FromURL(_, _, _)) => [unavailableSourceItem(WASM)]
      | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
        let release = descriptor.release
        let wasmAssets = Connection__DevALS.allWasmAssets(release)
        let makeSource = asset =>
          Connection__Download.Source.FromGitHub(channel, {
            Connection__Download__GitHub.DownloadDescriptor.asset: asset,
            release: release,
            saveAsFileName: descriptor.saveAsFileName,
          })
        await Promise.all(
          wasmAssets->Array.map(async asset => await makeDownloadItem(state, WASM, makeSource(asset))),
        )
      }
    | Ok(platform) =>
      let resolver = PlatformOps.resolveDownloadChannel(channel, true)
      switch await resolver(state.memento, state.globalStorageUri, platform) {
      | Error(_) => [unavailableSourceItem(Native), unavailableSourceItem(WASM)]
      | Ok(Connection__Download.Source.FromURL(_, _, _)) =>
        [unavailableSourceItem(Native), unavailableSourceItem(WASM)]
      | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
        let release = descriptor.release
        let nativeAssets = Connection__DevALS.allNativeAssetsForPlatform(release, platform)
        let wasmAssets = Connection__DevALS.allWasmAssets(release)
        let makeSource = asset =>
          Connection__Download.Source.FromGitHub(channel, {
            Connection__Download__GitHub.DownloadDescriptor.asset: asset,
            release: release,
            saveAsFileName: descriptor.saveAsFileName,
          })
        let nativeItems = await Promise.all(
          nativeAssets->Array.map(async asset =>
            await makeDownloadItem(state, Native, makeSource(asset))
          ),
        )
        let wasmItems = await Promise.all(
          wasmAssets->Array.map(async asset =>
            await makeDownloadItem(state, WASM, makeSource(asset))
          ),
        )
        Array.concat(nativeItems, wasmItems)
      }
    }

    let filteredItems = await suppressManagedVariants(
      state.globalStorageUri,
      Config.Connection.getAgdaPaths(),
      allItems,
    )
    filteredItems->Array.map(item => (item.downloaded, item.versionString, item.variantTag))
  }
}

// Event handlers module for testability and debugging
module Handler = {
  let handleDownload = async (
    state: State.t,
    platformDeps: Platform.t,
    variant: Download.variant,
    downloaded: bool,
    versionString: string,
    ~channel: Connection__Download.Channel.t=DevALS,
    ~refreshUI: option<unit => promise<unit>>=None,
  ) => {
    state.channels.log->Chan.emit(
      Log.SwitchVersionUI(SelectedDownloadAction(downloaded, versionString)),
    )

    if downloaded {
      // Derive path purely from local filesystem — no network call needed.
      // Walk releases/<tag>/<artifactDir>/ and find an artifact whose computed
      // versionString matches, with the right variant (native vs WASM).
      open Connection__Download
      let releasesUri = VSCode.Uri.joinPath(state.globalStorageUri, ["releases"])
      let found: ref<option<VSCode.Uri.t>> = ref(None)
      switch await FS.readDirectory(releasesUri) {
      | Error(_) => ()
      | Ok(tagEntries) =>
        let _ = await Promise.all(
          tagEntries->Array.map(async ((tagDirName, _)) => {
            let tagUri = VSCode.Uri.joinPath(releasesUri, [tagDirName])
            switch await FS.readDirectory(tagUri) {
            | Error(_) => ()
            | Ok(artifactEntries) =>
              artifactEntries->Array.forEach(((artifactDirName, _)) => {
                if found.contents->Option.isNone {
                  switch DownloadArtifact.parseName(artifactDirName) {
                  | None => ()
                  | Some(artifact) =>
                    let isWasm = DownloadArtifact.Platform.isWasm(artifact.platform)
                    let variantMatches = switch variant {
                    | WASM => isWasm
                    | Native => !isWasm
                    }
                    let channelTagMatches = switch channel {
                    | Channel.DevALS => artifact.releaseTag == "dev"
                    | Channel.LatestALS => artifact.releaseTag != "dev"
                    }
                    if variantMatches && channelTagMatches {
                      let vs = switch channel {
                      | Channel.DevALS =>
                        "Agda v" ++ artifact.agdaVersion ++ " Language Server (dev build)"
                      | Channel.LatestALS =>
                        "Agda v" ++ artifact.agdaVersion ++ " Language Server " ++
                        DownloadArtifact.versionLabel(artifact.releaseTag)
                      }
                      if vs == versionString {
                        // Build URI from the actual discovered directory names,
                        // not from artifact fields, to stay consistent with what's on disk.
                        let execName = DownloadArtifact.executableName(artifact)
                        let execUri = VSCode.Uri.joinPath(
                          releasesUri,
                          [tagDirName, artifactDirName, execName],
                        )
                        found := Some(execUri)
                      }
                    }
                  }
                }
              })
            }
          })
        )
      }
      switch found.contents {
      | None => ()
      | Some(execUri) =>
        // Verify the executable actually exists before adding it to config.
        switch await FS.stat(execUri) {
        | Error(_) => ()
        | Ok(_) =>
          let downloadedPath = Connection__Download.uriToPath(execUri)
          await Config.Connection.addAgdaPath(state.channels.log, downloadedPath)
          VSCode.Window.showInformationMessage(
            versionString ++ " is already downloaded",
            [],
          )->Promise.done
        }
      }
    } else {
      module PlatformOps = unpack(platformDeps)
      let onTrace = event => state.channels.log->Chan.emit(Log.DownloadTrace(event))
      let downloadResult = switch await PlatformOps.determinePlatform() {
      | Error(_) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
      | Ok(platform) =>
        let resolver = PlatformOps.resolveDownloadChannel(channel, true)
        switch await resolver(state.memento, state.globalStorageUri, platform) {
        | Error(e) => Error(e)
        | Ok(Connection__Download.Source.FromURL(_, _, _) as source) =>
          await PlatformOps.download(state.globalStorageUri, source, ~trace=onTrace)
        | Ok(Connection__Download.Source.FromGitHub(_, descriptor)) =>
          let release = descriptor.release
          let assets = switch variant {
          | Native => Connection__DevALS.allNativeAssetsForPlatform(release, platform)
          | WASM => Connection__DevALS.allWasmAssets(release)
          }
          let matchingSource = assets->Array.reduce(None, (found, asset) =>
            switch found {
            | Some(_) => found
            | None =>
              let src = Connection__Download.Source.FromGitHub(channel, {
                Connection__Download__GitHub.DownloadDescriptor.asset: asset,
                release: release,
                saveAsFileName: descriptor.saveAsFileName,
              })
              if Connection__Download.Source.toVersionString(src) == versionString {
                Some(src)
              } else {
                None
              }
            }
          )
          switch matchingSource {
          | None => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          | Some(src) => await PlatformOps.download(state.globalStorageUri, src, ~trace=onTrace)
          }
        }
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

  let handleChannelSwitch = async (
    state: State.t,
    platformDeps: Platform.t,
    _manager: SwitchVersionManager.t,
    selectedChannel: ref<Connection__Download.Channel.t>,
    channel: Connection__Download.Channel.t,
    updateUI,
  ) => {
    selectedChannel := channel
    await Memento.SelectedChannel.set(state.memento, Download.channelToLabel(channel))
    let newDownloadItems = await Download.getAllAvailableDownloads(
      state,
      platformDeps,
      ~channel,
    )
    await updateUI(newDownloadItems)
  }

  let onSelection = (
    state: State.t,
    platformDeps: Platform.t,
    manager: SwitchVersionManager.t,
    availableChannels: ref<array<Connection__Download.Channel.t>>,
    selectedChannel: ref<Connection__Download.Channel.t>,
    updateUI,
    view: View.t,
    selectedItems: array<Item.t>,
  ) => {
    let _ = (
      async () => {
        switch selectedItems[0] {
        | Some(selectedItem) =>
          Util.log("[ debug ] user selected item: " ++ selectedItem.label, "")
          switch selectedItem.data {
          | DownloadAction(_, versionString, _) when versionString == Constants.checkingAvailability =>
            ()
          | SelectOtherChannels =>
            let channelLabels = availableChannels.contents->Array.map(Download.channelToLabel)
            view.suppressHide = true
            try {
              let channelResult = await VSCode.Window.showQuickPickWithStringItems(
                VSCode.PromiseOr.make(Others(channelLabels)),
                {
                  placeHolder: "Select download channel",
                  canPickMany: false,
                },
                None,
              )
              view.suppressHide = false
              switch channelResult {
              | Some(label) =>
                switch Download.channelFromLabel(label) {
                | Some(channel) =>
                  await handleChannelSwitch(
                    state, platformDeps, manager, selectedChannel, channel, updateUI,
                  )
                  view->View.show
                | None => view->View.show
                }
              | None => view->View.show
              }
            } catch {
            | exn =>
              view.suppressHide = false
              let msg = switch exn {
              | Exn.Error(jsExn) => Exn.message(jsExn)->Option.getOr("unknown error")
              | _ => "unknown error"
              }
              Util.log("[ debug ] channel picker failed", msg)
              view->View.show
            }
          | DeleteDownloads =>
            Util.log("[ debug ] user clicked: Delete Downloads", "")
            view->View.destroy
            let cleanedDirectories = ref([])
            let failedUris = ref([])
            let deleteRoot = async (uri: VSCode.Uri.t) => {
              Util.log(
                "[ debug ] delete downloads: deleting managed directory",
                VSCode.Uri.toString(uri),
              )
              switch await FS.deleteRecursive(uri) {
              | Ok() =>
                Util.log(
                  "[ debug ] delete downloads: deleted managed directory",
                  VSCode.Uri.toString(uri),
                )
                cleanedDirectories := Array.concat(cleanedDirectories.contents, [uri])
              | Error(error) =>
                Util.log(
                  "[ debug ] delete downloads: deleteRecursive failed",
                  VSCode.Uri.toString(uri) ++ ": " ++ error,
                )
                switch await FS.stat(uri) {
                | Ok(_) =>
                  Util.log(
                    "[ debug ] delete downloads: directory still exists after failed delete",
                    VSCode.Uri.toString(uri),
                  )
                  failedUris := Array.concat(failedUris.contents, [uri])
                | Error(_) =>
                  // Missing directories are already clean from the perspective of Delete Downloads.
                  Util.log(
                    "[ debug ] delete downloads: directory already missing, treating as cleaned",
                    VSCode.Uri.toString(uri),
                  )
                  cleanedDirectories := Array.concat(cleanedDirectories.contents, [uri])
                }
              }
            }
            let managedRoots = Connection__Download.managedDeleteRoots(state.globalStorageUri)
            Util.log(
              "[ debug ] delete downloads: globalStorageUri",
              VSCode.Uri.toString(state.globalStorageUri),
            )
            Util.log(
              "[ debug ] delete downloads: managed roots",
              managedRoots->Array.map(root => VSCode.Uri.toString(root))->Array.join(" | "),
            )
            let rec deleteAllRoots = async (index: int): unit => {
              if index < Array.length(managedRoots) {
                await deleteRoot(Belt.Array.getExn(managedRoots, index))
                await deleteAllRoots(index + 1)
              }
            }
            await deleteAllRoots(0)
            // Delete orphaned in-flight temp files at globalStorageUri
            let inFlightUri = VSCode.Uri.joinPath(state.globalStorageUri, ["in-flight.download"])
            let inFlightZipUri = VSCode.Uri.joinPath(state.globalStorageUri, ["in-flight.download.zip"])
            let _ = await FS.delete(inFlightUri)
            let _ = await FS.delete(inFlightZipUri)
            // Clear cache for all repositories
            await Memento.ALSReleaseCache.clear(state.memento, "agda", "agda-language-server")
            await Memento.ALSReleaseCache.clear(state.memento, "banacorn", "agda-language-server")
            Util.log(
              "[ debug ] delete downloads: cleaned directories",
              cleanedDirectories.contents
              ->Array.map(dir => VSCode.Uri.toString(dir))
              ->Array.join(" | "),
            )
            Util.log(
              "[ debug ] delete downloads: failed uris",
              failedUris.contents->Array.map(uri => VSCode.Uri.toString(uri))->Array.join(", "),
            )
            await Memento.ResolvedMetadata.clearUnderDirectories(
              state.memento,
              cleanedDirectories.contents,
            )
            // Remove download-managed paths from connection.paths
            let currentPaths = Config.Connection.getAgdaPaths()
            Util.log(
              "[ debug ] delete downloads: current connection.paths",
              currentPaths->Array.join(" | "),
            )
            let filteredPaths = currentPaths->Array.filter(
              candidate => {
                let matchedDirectory =
                  cleanedDirectories.contents->Array.reduce(None, (found, dirUri) =>
                    switch found {
                    | Some(_) => found
                    | None =>
                      if Candidate.isUnderDirectory(Candidate.make(candidate), dirUri) {
                        Some(dirUri)
                      } else {
                        None
                      }
                    }
                  )
                switch matchedDirectory {
                | Some(dirUri) =>
                  Util.log(
                    "[ debug ] delete downloads: removing connection.path candidate",
                    candidate ++ " under " ++ VSCode.Uri.toString(dirUri),
                  )
                  false
                | None =>
                  Util.log(
                    "[ debug ] delete downloads: preserving connection.path candidate",
                    candidate,
                  )
                  true
                }
              },
            )
            Util.log(
              "[ debug ] delete downloads: filtered connection.paths",
              filteredPaths->Array.join(" | "),
            )
            await Config.Connection.setAgdaPaths(state.channels.log, filteredPaths)
            state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
            if failedUris.contents->Array.length == 0 {
              VSCode.Window.showInformationMessage(
                "All downloads and cache deleted",
                [],
              )->Promise.done
            } else {
              VSCode.Window.showWarningMessage(
                "Some downloads could not be deleted: " ++ failedUris.contents->Array.map(uri => VSCode.Uri.toString(uri))->Array.join(", "),
                [],
              )->Promise.done
            }
          | DownloadAction(downloaded, versionString, variantTag) =>
            Util.log("[ debug ] user clicked: download button = " ++ selectedItem.label, "")
            view->View.destroy
            let selectedVariant =
              Download.variantFromTag(variantTag)->Option.getOr(Download.Native)

            let currentChannel = selectedChannel.contents
            await handleDownload(
              state,
              platformDeps,
              selectedVariant,
              downloaded,
              versionString,
              ~channel=currentChannel,
            )
          | Candidate(selectedPath, _detail, entry, _) =>
            Util.log("[ debug ] user selected kind: " ++ selectedPath, "")
            view->View.destroy
            // Regular candidate selection - check if selection changed
            let changed = switch Memento.PreferredCandidate.get(manager.memento) {
            | Some(path) => !Candidate.equal(Candidate.make(selectedPath), Candidate.make(path))
            | None => true // If no previous selection, treat as changed
            }
            if changed {
              state.channels.log->Chan.emit(
                Log.SwitchVersionUI(SelectedCandidate(selectedPath, entry, true)),
              )
              await switchAgdaVersion(state, selectedPath)
              state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
            } else {
              // No change in selection - still emit completion
              state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
            }
          | NoInstallations | Separator(_) =>
            state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
          }
        | None =>
          view->View.destroy
          state.channels.log->Chan.emit(Log.SwitchVersionUI(SelectionCompleted))
        }
      }
    )()
  }

  let onHide = (view: View.t) => {
    if view.suppressHide {
      ()
    } else {
      Util.log("[ debug ] QuickPick hidden/cancelled by user", "")
      // QuickPick was hidden/cancelled by user - clean up
      view->View.destroy
    }
  }

  let backgroundUpdateFailureFallback = async (
    platformDeps: Platform.t,
    updateUI: array<(bool, string, string)> => promise<unit>,
  ): unit => {
    module PlatformOps = unpack(platformDeps)
    let fallback = switch await PlatformOps.determinePlatform() {
    | Ok(Connection__Download__Platform.Web) => [Download.unavailableItem(WASM)]
    | _ => [Download.unavailableItem(Native), Download.unavailableItem(WASM)]
    }
    try {
      await updateUI(fallback)
    } catch {
    | _ => ()
    }
  }

  let runBackgroundUpdate = async (
    downloadItemsPromise: promise<array<(bool, string, string)>>,
    platformDeps: Platform.t,
    manager: SwitchVersionManager.t,
    updateUI: array<(bool, string, string)> => promise<unit>,
  ): unit => {
    try {
      let downloadItems = await downloadItemsPromise
      let phase3Changed = await SwitchVersionManager.probeVersions(manager, platformDeps)
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

  let onActivate = async (
    state: State.t,
    platformDeps: Platform.t,
    ~downloadItemsPromiseOverride: option<promise<array<(bool, string, string)>>>=None,
  ) => {
    let manager = SwitchVersionManager.make(state)
    let view = View.make(state.channels.log)
    let availableChannels = ref([Connection__Download.Channel.DevALS])
    let restoredChannel = switch Memento.SelectedChannel.get(state.memento) {
    | Some(label) => Download.channelFromLabel(label)->Option.getOr(Connection__Download.Channel.DevALS)
    | None => Connection__Download.Channel.DevALS
    }
    let selectedChannel = ref(restoredChannel)

    // Helper function to update UI with current state
    let updateUI = async (downloadItems: array<(bool, string, string)>): unit => {
      let downloadHeader =
        "Download (channel: " ++ Download.channelToLabel(selectedChannel.contents) ++ ")"
      let itemData = await SwitchVersionManager.getItemData(
        manager,
        downloadItems,
        ~downloadHeader,
        ~platformDeps=Some(platformDeps),
      )

      // Log selection marking for testing observability
      let candidateItemDatas = itemData->Array.filterMap(item =>
        switch item {
        | Candidate(path, _, entry, isSelected) => Some(path, entry.kind, entry.error, isSelected)
        | _ => None
        }
      )
      state.channels.log->Chan.emit(Log.SwitchVersionUI(UpdatedCandidates(candidateItemDatas)))
      state.channels.log->Chan.emit(Log.SwitchVersionUI(Others(downloadHeader)))
      state.channels.log->Chan.emit(Log.SwitchVersionUI(UpdatedDownloadItems(downloadItems)))

      let items = Item.fromItemDataArray(itemData, state.extensionUri)
      view->View.updateItems(items)
    }

    // Setup quickpick
    view->View.setPlaceholder("Switch Agda Version")

    availableChannels := (await Download.getAvailableChannels(platformDeps))

    // Clamp restored channel to available channels
    if !(availableChannels.contents->Array.includes(selectedChannel.contents)) {
      selectedChannel := availableChannels.contents->Array.get(0)->Option.getOr(Connection__Download.Channel.DevALS)
    }

    // PHASE 1: Show cached items immediately with placeholders to prevent jitter
    await updateUI(await Download.getPlaceholderDownloadItems(platformDeps))
    view->View.show
    state.channels.log->Chan.emit(SwitchVersionUI(Others("QuickPick shown")))

    // Get download info asynchronously in background
    let downloadItemsPromise = switch downloadItemsPromiseOverride {
    | Some(override) => override
    | None => Download.getAllAvailableDownloads(state, platformDeps, ~channel=selectedChannel.contents)
    }

    // Setup event handlers
    view->View.onSelection(selectedItems =>
      onSelection(
        state,
        platformDeps,
        manager,
        availableChannels,
        selectedChannel,
        updateUI,
        view,
        selectedItems,
      )
    )
    view->View.onHide(() => onHide(view))

    // Start background update
    let _ = runBackgroundUpdate(downloadItemsPromise, platformDeps, manager, updateUI)
  }
}

// Main entry point
let activate = Handler.onActivate
