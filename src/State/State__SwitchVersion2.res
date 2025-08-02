// Lazy-loading version switching module with cached endpoint display

module ItemFormatting = {
  type endpointInfo = {
    endpoint: Memento.Endpoints.endpoint,
    error: option<string>,
  }

  // Format endpoint display information
  let formatEndpointInfo = (filename: string, endpointInfo: endpointInfo, isPicked: bool) => {
    let (baseLabel, baseDescription) = switch (endpointInfo.endpoint, endpointInfo.error) {
    | (Agda(Some(version)), _) => 
      ("Agda", "v" ++ version)
    | (Agda(None), _) =>
      ("Agda", "version unknown")
    | (ALS(Some((alsVersion, agdaVersion))), _) => 
      ("$(squirrel)  ALS", "v" ++ alsVersion ++ ", Agda v" ++ agdaVersion)
    | (ALS(None), _) =>
      ("$(squirrel)  ALS", "version unknown")
    | (Unknown, Some(error)) =>
      ("$(error) " ++ filename, "Error: " ++ error)
    | (Unknown, None) => 
      ("$(question) " ++ filename, "Unknown executable")
    }
    
    // Add "Selected" suffix if this is the picked connection
    let description = isPicked ? baseDescription ++ " (Selected)" : baseDescription
    (baseLabel, description)
  }

  // Determine if endpoint should have an icon
  let shouldHaveIcon = (endpoint: Memento.Endpoints.endpoint): bool => {
    switch endpoint {
    | Agda(_) => true
    | _ => false
    }
  }

  // Format endpoint from entry
  let formatEndpoint = (filename: string, entry: Memento.Endpoints.entry, isPicked: bool) => {
    formatEndpointInfo(filename, {endpoint: entry.endpoint, error: entry.error}, isPicked)
  }
}

module ItemCreation = {

  // Format endpoint information for display with picked status
  let formatEndpoint = (filename: string, entry: Memento.Endpoints.entry, isPicked: bool) => {
    ItemFormatting.formatEndpoint(filename, entry, isPicked)
  }

  // Create quickpick item from endpoint entry with picked status
  let createEndpointItem = (path: string, entry: Memento.Endpoints.entry, extensionUri: VSCode.Uri.t, isPicked: bool): VSCode.QuickPickItem.t => {
    let filename = NodeJs.Path.basename(path)
    let (label, description) = formatEndpoint(filename, entry, isPicked)
    
    // Add Agda icon for Agda endpoints
    let iconPath = if ItemFormatting.shouldHaveIcon(entry.endpoint) {
      Some(VSCode.IconPath.fromDarkAndLight({
        "dark": VSCode.Uri.joinPath(extensionUri, ["asset/dark.png"]),
        "light": VSCode.Uri.joinPath(extensionUri, ["asset/light.png"]),
      }))
    } else {
      None
    }
    
    let baseItem: VSCode.QuickPickItem.t = {
      label: label,
      description: description,
      detail: path,
    }
    
    switch iconPath {
    | Some(icon) => {
        ...baseItem,
        iconPath: icon,
      }
    | None => baseItem
    }
  }

  // Create separator item
  let createSeparatorItem = (label: string): VSCode.QuickPickItem.t => {
    {
      label: label,
      kind: VSCode.QuickPickItemKind.Separator,
    }
  }

  // Create "no installations" placeholder item
  let createNoInstallationsItem = (): VSCode.QuickPickItem.t => {
    {
      label: "$(info) No installations found",
      description: "Try installing Agda or ALS first",
      detail: "No executable paths detected",
    }
  }
}

module QuickPickManager = {
  type t = {
    quickPick: VSCode.QuickPick.t<VSCode.QuickPickItem.t>,
    subscriptions: array<VSCode.Disposable.t>,
    mutable items: array<VSCode.QuickPickItem.t>,
  }

  let make = (): t => {
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
  }
}

module EndpointLogic = {
  // Determine picked connection
  let getPickedPath = (memento: Memento.t): option<string> => {
    Memento.PickedConnection.get(memento)
  }

  // Convert entries to items
  let entriesToItems = (
    entries: Dict.t<Memento.Endpoints.entry>,
    extensionUri: VSCode.Uri.t,
    pickedPath: option<string>
  ): array<VSCode.QuickPickItem.t> => {
    let pathItems = 
      entries
      ->Dict.toArray
      ->Array.map(((path, entry)) => {
        let isPicked = switch pickedPath {
        | Some(picked) => picked == path
        | None => false
        }
        ItemCreation.createEndpointItem(path, entry, extensionUri, isPicked)
      })

    if Array.length(pathItems) > 0 {
      Array.concat([ItemCreation.createSeparatorItem("Installed")], pathItems)
    } else {
      [ItemCreation.createNoInstallationsItem()]
    }
  }

  // Get paths that need version probing
  let getPathsNeedingProbe = (entries: Dict.t<Memento.Endpoints.entry>): array<string> => {
    entries
    ->Dict.toArray
    ->Array.filterMap(((path, entry)) => {
      switch entry.endpoint {
      | Agda(None) | ALS(None) => Some(path)
      | _ => None
      }
    })
  }

  // Check if entries changed
  let entriesChanged = (oldEntries: Dict.t<Memento.Endpoints.entry>, newEntries: Dict.t<Memento.Endpoints.entry>): bool => {
    newEntries !== oldEntries
  }
}

module EndpointManager = {
  // Single source of truth for endpoint entries
  type t = {
    mutable entries: Dict.t<Memento.Endpoints.entry>,
    extensionUri: VSCode.Uri.t,
  }
  
  let make = (state: State.t): t => {
    entries: Memento.Endpoints.entries(state.memento),
    extensionUri: state.extensionUri,
  }
  
  // Convert current entries to quickpick items with picked connection marking
  let toItems = (self: t, memento: Memento.t): array<VSCode.QuickPickItem.t> => {
    let pickedPath = EndpointLogic.getPickedPath(memento)
    EndpointLogic.entriesToItems(self.entries, self.extensionUri, pickedPath)
  }
  
  
  // Update items with visual marking of picked connection
  let updateItems = (self: t, qp: QuickPickManager.t, memento: Memento.t): unit => {
    let items = toItems(self, memento)
    qp->QuickPickManager.updateItems(items)
  }
  
  // Update entries from memento and return whether anything changed
  let refreshFromMemento = (self: t, memento: Memento.t): bool => {
    let newEntries = Memento.Endpoints.entries(memento)
    let changed = EndpointLogic.entriesChanged(self.entries, newEntries)
    if changed {
      self.entries = newEntries
    }
    changed
  }
  
  // Phase 2: Discover and sync filesystem paths
  let syncWithFilesystem = async (self: t, state: State.t, platformDeps: Platform.t): bool => {
    module PlatformOps = unpack(platformDeps)
    
    // Discover new endpoints with inferred types
    let discoveredEndpoints = await PlatformOps.getInstalledEndpointsAndPersistThem2(state.globalStorageUri)
    
    // Sync cache with discovered endpoints
    await Memento.Endpoints.syncWithPaths(state.memento, discoveredEndpoints)
    
    // Update our local entries and return if changed
    refreshFromMemento(self, state.memento)
  }
  
  // Phase 3: Probe for version information
  let probeVersions = async (self: t, state: State.t): bool => {
    // Get paths that need version probing
    let pathsToProbe = EndpointLogic.getPathsNeedingProbe(self.entries)
    
    if Array.length(pathsToProbe) == 0 {
      false // Nothing to probe
    } else {
      // Probe endpoints in parallel
      let probePromises = pathsToProbe
        ->Array.map(async path => {
          let uri = Connection__URI.parse(path)
          switch await Connection__Endpoint.probeFilepath(uri) {
          | Ok(Connection__Endpoint.Agda(version, _)) => 
            await Memento.Endpoints.setVersion(state.memento, path, Memento.Endpoints.Agda(Some(version)))
            Some(path)
          | Ok(Connection__Endpoint.ALS(alsVersion, agdaVersion, _, _)) =>
            await Memento.Endpoints.setVersion(state.memento, path, Memento.Endpoints.ALS(Some((alsVersion, agdaVersion))))
            Some(path)
          | Error(error) =>
            await Memento.Endpoints.setError(state.memento, path, Connection__Endpoint.Error.toString(error))
            Some(path)
          }
        })
      
      let updateResults = await Promise.all(probePromises)
      let updatedPaths = updateResults->Array.filterMap(x => x)
      
      if Array.length(updatedPaths) > 0 {
        // Refresh our entries from memento
        let _ = refreshFromMemento(self, state.memento)
        true
      } else {
        false
      }
    }
  }
}

// Main entry point
let run = async (state: State.t, platformDeps: Platform.t) => {
  let qp = QuickPickManager.make()
  let endpointManager = EndpointManager.make(state)
  
  // Setup quickpick
  qp->QuickPickManager.setPlaceholder("Switch Version (v2)")
  
  // PHASE 1: Show cached items immediately with visual marking
  endpointManager->EndpointManager.updateItems(qp, state.memento)
  qp->QuickPickManager.show
  
  // Setup event handlers
  qp->QuickPickManager.onSelection(selectedItems => {
    qp->QuickPickManager.destroy
    
    // Save the user's selection to PickedConnection
    switch selectedItems[0] {
    | Some(selectedItem) => {
      switch selectedItem.detail {
      | Some(selectedPath) => {
        let _ = Memento.PickedConnection.set(state.memento, Some(selectedPath))
      }
      | None => ()
      }
    }
    | None => ()
    }
  })
  
  qp->QuickPickManager.onHide(() => {
    qp->QuickPickManager.destroy
  })
  
  // Background update process (sequential phases)
  let backgroundUpdate = async () => {
    try {
      // PHASE 2: Sync with filesystem (discover new paths)
      let phase2Changed = await endpointManager->EndpointManager.syncWithFilesystem(state, platformDeps)
      if phase2Changed {
        endpointManager->EndpointManager.updateItems(qp, state.memento)
      }
      
      // PHASE 3: Probe version information
      let phase3Changed = await endpointManager->EndpointManager.probeVersions(state)
      if phase3Changed {
        endpointManager->EndpointManager.updateItems(qp, state.memento)
      }
    } catch {
    | _exn => () // Ignore background update errors
    }
  }
  
  // Start background update
  let _ = backgroundUpdate()
}