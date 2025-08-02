// Lazy-loading version switching module with cached endpoint display

module ItemCreation = {

  // Format endpoint information for display
  let formatEndpoint = (filename: string, entry: Memento.Endpoints.entry) => {
    switch (entry.endpoint, entry.error) {
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
  }

  // Create quickpick item from endpoint entry
  let createEndpointItem = (path: string, entry: Memento.Endpoints.entry, extensionUri: VSCode.Uri.t): VSCode.QuickPickItem.t => {
    let filename = NodeJs.Path.basename(path)
    let (label, description) = formatEndpoint(filename, entry)
    
    // Add Agda icon for Agda endpoints (like v1)
    let iconPath = switch entry.endpoint {
    | Agda(_) => Some(VSCode.IconPath.fromDarkAndLight({
        "dark": VSCode.Uri.joinPath(extensionUri, ["asset/dark.png"]),
        "light": VSCode.Uri.joinPath(extensionUri, ["asset/light.png"]),
      }))
    | _ => None
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
  
  // Convert current entries to quickpick items
  let toItems = (self: t): array<VSCode.QuickPickItem.t> => {
    let pathItems = 
      self.entries
      ->Dict.toArray
      ->Array.map(((path, entry)) => ItemCreation.createEndpointItem(path, entry, self.extensionUri))

    if Array.length(pathItems) > 0 {
      Array.concat([ItemCreation.createSeparatorItem("Installed")], pathItems)
    } else {
      [ItemCreation.createNoInstallationsItem()]
    }
  }
  
  // Update entries from memento and return whether anything changed
  let refreshFromMemento = (self: t, memento: Memento.t): bool => {
    let newEntries = Memento.Endpoints.entries(memento)
    let changed = newEntries !== self.entries
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
    let pathsToProbe = self.entries
      ->Dict.toArray
      ->Array.filterMap(((path, entry)) => {
        switch entry.endpoint {
        | Agda(None) | ALS(None) => Some(path)
        | _ => None
        }
      })
    
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
  
  // PHASE 1: Show cached items immediately (instant UX)
  let initialItems = endpointManager->EndpointManager.toItems
  qp->QuickPickManager.updateItems(initialItems)
  qp->QuickPickManager.show
  
  // Setup event handlers
  qp->QuickPickManager.onSelection(_selectedItems => {
    qp->QuickPickManager.destroy
  })
  
  qp->QuickPickManager.onHide(() => {
    qp->QuickPickManager.destroy
  })
  
  // Background update process (sequential phases for safety)
  let backgroundUpdate = async () => {
    try {
      // PHASE 2: Sync with filesystem (discover new paths)
      let phase2Changed = await endpointManager->EndpointManager.syncWithFilesystem(state, platformDeps)
      if phase2Changed {
        let updatedItems = endpointManager->EndpointManager.toItems
        qp->QuickPickManager.updateItems(updatedItems)
      }
      
      // PHASE 3: Probe version information (only after phase 2 completes)
      let phase3Changed = await endpointManager->EndpointManager.probeVersions(state)
      if phase3Changed {
        let finalItems = endpointManager->EndpointManager.toItems
        qp->QuickPickManager.updateItems(finalItems)
      }
    } catch {
    | exn => 
      // Log error but don't crash the UI
      Js.log("Background update failed: " ++ Js.String.make(exn))
    }
  }
  
  // Start background update
  backgroundUpdate()->ignore
}