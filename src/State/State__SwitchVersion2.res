// Lazy-loading version switching module with cached endpoint display

module ItemCreation = {
  // Infer endpoint type from filename
  let inferEndpointType = (filename: string) => {
    let baseName = filename->String.toLowerCase->NodeJs.Path.basename
    // Remove common executable extensions
    let cleanName = baseName
      ->String.replace(".exe", "")
      ->String.replace(".cmd", "")
      ->String.replace(".bat", "")
    
    if cleanName == "agda" || cleanName->String.startsWith("agda-") {
      Some(#Agda)
    } else if cleanName == "als" || cleanName->String.startsWith("als-") {
      Some(#ALS)  
    } else {
      None
    }
  }

  // Format endpoint information for display
  let formatEndpoint = (filename: string, entry: Memento.Endpoints.entry) => {
    switch (entry.endpoint, entry.error) {
    | (Agda(Some(version)), _) => 
      ("Agda", "Agda v" ++ version)
    | (Agda(None), _) =>
      ("Agda", "version unknown")
    | (ALS(Some((alsVersion, agdaVersion))), _) => 
      ("$(squirrel)  ALS", "ALS v" ++ alsVersion ++ ", Agda v" ++ agdaVersion)
    | (ALS(None), _) =>
      ("$(squirrel)  ALS", "version unknown")
    | (Unknown, Some(error)) =>
      ("$(error) " ++ filename, "Error: " ++ error)
    | (Unknown, None) => 
      // No cached info - infer from filename for better decoration
      switch inferEndpointType(filename) {
      | Some(#Agda) => ("Agda", "version unknown")
      | Some(#ALS) => ("$(squirrel)  ALS", "version unknown")
      | None => ("$(question) " ++ filename, "Unknown executable")
      }
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
    | _ => 
      // Also add icon for inferred Agda endpoints
      switch inferEndpointType(filename) {
      | Some(#Agda) => Some(VSCode.IconPath.fromDarkAndLight({
          "dark": VSCode.Uri.joinPath(extensionUri, ["asset/dark.png"]),
          "light": VSCode.Uri.joinPath(extensionUri, ["asset/light.png"]),
        }))
      | _ => None
      }
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

module EndpointSync = {
  // Convert endpoint entries to quickpick items
  let entriesToItems = (endpointEntries: Dict.t<Memento.Endpoints.entry>, extensionUri: VSCode.Uri.t): array<VSCode.QuickPickItem.t> => {
    let pathItems = 
      endpointEntries
      ->Dict.toArray
      ->Array.map(((path, entry)) => ItemCreation.createEndpointItem(path, entry, extensionUri))

    if Array.length(pathItems) > 0 {
      Array.concat([ItemCreation.createSeparatorItem("Installed")], pathItems)
    } else {
      [ItemCreation.createNoInstallationsItem()]
    }
  }

  // Sync cache with filesystem and return updated items
  let syncAndGetItems = async (state: State.t, platformDeps: Platform.t): array<VSCode.QuickPickItem.t> => {
    module PlatformOps = unpack(platformDeps)
    
    // Discover new paths
    let installedPaths = await PlatformOps.getInstalledEndpointsAndPersistThem2(state.globalStorageUri)
    
    // Sync cache with discovered paths
    await Memento.Endpoints.syncWithPaths(state.memento, installedPaths)
    
    // Return updated items
    let updatedEntries = Memento.Endpoints.entries(state.memento)
    entriesToItems(updatedEntries, state.extensionUri)
  }
}

// Main entry point
let run = async (state: State.t, platformDeps: Platform.t) => {
  let qp = QuickPickManager.make()
  
  // Setup quickpick
  qp->QuickPickManager.setPlaceholder("Switch Version (v2)")
  
  // PHASE 1: Show cached items immediately (instant UX)
  let cachedEntries = Memento.Endpoints.entries(state.memento)
  let initialItems = EndpointSync.entriesToItems(cachedEntries, state.extensionUri)
  qp->QuickPickManager.updateItems(initialItems)
  qp->QuickPickManager.show
  
  // Setup event handlers
  qp->QuickPickManager.onSelection(_selectedItems => {
    qp->QuickPickManager.destroy
  })
  
  qp->QuickPickManager.onHide(() => {
    qp->QuickPickManager.destroy
  })
  
  // PHASE 2: Update with newly discovered paths (background sync)
  let backgroundUpdate = async () => {
    let updatedItems = await EndpointSync.syncAndGetItems(state, platformDeps)
    qp->QuickPickManager.updateItems(updatedItems)
  }
  
  // Start background update
  backgroundUpdate()->ignore
}