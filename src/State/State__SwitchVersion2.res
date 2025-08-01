// Lazy-loading version switching module with cached endpoint display

module ItemCreation = {
  // Format endpoint information for display
  let formatEndpoint = (filename: string, entry: Memento.Endpoints.entry) => {
    switch entry.endpoint {
    | Some(Agda(version)) => 
      ("$(file-binary) " ++ filename, "Agda v" ++ version)
    | Some(ALS(alsVersion, agdaVersion)) => 
      ("$(squirrel) " ++ filename, "ALS v" ++ alsVersion ++ ", Agda v" ++ agdaVersion)
    | None => 
      switch entry.error {
      | Some(error) => ("$(error) " ++ filename, "Error: " ++ error)
      | None => ("$(file-binary) " ++ filename, "Unknown version (not probed yet)")
      }
    }
  }

  // Create quickpick item from endpoint entry
  let createEndpointItem = (path: string, entry: Memento.Endpoints.entry): VSCode.QuickPickItem.t => {
    let filename = NodeJs.Path.basename(path)
    let (label, description) = formatEndpoint(filename, entry)
    
    {
      label: label,
      description: description,
      detail: path,
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
  let entriesToItems = (endpointEntries: Dict.t<Memento.Endpoints.entry>): array<VSCode.QuickPickItem.t> => {
    let pathItems = 
      endpointEntries
      ->Dict.toArray
      ->Array.map(((path, entry)) => ItemCreation.createEndpointItem(path, entry))

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
    entriesToItems(updatedEntries)
  }
}

// Main entry point
let run = async (state: State.t, platformDeps: Platform.t) => {
  let qp = QuickPickManager.make()
  
  // Setup quickpick
  qp->QuickPickManager.setPlaceholder("Switch Version (v2)")
  
  // PHASE 1: Show cached items immediately (instant UX)
  let cachedEntries = Memento.Endpoints.entries(state.memento)
  let initialItems = EndpointSync.entriesToItems(cachedEntries)
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