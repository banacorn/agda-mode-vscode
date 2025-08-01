// Simplified version switching module without connection endpoints
// Just shows a dummy quickpick item

module QP = {
  type t = {
    quickPick: VSCode.QuickPick.t<VSCode.QuickPickItem.t>,
    subscriptions: array<VSCode.Disposable.t>,
  }

  let make = () => {
    quickPick: VSCode.Window.createQuickPick(),
    subscriptions: [],
  }

  let render = self => {
    self.quickPick->VSCode.QuickPick.show
  }

  let destroy = self => {
    self.quickPick->VSCode.QuickPick.dispose
    self.subscriptions->Array.forEach(sub => sub->VSCode.Disposable.dispose)
  }
}

let run = async (state: State.t, platformDeps: Platform.t) => {
  let qp = QP.make()

  // Set placeholder
  qp.quickPick->VSCode.QuickPick.setPlaceholder("Switch Version (v2)")

  // Fetch installed paths using the new function
  module PlatformOps = unpack(platformDeps)
  let installedPaths = await PlatformOps.getInstalledEndpointsAndPersistThem2(
    state.globalStorageUri,
  )

  // Sync Memento.Endpoints with discovered paths
  await Memento.Endpoints.syncWithPaths(state.memento, installedPaths)

  // Get the synced endpoint entries
  let endpointEntries = Memento.Endpoints.entries(state.memento)

  // Convert cached entries to quickpick items
  let pathItems =
    endpointEntries
    ->Dict.toArray
    ->Array.map(((path, entry)) => {
      // Extract just the filename for the label
      let filename = NodeJs.Path.basename(path)
      
      // Create description based on cached endpoint info
      let (label, description) = switch entry.endpoint {
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
      
      let item: VSCode.QuickPickItem.t = {
        label: label,
        description: description,
        detail: path,
      }
      item
    })

  // Add separator and items if we have paths
  let items = if Array.length(pathItems) > 0 {
    let separator: VSCode.QuickPickItem.t = {
      label: "Installed",
      kind: VSCode.QuickPickItemKind.Separator,
    }
    Array.concat([separator], pathItems)
  } else {
    let noPathsItem: VSCode.QuickPickItem.t = {
      label: "$(info) No installations found",
      description: "Try installing Agda or ALS first",
      detail: "No executable paths detected",
    }
    [noPathsItem]
  }

  // Set items
  qp.quickPick->VSCode.QuickPick.setItems(items)

  // Show the quickpick
  qp->QP.render

  // Handle selection - just close on any selection
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(_selectedItems => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)

  // Handle hide event
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
}
