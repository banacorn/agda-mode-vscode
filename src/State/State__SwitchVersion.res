// Plan: Lazy Version Probing for State__SwitchVersion

//   Phase 1: Modify QuickPick Item Creation

//   1. Change endpointToItem function (line 322):
//     - Instead of requiring full endpoint info with version, accept just URI + cached version info
//     - Check Memento.EndpointVersion.get() for cached version data
//     - Show version if cached, otherwise show just the URI path
//   2. Update item creation functions:
//     - createAgdaItem: Accept optional version, show "Unknown version" if not cached
//     - createALSItem: Accept optional versions, show "Unknown version" if not cached
//     - Add visual indicator (e.g., different icon/description) for uncached vs cached endpoints

//   Phase 2: Replace Eager Probing

//   1. Remove line 356: await PlatformOps.getInstalledEndpointsAndPersistThem()
//   2. Replace with: Get raw URIs from Config.Connection.getAgdaPaths()
//   3. For each URI: Check Memento.EndpointVersion.get() for cached info
//   4. Create items: Show cached version info or "probe needed" placeholder

//   Phase 3: Implement Lazy Probing on Selection

//   1. Modify handleSelection (line 257):
//     - When user selects an endpoint, check if version is cached
//     - If not cached: probe the endpoint using Connection.Endpoint.fromVSCodeUri()
//     - Cache success: Memento.EndpointVersion.setVersion()
//     - Cache failure: Memento.EndpointVersion.setError()
//     - Update the connection after probing
//   2. Add progress indication:
//     - Show "Probing endpoint..." message during version detection
//     - Handle probe failures gracefully

//   Phase 4: Update Rendering Logic

//   1. Add refreshItems function: Regenerate quickpick items using latest cache
//   2. Call after probing: Update quickpick to show newly discovered version info
//   3. Handle errors: Show error items for endpoints that failed to probe

//   Benefits:

//   - Faster initial load: No upfront probing delays
//   - Better UX: Immediate quickpick display, progressive enhancement
//   - Caching: Avoid re-probing known endpoints
//   - Error resilience: Don't block UI on individual endpoint failures

//   Files to modify:

//   - src/State/State__SwitchVersion.res (main changes)
//   - Potentially update ItemCreation module for optional version display

module VersionDisplay = {
  // Format version strings for display
  let formatAgdaVersion = (version: string): string => "Agda v" ++ version

  let formatALSVersion = (alsVersion: string, agdaVersion: string): string =>
    "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion

  let formatSwitchingMessage = (version: string): string => "Switching to " ++ version

  let formatSwitchedMessage = (version: string): string => "Switched to " ++ version
}

module SelectionParsing = {
  // Parse selection types from quick pick items
  type selectionType =
    | OpenFolder
    | DownloadLatestALS
    | SwitchToEndpoint(string)

  let parseSelection = (label: string, detail: option<string>): selectionType =>
    switch label {
    | "$(folder-opened)  Open download folder" => OpenFolder
    | "$(cloud-download)  Download the latest Agda Language Server" => DownloadLatestALS
    | _ =>
      switch detail {
      | Some(path) => SwitchToEndpoint(path)
      | None => SwitchToEndpoint("")
      }
    }
}

module DownloadWorkflow = {
  // Download result type
  type downloadResult =
    | Success(Connection.Endpoint.t, bool) // endpoint, was already downloaded
    | Failure(string) // error message

  // Check if ALS is already downloaded and download if needed
  let downloadLatestALS = async (
    platformDeps: Platform.t,
    memento: Memento.t,
    globalStorageUri: VSCode.Uri.t,
  ) => {
    module PlatformOps = unpack(platformDeps)
    switch await PlatformOps.determinePlatform() {
    | Error(_) =>
      Failure("Failed to determine the platform for downloading the Agda Language Server")
    | Ok(platform) =>
      switch await PlatformOps.alreadyDownloaded(globalStorageUri)() {
      | None =>
        switch await PlatformOps.downloadLatestALS(memento, globalStorageUri)(platform) {
        | Error(error) => Failure(AgdaModeVscode.Connection__Download.Error.toString(error))
        | Ok(endpoint) => Success(endpoint, false) // false means it was not already downloaded
        }
      | Some(endpoint) => Success(endpoint, true) // true means it was already downloaded
      }
    }
  }
  // Show appropriate message based on download result
  let handleDownloadResult = async (result: downloadResult, rerender: unit => promise<unit>) => {
    switch result {
    | Failure(message) =>
      let _ = await VSCode.Window.showErrorMessage(message, [])
    | Success(endpoint, alreadyDownloaded) =>
      let version = switch endpoint {
      | Agda(version, _) => VersionDisplay.formatAgdaVersion(version)
      | ALS(alsVersion, agdaVersion, _, _) =>
        VersionDisplay.formatALSVersion(alsVersion, agdaVersion)
      }
      if alreadyDownloaded {
        let _ = await VSCode.Window.showInformationMessage(version ++ " is already downloaded", [])
      } else {
        // rerender the quick pick so that the downloaded endpoint is shown as installed
        await rerender()
        let _ = await VSCode.Window.showInformationMessage(
          version ++ " successfully downloaded",
          [],
        )
      }
    }
  }
}

module ItemCreation = {
  // Create QuickPick items for various endpoints and operations
  let createSeparatorItem = (label: string): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label,
    kind: VSCode.QuickPickItemKind.Separator,
  }

  let createFolderItem = (): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: "$(folder-opened)  Open download folder",
    description: "Where the language servers are downloaded to",
  }

  let createDownloadItem = (downloaded: bool, versionString: string): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: "$(cloud-download)  Download the latest Agda Language Server",
    description: if downloaded {
      "Downloaded and installed"
    } else {
      ""
    },
    detail: versionString,
  }

  let createAgdaItem = (
    version: string,
    path: string,
    isSelected: bool,
    extensionUri: VSCode.Uri.t,
  ): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: VersionDisplay.formatAgdaVersion(version),
    description: if isSelected {
      "Selected"
    } else {
      ""
    },
    detail: path,
    iconPath: VSCode.IconPath.fromDarkAndLight({
      "dark": VSCode.Uri.joinPath(extensionUri, ["asset/dark.png"]),
      "light": VSCode.Uri.joinPath(extensionUri, ["asset/light.png"]),
    }),
  }

  let createALSItem = (
    alsVersion: string,
    agdaVersion: string,
    method: Connection__Transport.t,
    isSelected: bool,
  ): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: "$(squirrel)  " ++
    VersionDisplay.formatALSVersion(alsVersion, agdaVersion),
    description: if isSelected {
      "Selected"
    } else {
      ""
    },
    detail: switch method {
    | ViaTCP(_, url) => url.toString()
    | ViaPipe(path, _) => path
    },
  }

  let createErrorItem = (error: Connection__Endpoint.Error.t): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: "$(error)  Bad path",
    description: "",
    detail: Connection__Endpoint.Error.toString(error),
  }
}

let openGlobalStorageFolder = async (state: State.t) => {
  let _result = await VSCode.Env.openExternal(state.globalStorageUri)
}

let switchAgdaVersion = async (state: State.t) => {
  // display what we are going to do
  switch await Connection.Endpoint.getPicked(state.memento, Config.Connection.getAgdaPaths()) {
  | Error(_) => ()
  | Ok(Agda(version, _)) => {
      await State__View.Panel.displayStatus(state, "")
      await State__View.Panel.display(
        state,
        View.Header.Plain(
          VersionDisplay.formatSwitchingMessage(VersionDisplay.formatAgdaVersion(version)),
        ),
        [],
      )
    }
  | Ok(ALS(alsVersion, agdaVersion, _, _)) => {
      await State__View.Panel.displayStatus(state, "")
      await State__View.Panel.display(
        state,
        View.Header.Plain(
          VersionDisplay.formatSwitchingMessage(
            VersionDisplay.formatALSVersion(alsVersion, agdaVersion),
          ),
        ),
        [],
      )
    }
  }

  // stop the old connection
  let _ = await state.connection->Connection.destroy

  // start with the new connection
  switch await Connection.make(
    state.platformDeps,
    state.memento,
    state.globalStorageUri,
    Config.Connection.getAgdaPaths(),
    ["als", "agda"],
  ) {
  | Ok(conn) =>
    state.connection = Some(conn)
    switch await Connection.Endpoint.getPicked(state.memento, Config.Connection.getAgdaPaths()) {
    | Error(_) => ()
    | Ok(Agda(version, _path)) => {
        let formattedVersion = VersionDisplay.formatAgdaVersion(version)
        await State__View.Panel.displayStatus(state, formattedVersion)
        await State__View.Panel.display(
          state,
          View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
          [],
        )
      }
    | Ok(ALS(alsVersion, agdaVersion, _, _)) => {
        let formattedVersion = VersionDisplay.formatALSVersion(alsVersion, agdaVersion)
        await State__View.Panel.displayStatus(state, formattedVersion)
        await State__View.Panel.display(
          state,
          View.Header.Success(VersionDisplay.formatSwitchedMessage(formattedVersion)),
          [],
        )
      }
    }

  | Error(error) => {
      let (errorHeader, errorBody) = Connection.Error.toString(error)
      let header = View.Header.Error(
        "Failed to switch to a different installation: " ++ errorHeader,
      )
      let body = [Item.plainText(errorBody)]
      await State__View.Panel.display(state, header, body)
    }
  }
}

module QP = {
  type t = {
    state: State.t,
    rerender: unit => promise<unit>,
    quickPick: VSCode.QuickPick.t<VSCode.QuickPickItem.t>,
    mutable items: array<VSCode.QuickPickItem.t>,
    subscriptions: array<VSCode.Disposable.t>,
  }

  let make = (state, run) => {
    state,
    rerender: () => run(state),
    quickPick: VSCode.Window.createQuickPick(),
    items: [],
    subscriptions: [],
  }

  let render = self => {
    self.quickPick->VSCode.QuickPick.setItems(self.items)
    self.quickPick->VSCode.QuickPick.show
  }

  let destroy = self => {
    self.quickPick->VSCode.QuickPick.dispose
    self.subscriptions->Array.forEach(sub => sub->VSCode.Disposable.dispose)
  }
}

let handleSelection = async (
  self: QP.t,
  platformDeps: Platform.t,
  memento: Memento.t,
  globalStorageUri: VSCode.Uri.t,
  selection: VSCode.QuickPickItem.t,
) => {
  switch SelectionParsing.parseSelection(selection.label, selection.detail) {
  | OpenFolder =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  | DownloadLatestALS =>
    let result = await DownloadWorkflow.downloadLatestALS(platformDeps, memento, globalStorageUri)
    await DownloadWorkflow.handleDownloadResult(result, self.rerender)

  | SwitchToEndpoint(rawPath) =>
    switch await Connection.Endpoint.getPicked(
      self.state.memento,
      Config.Connection.getAgdaPaths(),
    ) {
    | Error(_) => await self.rerender()
    | Ok(original) =>
      let selectionChanged =
        rawPath !== Connection.Endpoint.toURI(original)->Connection.URI.toString
      if selectionChanged {
        switch Connection.URI.parse(rawPath) {
        | LspURI(_, url) => Js.log("Trying to connect with: " ++ url.toString())
        | FileURI(_, uri) =>
          switch await Connection.Endpoint.fromVSCodeUri(uri) {
          | Error(e) => Js.log(e)
          | Ok(newEndpoint) =>
            // save the selected connection as the "picked" connection
            await Connection.Endpoint.setPicked(self.state.memento, Some(newEndpoint))
            await switchAgdaVersion(self.state)
            await self.rerender()
          }
        }
      }
    }
  }
}

let rec run = async (state, platformDeps: Platform.t) => {
  let qp = QP.make(state, state => run(state, platformDeps))
  // set placeholder
  qp.quickPick->VSCode.QuickPick.setPlaceholder("Switch Agda Version")

  // let latestALSAlreadyDownloaded = await State__Connection.LatestALS.alreadyDownloaded(state)()

  // items to be shown in the quick pick
  let miscItems = [ItemCreation.createFolderItem()]

  //
  //  Installed Agda or Agda Language Server
  //

  let selected = await Connection.Endpoint.getPicked(
    state.memento,
    Config.Connection.getAgdaPaths(),
  )

  let isSelected = endpoint =>
    switch selected {
    | Error(_) => false
    | Ok(selected) =>
      switch endpoint {
      | Error(_) => false
      | Ok(endpoint) => Connection.Endpoint.toURI(selected) == Connection.Endpoint.toURI(endpoint)
      }
    }

  // converting a endpoiont to a quick pick item
  let endpointToItem = endpoiont =>
    switch endpoiont {
    | Ok(Connection.Endpoint.Agda(version, path)) =>
      ItemCreation.createAgdaItem(version, path, isSelected(endpoiont), state.extensionUri)
    | Ok(ALS(alsVersion, agdaVersion, method, _)) =>
      ItemCreation.createALSItem(alsVersion, agdaVersion, method, isSelected(endpoiont))
    | Error(error) => ItemCreation.createErrorItem(error)
    }

  let fetchSpecToItem = (globalStoragePath: VSCode.Uri.t, installedPaths: array<string>) => (
    fetchSpec: Connection__Download__GitHub.FetchSpec.t,
  ) => {
    let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
      asset.name
      ->String.replaceRegExp(%re("/als-Agda-/"), "")
      ->String.replaceRegExp(%re("/-.*/"), "")
    let agdaVersion = getAgdaVersion(fetchSpec.asset)
    let alsVersion =
      fetchSpec.release.name->String.split(".")->Array.last->Option.getOr(fetchSpec.release.name) // take the last digit as the version

    let filename = NodeJs.Path.join([
      VSCode.Uri.fsPath(globalStoragePath),
      fetchSpec.saveAsFileName,
      "als",
    ])
    let downloaded = Array.includes(installedPaths, filename)
    let versionString = VersionDisplay.formatALSVersion(alsVersion, agdaVersion)
    ItemCreation.createDownloadItem(downloaded, versionString)
  }

  // installed Agda or Agda Language Server
  let installedSeperator = [ItemCreation.createSeparatorItem("Installed")]

  module PlatformOps = unpack(platformDeps)
  let installedEndpoints = await PlatformOps.getInstalledEndpointsAndPersistThem(
    state.globalStorageUri,
  )
  let installedPaths =
    installedEndpoints
    ->Dict.valuesToArray
    ->Array.filterMap(x =>
      switch x {
      | Error(_) => None
      | Ok(endpoint) => Some(Connection.Endpoint.toURI(endpoint)->Connection.URI.toString)
      }
    )
  let installedItemsFromSettings = installedEndpoints->Dict.valuesToArray->Array.map(endpointToItem)

  let downloadSeperator = [ItemCreation.createSeparatorItem("Download")]

  let downloadLatestALSFetchSpec = switch await PlatformOps.determinePlatform() {
  | Error(_) => None
  | Ok(platform) =>
    switch await Connection__LatestALS.getFetchSpec(
      state.memento,
      state.globalStorageUri,
      platform,
    ) {
    | Error(_) => None
    | Ok(fetchSpec) => Some(fetchSpec)
    }
  }
  let downloadLatestALSItems = switch downloadLatestALSFetchSpec {
  | None => []
  | Some(fetchSpec) => [fetchSpecToItem(state.globalStorageUri, installedPaths)(fetchSpec)]
  }

  let items = Array.flat([
    // installed endpoints
    installedSeperator,
    installedItemsFromSettings,
    // downloadable endpoints
    downloadSeperator,
    downloadLatestALSItems,
    // misc operations
    miscItems,
  ])
  qp.items = items

  qp->QP.render

  // downloadable"

  // events
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(selectedItems => {
    selectedItems[0]->Option.forEach(item =>
      handleSelection(qp, platformDeps, state.memento, state.globalStorageUri, item)->ignore
    )
  })
  ->Util.Disposable.add(qp.subscriptions)
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
}
