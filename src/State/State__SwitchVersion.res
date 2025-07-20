// Pure business logic modules
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
    | SwitchToTarget(string)

  let parseSelection = (label: string, detail: option<string>): selectionType =>
    switch label {
    | "$(folder-opened)  Open download folder" => OpenFolder
    | "$(cloud-download)  Download the latest Agda Language Server" => DownloadLatestALS
    | _ =>
      switch detail {
      | Some(path) => SwitchToTarget(path)
      | None => SwitchToTarget("")
      }
    }
}

module DownloadWorkflow = {
  // Download result type
  type downloadResult =
    | Success(Connection.Target.t, bool) // target, was already downloaded
    | Failure(string) // error message

  // Check if ALS is already downloaded and download if needed
  let downloadLatestALS = async (memento: State__Memento.t, globalStorageUri: VSCode.Uri.t) => {
    switch await Connection__Download__Platform.determine() {
    | Error(_) =>
      Failure("Failed to determine the platform for downloading the Agda Language Server")
    | Ok(platform) =>
      switch await Connection__LatestALS.alreadyDownloaded(globalStorageUri)() {
      | None =>
        switch await Connection__LatestALS.download(memento, globalStorageUri)(platform) {
        | Error(error) => Failure(AgdaModeVscode.Connection__Download.Error.toString(error))
        | Ok(target) => Success(target, false) // false means it was not already downloaded
        }
      | Some(target) => Success(target, true) // true means it was already downloaded
      }
    }
  }
  // Show appropriate message based on download result
  let handleDownloadResult = async (result: downloadResult, rerender: unit => promise<unit>) => {
    switch result {
    | Failure(message) =>
      let _ = await VSCode.Window.showErrorMessage(message, [])
    | Success(target, alreadyDownloaded) =>
      let version = switch target {
      | Agda(version, _) => VersionDisplay.formatAgdaVersion(version)
      | ALS(alsVersion, agdaVersion, _) => VersionDisplay.formatALSVersion(alsVersion, agdaVersion)
      }
      if alreadyDownloaded {
        let _ = await VSCode.Window.showInformationMessage(version ++ " is already downloaded", [])
      } else {
        // rerender the quick pick so that the downloaded target is shown as installed
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
  // Create QuickPick items for various targets and operations
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
    extensionPath: string,
  ): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: VersionDisplay.formatAgdaVersion(version),
    description: if isSelected {
      "Selected"
    } else {
      ""
    },
    detail: path,
    iconPath: VSCode.IconPath.fromDarkAndLight({
      "dark": VSCode.Uri.joinPath(VSCode.Uri.file(extensionPath), ["asset/dark.png"]),
      "light": VSCode.Uri.joinPath(VSCode.Uri.file(extensionPath), ["asset/light.png"]),
    }),
  }

  let createALSItem = (
    alsVersion: string,
    agdaVersion: string,
    method: Connection__Target__IPC.t,
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
    | ViaTCP(url) => url.toString()
    | ViaPipe(path, _, _) => path
    },
  }

  let createErrorItem = (error: Connection__Target.Error.t): VSCode.QuickPickItem.t => {
    VSCode.QuickPickItem.label: "$(error)  Bad path",
    description: "",
    detail: Connection__Target.Error.toString(error),
  }
}

let openGlobalStorageFolder = async (state: State.t) => {
  let _result = await VSCode.Env.openExternal(state.globalStorageUri)
}

let switchAgdaVersion = async (state: State.t) => {
  // display what we are going to do
  switch await Connection.Target.getPicked(state.memento, Config.Connection.getAgdaPaths()) {
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
  | Ok(ALS(alsVersion, agdaVersion, _)) => {
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
  let platformDeps = Platform.makeDesktop()
  switch await Connection.make(
    platformDeps,
    state.memento,
    state.globalStorageUri,
    Config.Connection.getAgdaPaths(),
    ["als", "agda"],
  ) {
  | Ok(conn) =>
    state.connection = Some(conn)
    switch await Connection.Target.getPicked(state.memento, Config.Connection.getAgdaPaths()) {
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
    | Ok(ALS(alsVersion, agdaVersion, _)) => {
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
  memento: State__Memento.t,
  globalStorageUri: VSCode.Uri.t,
  selection: VSCode.QuickPickItem.t,
) => {
  switch SelectionParsing.parseSelection(selection.label, selection.detail) {
  | OpenFolder =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  | DownloadLatestALS =>
    let result = await DownloadWorkflow.downloadLatestALS(memento, globalStorageUri)
    await DownloadWorkflow.handleDownloadResult(result, self.rerender)

  | SwitchToTarget(rawPath) =>
    switch await Connection.Target.getPicked(self.state.memento, Config.Connection.getAgdaPaths()) {
    | Error(_) => await self.rerender()
    | Ok(original) =>
      let selectionChanged = rawPath !== Connection.Target.toURI(original)->Connection.URI.toString
      if selectionChanged {
        switch Connection.URI.parse(rawPath) {
        | URL(url) => Js.log("Trying to connect with: " ++ url.toString())
        | Filepath(path) =>
          switch await Connection.Target.fromRawPath(path) {
          | Error(e) => Js.log(e)
          | Ok(newTarget) =>
            // save the selected connection as the "picked" connection
            await Connection.Target.setPicked(self.state.memento, Some(newTarget))
            await switchAgdaVersion(self.state)
            await self.rerender()
          }
        }
      }
    }
  }
}

let rec run = async state => {
  let qp = QP.make(state, run)
  // set placeholder
  qp.quickPick->VSCode.QuickPick.setPlaceholder("Switch Agda Version")

  // let latestALSAlreadyDownloaded = await State__Connection.LatestALS.alreadyDownloaded(state)()

  // items to be shown in the quick pick
  let miscItems = [ItemCreation.createFolderItem()]

  //
  //  Installed Agda or Agda Language Server
  //

  let selected = await Connection.Target.getPicked(state.memento, Config.Connection.getAgdaPaths())

  let isSelected = target =>
    switch selected {
    | Error(_) => false
    | Ok(selected) =>
      switch target {
      | Error(_) => false
      | Ok(target) => Connection.Target.toURI(selected) == Connection.Target.toURI(target)
      }
    }

  // converting a target to a quick pick item
  let targetToItem = target =>
    switch target {
    | Ok(Connection.Target.Agda(version, path)) =>
      ItemCreation.createAgdaItem(version, path, isSelected(target), state.extensionPath)
    | Ok(ALS(alsVersion, agdaVersion, method)) =>
      ItemCreation.createALSItem(alsVersion, agdaVersion, method, isSelected(target))
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

  let platformDeps = Platform.makeDesktop()
  module PlatformOps = unpack(platformDeps)
  let installedTargets = await PlatformOps.getInstalledTargetsAndPersistThem(state.globalStorageUri)
  let installedPaths =
    installedTargets
    ->Dict.valuesToArray
    ->Array.filterMap(x =>
      switch x {
      | Error(_) => None
      | Ok(target) => Some(Connection.Target.toURI(target)->Connection.URI.toString)
      }
    )
  let installedItemsFromSettings = installedTargets->Dict.valuesToArray->Array.map(targetToItem)

  let downloadSeperator = [ItemCreation.createSeparatorItem("Download")]

  let downloadLatestALSFetchSpec = switch await Connection__Download__Platform.determine() {
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
    // installed targets
    installedSeperator,
    installedItemsFromSettings,
    // downloadable targets
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
      handleSelection(qp, state.memento, state.globalStorageUri, item)->ignore
    )
  })
  ->Util.Disposable.add(qp.subscriptions)
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
}
