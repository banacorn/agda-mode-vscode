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
        View.Header.Plain("Switching to Agda v" ++ version),
        [],
      )
    }
  | Ok(ALS(alsVersion, agdaVersion, _)) => {
      await State__View.Panel.displayStatus(state, "")
      await State__View.Panel.display(
        state,
        View.Header.Plain(
          "Switching to Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion,
        ),
        [],
      )
    }
  }

  // stop the old connection
  let _ = await state.connection->Connection.destroy

  // start with the new connection
  let platform = await Connection__Download__Platform.determine()
  switch await Connection.make(
    state.memento,
    Config.Connection.getAgdaPaths(),
    ["als", "agda"],
    platform,
    State__Connection.askUserAboutDownloadPolicy,
    Connection.LatestALS.alreadyDownloaded(state.globalStorageUri),
    Connection.LatestALS.download(state.memento, state.globalStorageUri),
  ) {
  | Ok(conn) =>
    state.connection = Some(conn)
    switch await Connection.Target.getPicked(state.memento, Config.Connection.getAgdaPaths()) {
    | Error(_) => ()
    | Ok(Agda(version, _path)) => {
        await State__View.Panel.displayStatus(state, "Agda v" ++ version)
        await State__View.Panel.display(
          state,
          View.Header.Success("Switched to Agda v" ++ version),
          [],
        )
      }
    | Ok(ALS(alsVersion, agdaVersion, _)) => {
        await State__View.Panel.displayStatus(
          state,
          "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion,
        )
        await State__View.Panel.display(
          state,
          View.Header.Success(
            "Switched to Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion,
          ),
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
  switch selection.label {
  | "$(folder-opened)  Open download folder" =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  | "$(cloud-download)  Download the latest Agda Language Server" =>
    Js.log("Downloading the latest Agda Language Server...")

    switch await Connection__Download__Platform.determine() {
    | Error(_) =>
      Js.log("Failed to determine the platform for downloading the Agda Language Server")
    | Ok(platform) =>
      switch await Connection.LatestALS.download(memento, globalStorageUri)(platform) {
      | Error(_) => Js.log("Failed to download the latest Agda Language Server")
      | Ok(target) =>
        Js.log("Downloaded Agda Language Server")
        await self.rerender()
      }
    }

  // await self.rerender()
  // | "$(sync)  Check for updates" =>
  //   let repo = Connection.makeAgdaLanguageServerRepo(memento, globalStoragePath)
  //   let _ = await Connection__Download__GitHub.ReleaseManifest.fetchFromGitHubAndCache(repo)
  //   await self.rerender()
  | _ =>
    switch await Connection.Target.getPicked(self.state.memento, Config.Connection.getAgdaPaths()) {
    | Error(_) =>
      // self->QP.destroy // close the quick pick
      await self.rerender()
    | Ok(original) =>
      // self->QP.destroy // close the quick pick
      switch selection.detail {
      | None => ()
      | Some(rawPath) =>
        let selectionChanged =
          rawPath !== Connection.Target.toURI(original)->Connection.URI.toString
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
      // }
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
  let miscItems = [
    // {
    //   VSCode.QuickPickItem.label: "Misc",
    //   kind: Separator,
    // },
    // if latestALSAlreadyDownloaded {
    //   let ageInSecs = Connection__Download__GitHub.ReleaseManifest.cacheAgeInSecs(state.memento)
    //   let description = switch ageInSecs / 3600 {
    //   | 0 =>
    //     switch ageInSecs / 60 {
    //     | 0 => "last checked: less than a minute ago"
    //     | 1 => "last checked: 1 minute ago"
    //     | minutes => "last checked: " ++ string_of_int(minutes) ++ " minutes ago"
    //     }
    //   | 1 => "last checked: 1 hour ago"
    //   | hours if hours >= 24 => "last checked: more than a day ago"
    //   | hours => "last checked: " ++ string_of_int(hours) ++ " hours ago"
    //   }

    //   {
    //     VSCode.QuickPickItem.label: "$(sync)  Check for updates",
    //     description,
    //   }
    // } else {
    //   {
    //     VSCode.QuickPickItem.label: "$(cloud-download)  Download the latest Agda Language Server",
    //     description: "automatically kept up-to-date",
    //   }
    // },
    // {
    //   VSCode.QuickPickItem.label: "$(cloud-download)  Download other versions of Agda Language Server",
    // },
    {
      VSCode.QuickPickItem.label: "$(folder-opened)  Open download folder",
      description: "Where the language servers are downloaded to",
    },
  ]

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
    | Ok(Connection.Target.Agda(version, path)) => {
        VSCode.QuickPickItem.label: "Agda v" ++ version,
        description: if isSelected(target) {
          "Selected"
        } else {
          ""
        },
        detail: path,
        iconPath: VSCode.IconPath.fromDarkAndLight({
          "dark": VSCode.Uri.joinPath(VSCode.Uri.file(state.extensionPath), ["asset/dark.png"]),
          "light": VSCode.Uri.joinPath(VSCode.Uri.file(state.extensionPath), ["asset/light.png"]),
        }),
      }
    | Ok(ALS(alsVersion, agdaVersion, method)) => {
        VSCode.QuickPickItem.label: "$(squirrel)  Agda v" ++
        agdaVersion ++
        " Language Server v" ++
        alsVersion,
        description: if isSelected(target) {
          "Selected"
        } else {
          ""
        },
        detail: switch method {
        | ViaTCP(url) => url.toString()
        | ViaPipe(path, _, _) => path
        },
      }
    // | Error(Connection__Error.SomethingWentWrong(path, error)) => {
    //     VSCode.QuickPickItem.label: "$(error)  Error",
    //     description: Connection__Process__Exec.Error.toString(error),
    //     detail: path,
    //   }
    // | Error(CannotHandleURLsATM(url)) => {
    //     VSCode.QuickPickItem.label: "$(question)  Error",
    //     description: "cannot handle URLs at the moment",
    //     detail: url,
    //   }
    | Error(error) => {
        VSCode.QuickPickItem.label: "$(error)  Bad path",
        description: "",
        detail: Connection__Target.Error.toString(error),
      }
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
    let versionString = "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion
    {
      VSCode.QuickPickItem.label: "$(cloud-download)  Download the latest Agda Language Server",
      description: if downloaded {
        "Downloaded and installed"
      } else {
        ""
      },
      detail: versionString,
    }
  }

  // installed Agda or Agda Language Server
  let installedSeperator = [
    {
      VSCode.QuickPickItem.label: "Installed",
      kind: Separator,
    },
  ]

  let installedTargets = await Connection.getInstalledTargetsAndPersistThem(state.globalStorageUri)
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

  let downloadSeperator = [
    {
      VSCode.QuickPickItem.label: "Download",
      kind: Separator,
    },
  ]

  let downloadLatestALSFetchSpec = switch await Connection__Download__Platform.determine() {
  | Error(_) => None
  | Ok(platform) =>
    switch await Connection.LatestALS.getFetchSpec(
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
