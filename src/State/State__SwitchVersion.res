let openGlobalStorageFolder = async (state: State__Type.t) => {
  let _result = await VSCode.Env.openExternal(state.globalStorageUri)
}

let switchAgdaVersion = async (state: State__Type.t) => {
  // display what we are going to do
  switch await Connection.Target.getPicked(state.memento) {
  | None => ()
  | Some(Agda(version, _)) => {
      await State.View.Panel.displayStatus(state, "")
      await State.View.Panel.display(state, View.Header.Plain("Switching to Agda v" ++ version), [])
    }
  | Some(ALS(alsVersion, agdaVersion, _)) => {
      await State.View.Panel.displayStatus(state, "")
      await State.View.Panel.display(
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
  switch await Connection.make(state.memento) {
  | Ok(conn) =>
    state.connection = Some(conn)
    switch await Connection.Target.getPicked(state.memento) {
    | None => ()
    | Some(Agda(version, _path)) => {
        await State.View.Panel.displayStatus(state, "Agda v" ++ version)
        await State.View.Panel.display(
          state,
          View.Header.Success("Switched to Agda v" ++ version),
          [],
        )
      }
    | Some(ALS(alsVersion, agdaVersion, _)) => {
        await State.View.Panel.displayStatus(
          state,
          "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion,
        )
        await State.View.Panel.display(
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
      await State.View.Panel.display(state, header, body)
    }
  }
}

module LatestALS = {
  // check if the latest ALS is already downloaded
  let alreadyDownloaded = async globalStoragePath => {
    let path = NodeJs.Path.join([globalStoragePath, "latest-als"])
    switch await NodeJs.Fs.access(path) {
    | () => true
    | exception _ => false
    }
  }

  // download the latest ALS
  let download = async (memento, globalStoragePath, target) => {
    let onDownload = _ => ()
    switch await Connection__Download__GitHub.download(
      target,
      memento,
      globalStoragePath,
      onDownload,
    ) {
    | Error(e) => Js.log("Failed to download: " ++ Connection__Download__GitHub.Error.toString(e))
    | Ok(isCached) =>
      Js.log("isCached: " ++ string_of_bool(isCached))

      // add the path of the downloaded file to the config
      let destPath = NodeJs.Path.join([globalStoragePath, target.saveAsFileName, "als"])
      Js.log("Downloaded to: " ++ destPath)
      Config.Connection.addAgdaPath(destPath)->ignore
    }
  }
}

module QP = {
  type t = {
    state: State__Type.t,
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
  latestALS: option<Connection__Download__GitHub.Target.t>,
  globalStoragePath: string,
  selection: VSCode.QuickPickItem.t,
) => {
  switch selection.label {
  | "$(folder-opened)  Open download folder" =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  | "$(cloud-download)  Download the latest Agda Language Server" =>
    switch latestALS {
    | None => ()
    | Some(target) => {
        await LatestALS.download(memento, globalStoragePath, target)
        await self.rerender()
      }
    }
  | "$(sync)  Check for updates" =>
    let repo = Connection.makeAgdaLanguageServerRepo(memento, globalStoragePath)
    let _ = await Connection__Download__GitHub.ReleaseManifest.fetchFromGitHubAndCache(repo)
    await self.rerender()
  | _ =>
    switch await Connection.Target.getPicked(self.state.memento) {
    | None =>
      Js.log("no targets available")
      // self->QP.destroy // close the quick pick
      await self.rerender()
    | Some(original) =>
      // self->QP.destroy // close the quick pick
      switch selection.detail {
      | None => ()
      | Some(rawPath) =>
        let selectionChanged =
          rawPath !== Connection.Target.toURI(original)->Connection.URI.toString
        if selectionChanged {
          switch await Connection.URI.parse(rawPath) {
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

  let latestALSAlreadyDownloaded = await LatestALS.alreadyDownloaded(
    VSCode.Uri.fsPath(state.globalStorageUri),
  )

  // items to be shown in the quick pick
  let miscItems = [
    {
      VSCode.QuickPickItem.label: "Download",
      kind: Separator,
    },
    if latestALSAlreadyDownloaded {
      let ageInSecs = Connection__Download__GitHub.ReleaseManifest.cacheAgeInSecs(state.memento)
      let description = switch ageInSecs / 3600 {
      | 0 =>
        switch ageInSecs / 60 {
        | 0 => "last checked: less than a minute ago"
        | 1 => "last checked: 1 minute ago"
        | minutes => "last checked: " ++ string_of_int(minutes) ++ " minutes ago"
        }
      | 1 => "last checked: 1 hour ago"
      | hours if hours >= 24 => "last checked: more than a day ago"
      | hours => "last checked: " ++ string_of_int(hours) ++ " hours ago"
      }

      {
        VSCode.QuickPickItem.label: "$(sync)  Check for updates",
        description,
      }
    } else {
      {
        VSCode.QuickPickItem.label: "$(cloud-download)  Download the latest Agda Language Server",
        description: "automatically kept up-to-date",
      }
    },
    // {
    //   VSCode.QuickPickItem.label: "$(cloud-download)  Download other verions of Agda Language Server",
    // },
    {
      VSCode.QuickPickItem.label: "$(folder-opened)  Open download folder",
      description: "Where the language servers are downloaded to",
    },
  ]

  //
  //  Installations
  //

  let picked = await Connection.Target.getPicked(state.memento)
  let isPicked = target =>
    switch picked {
    | None => false
    | Some(picked) =>
      switch target {
      | Error(_) => false
      | Ok(target) => Connection.Target.toURI(picked) == Connection.Target.toURI(target)
      }
    }

  // converting a target to a quick pick item
  let targetToItem = target =>
    switch target {
    | Ok(Connection.Target.Agda(version, path)) => {
        VSCode.QuickPickItem.label: "Agda v" ++ version,
        description: if isPicked(target) {
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
        description: if isPicked(target) {
          "Selected"
        } else {
          ""
        },
        detail: switch method {
        | Error(path) => path
        | Ok(ViaTCP(url, _)) => url.toString()
        | Ok(ViaPipe(path, _, _, _)) => path
        },
      }
    | Error(Connection__Error.ValidationError(path, error)) => {
        VSCode.QuickPickItem.label: "$(error)  Error",
        description: Connection__Validation.Error.toString(error),
        detail: path,
      }
    | Error(CannotHandleURLsATM(url)) => {
        VSCode.QuickPickItem.label: "$(question)  Error",
        description: "cannot handle URLs at the moment",
        detail: url,
      }
    | Error(error) =>
      let (header, body) = Connection.Error.toString(error)
      {
        VSCode.QuickPickItem.label: "$(error)  Error",
        description: header,
        detail: body,
      }
    }

  // other installations
  let installationsSeperator = [
    {
      VSCode.QuickPickItem.label: "Installations",
      kind: Separator,
    },
  ]
  let installationTargets = await Connection.Target.getAllFromConfig()
  let installationsItems = installationTargets->Array.map(targetToItem)

  let items = Array.flat([
    // installation
    installationsSeperator,
    installationsItems,
    // misc operations
    miscItems,
  ])
  qp.items = items
  qp->QP.render

  // downloadable"
  let chooseAssetFromRelease = (release: Connection__Download__GitHub.Release.t): array<
    Connection__Download__GitHub.Asset.t,
  > => {
    // determine the platform
    let platform = switch NodeJs.Os.platform() {
    | "darwin" =>
      switch Node__OS.arch() {
      | "x64" => Some("macos-x64")
      | "arm64" => Some("macos-arm64")
      | _ => None
      }
    | "linux" => Some("ubuntu")
    | "win32" => Some("windows")
    | _ => None
    }
    switch platform {
    | Some(platform) =>
      release.assets->Array.filter(asset => asset.name->String.endsWith(platform ++ ".zip"))
    | None => []
    }
  }

  let latestALS = switch await Connection.getALSReleaseManifest(
    state.memento,
    state.globalStorageUri,
  ) {
  | Error(_error) => None
  | Ok(releases) =>
    // only releases after 2024-12-18 are considered
    let laterReleases =
      releases->Array.filter(release =>
        Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
      )
    // present only the latest release at the moment
    let latestRelease =
      laterReleases
      ->Array.toSorted((a, b) =>
        Date.compare(Date.fromString(b.published_at), Date.fromString(a.published_at))
      )
      ->Array.get(0)

    switch latestRelease {
    | None => None
    | Some(latestRelease) =>
      // for v0.2.7.0.0 onward, the ALS version is represented by the last digit
      // let alsVersion = {
      //   let parts = String.split(latestRelease.tag_name, ".")
      //   parts[Array.length(parts) - 1]->Option.getOr("?")
      // }
      let getAgdaVersion = (asset: Connection__Download__GitHub.Asset.t) =>
        asset.name
        ->String.replaceRegExp(%re("/als-Agda-/"), "")
        ->String.replaceRegExp(%re("/-.*/"), "")
      // choose the assets of the corresponding platform
      let assets = chooseAssetFromRelease(latestRelease)
      // choose the asset with the latest Agda version
      assets
      ->Array.toSorted((a, b) => Util.Version.compare(getAgdaVersion(b), getAgdaVersion(a)))
      ->Array.map(asset => {
        Connection__Download__GitHub.Target.release: latestRelease,
        asset,
        saveAsFileName: "latest-als",
      })
      ->Array.get(0)
    }
  }

  // events
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(selectedItems => {
    selectedItems[0]->Option.forEach(item =>
      handleSelection(
        qp,
        state.memento,
        latestALS,
        VSCode.Uri.fsPath(state.globalStorageUri),
        item,
      )->ignore
    )
  })
  ->Util.Disposable.add(qp.subscriptions)
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
}
