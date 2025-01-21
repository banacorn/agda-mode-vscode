let openGlobalStorageFolder = async (state: State__Type.t) => {
  let _result = await VSCode.Env.openExternal(state.globalStorageUri)
}

let switchAgdaVersion = async state => {
  // display what we are going to do
  switch await Connection.Target.getPicked(state) {
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
  let _ = await Connection.stop()

  // start with the new connection
  switch await Connection.start(state) {
  | Ok() =>
    switch await Connection.Target.getPicked(state) {
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
  memento: State__Type.Memento.t,
  globalStoragePath: string,
  selection: VSCode.QuickPickItem.t,
) => {
  switch selection.label {
  | "Open download folder" =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  | _ =>
    switch await Connection.Target.getPicked(self.state) {
    | None =>
      Js.log("no targets available")
      // self->QP.destroy // close the quick pick
      await self.rerender()
    | Some(original) =>
      // self->QP.destroy // close the quick pick
      switch selection.detail {
      | None => ()
      | Some(rawPath) =>
        // The rest of the items are assumed to be:
        //    1. Agda/ALS installations available for connection
        //    2. Prebuilt ALS installations available for download
        // We can look at the `detail` field to distinguish between the two:
        //    1. Always a filepath or a URL
        //    2. Starts with "From " and followed by a URL
        let isDownloadable = String.startsWith(rawPath, "From ")
        if isDownloadable {
          let url = rawPath->String.sliceToEnd(~start=5)
          Js.log("Downloading from " ++ url)
          let onDownload = _ => ()
          switch await Connection.Target.downloadALS(memento, globalStoragePath, onDownload) {
          | Error(e) =>
            Js.log("Failed to download: " ++ Connection.Resolver.GitHub.Error.toString(e))
          | Ok((isCached, target)) =>
            Js.log("isCached: " ++ string_of_bool(isCached))
            Js.log2("target: ", target)
            let destPath = NodeJs.Path.join([globalStoragePath, target.saveAsFileName, "als"])
            Js.log("Downloaded to: " ++ destPath)

            // add the path of the downloaded file to the config
            Config.Connection.addAgdaPath(destPath)->ignore
          }
        } else {
          let selectionChanged =
            rawPath !== Connection.Target.toURI(original)->Connection.URI.toString
          if selectionChanged {
            switch await Connection.URI.parse(rawPath) {
            | None => Js.log("Cannot parse path: " ++ rawPath)
            | Some(URL(url)) => Js.log("Trying to connect with: " ++ url.toString())
            | Some(Filepath(path)) =>
              switch await Connection.Target.fromRawPath(path) {
              | Error(e) => Js.log(e)
              | Ok(newTarget) =>
                // save the selected connection as the "picked" connection
                await Connection.Target.setPicked(self.state, Some(newTarget))
                await switchAgdaVersion(self.state)
                await self.rerender()
              }
            }
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

  // items to be shown in the quick pick
  let miscItems = [
    {
      VSCode.QuickPickItem.label: "",
      kind: Separator,
    },
    {
      VSCode.QuickPickItem.label: "Open download folder",
      description: "Where the language servers are downloaded",
    },
  ]

  //
  //  Installations
  //

  let picked = await Connection.Target.getPicked(state)
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
    | Error(CannotResolvePath(path)) => {
        VSCode.QuickPickItem.label: "$(question)  Error",
        description: "unable to resolve the given path",
        detail: path,
      }
    | Error(error) =>
      let (header, body) = Connection.Error.toString(error)
      {
        VSCode.QuickPickItem.label: "$(error)  Error",
        description: header,
        detail: body,
      }
    }

  // // selected installation
  // let selectedInstallationSeperator = [
  //   {
  //     VSCode.QuickPickItem.label: "Selected installation",
  //     kind: Separator,
  //   },
  // ]
  // let selectedInstallation =
  //   installationTargets
  //   ->Array.filter(isPicked)
  //   ->Array.map(targetToItem)
  //   ->Array.slice(~start=0, ~end=1)

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
    // // selected
    // selectedInstallationSeperator,
    // selectedInstallation,
    // installation
    installationsSeperator,
    installationsItems,
    // misc operations
    miscItems,
  ])
  qp.items = items
  qp->QP.render

  // // downloadable"
  // let chooseFromRelease = (release: Connection.Resolver.GitHub.Release.t): array<
  //   Connection.Resolver.GitHub.Asset.t,
  // > => {
  //   // determine the platform
  //   let platform = switch NodeJs.Os.platform() {
  //   | "darwin" =>
  //     switch Node__OS.arch() {
  //     | "x64" => Some("macos-x64")
  //     | "arm64" => Some("macos-arm64")
  //     | _ => None
  //     }
  //   | "linux" => Some("ubuntu")
  //   | "win32" => Some("windows")
  //   | _ => None
  //   }
  //   switch platform {
  //   | Some(platform) =>
  //     release.assets->Array.filter(asset => asset.name->String.endsWith(platform ++ ".zip"))
  //   | None => []
  //   }
  // }

  // // ALS Prebuilt
  // let alsPrebuiltItemsSeparator = [
  //   {
  //     VSCode.QuickPickItem.label: "Available for download",
  //     kind: Separator,
  //   },
  // ]
  // let alsPrebuiltItems = switch await Connection.getALSReleaseManifest(state) {
  // | Error(error) =>
  //   let (header, body) = Connection.Error.toString(error)
  //   [
  //     {
  //       VSCode.QuickPickItem.label: "$(debug-disconnect)  Error",
  //       description: header,
  //       detail: body,
  //     },
  //   ]
  // | Ok(releases) =>
  //   // only releases after 2024-12-18 are considered
  //   let laterReleases =
  //     releases->Array.filter(release =>
  //       Date.fromString(release.published_at) >= Date.fromString("2024-12-18")
  //     )
  //   // present only the latest release at the moment
  //   let latestRelease =
  //     laterReleases
  //     ->Array.toSorted((a, b) =>
  //       Date.compare(Date.fromString(b.published_at), Date.fromString(a.published_at))
  //     )
  //     ->Array.get(0)

  //   switch latestRelease {
  //   | None => []
  //   | Some(latestRelease) =>
  //     // for v0.2.7.0.0 onward, the ALS version is represented by the last digit
  //     let alsVersion = {
  //       let parts = String.split(latestRelease.tag_name, ".")
  //       parts[Array.length(parts) - 1]->Option.getOr("?")
  //     }
  //     // choose the assets of the corresponding platform
  //     let assets = chooseFromRelease(latestRelease)
  //     assets->Array.map(asset => {
  //       let agdaVersion =
  //         asset.name
  //         ->String.replaceRegExp(%re("/als-Agda-/"), "")
  //         ->String.replaceRegExp(%re("/-.*/"), "")
  //       {
  //         VSCode.QuickPickItem.label: "$(squirrel)  Language Server v" ++ alsVersion,
  //         description: "Agda v" ++ agdaVersion,
  //         detail: "From " ++ asset.browser_download_url,
  //       }
  //     })
  //   }
  // }

  // let items = Array.flat([
  //   // selected
  //   selectedInstallationSeperator,
  //   selectedInstallation,
  //   // others
  //   otherInstallationsSeperator,
  //   otherInstallations,
  //   // downloadables
  //   alsPrebuiltItemsSeparator,
  //   alsPrebuiltItems,
  //   // misc operations
  //   miscItems,
  // ])
  // qp.items = items
  // qp->QP.render

  // events
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(selectedItems => {
    selectedItems[0]->Option.forEach(item =>
      handleSelection(qp, state.memento, VSCode.Uri.fsPath(state.globalStorageUri), item)->ignore
    )
  })
  ->Util.Disposable.add(qp.subscriptions)
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)
}
