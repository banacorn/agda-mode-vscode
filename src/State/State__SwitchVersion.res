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
  _memento: State__Memento.t,
  _globalStoragePath: VSCode.Uri.t,
  selection: VSCode.QuickPickItem.t,
) => {
  switch selection.label {
  | "$(folder-opened)  Open download folder" =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  // | "$(cloud-download)  Download the latest Agda Language Server" =>
  //   let _ = await Connection.downloadLatestALS(memento, globalStoragePath, _ => ())
  //   await self.rerender()
  // | "$(sync)  Check for updates" =>
  //   let repo = Connection.makeAgdaLanguageServerRepo(memento, globalStoragePath)
  //   let _ = await Connection__Download__GitHub.ReleaseManifest.fetchFromGitHubAndCache(repo)
  //   await self.rerender()
  | _ =>
    switch await Connection.Target.getPicked(self.state.memento) {
    | None =>
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

  let latestALSAlreadyDownloaded = await Connection.isLatestALSDownloaded(state.globalStorageUri)

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
  // let latestALS = await LatestALS.getTarget(state)

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
