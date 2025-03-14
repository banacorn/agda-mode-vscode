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
    State__Request.askUserAboutDownloadPolicy,
    State__Request.LatestALS.alreadyDownloaded(state),
    State__Request.LatestALS.download(state),
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

  // let latestALSAlreadyDownloaded = await State__Request.LatestALS.alreadyDownloaded(state)()

  // items to be shown in the quick pick
  let miscItems = [
    {
      VSCode.QuickPickItem.label: "Download",
      kind: Separator,
    },
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

  let picked = await Connection.Target.getPicked(state.memento, Config.Connection.getAgdaPaths())
  let isPicked = target =>
    switch picked {
    | Error(_) => false
    | Ok(picked) =>
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

  // other installations
  let installationsSeperator = [
    {
      VSCode.QuickPickItem.label: "Installations",
      kind: Separator,
    },
  ]
  let installationTargets = await Connection.Target.fromURIs(Config.Connection.getAgdaPaths())
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
