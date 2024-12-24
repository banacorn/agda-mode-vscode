let openGlobalStorageFolder = async (state: State__Type.t) => {
  let _result = await VSCode.Env.openExternal(state.globalStorageUri)
}

let switchAgdaVersion = async state => {
  // display what we are going to do
  switch await Connection.Target.getPicked(state) {
  | None => ()
  | Some(Agda(version, path)) => {
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
    | Some(Agda(version, path)) => {
        await State.View.Panel.displayStatus(state, "Agda v" ++ version)
        await State.View.Panel.display(
          state,
          View.Header.Success("Switched to Agda v" ++ version),
          [],
        )
      }
    | Some(ALS(alsVersion, agdaVersion, _)) => {
        await State.View.Panel.displayStatus(state, "Agda v" ++ agdaVersion ++ " Language Server v" ++ alsVersion)
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
    quickPick: VSCode.QuickPick.t<VSCode.QuickPickItem.t>,
    mutable items: array<VSCode.QuickPickItem.t>,
    subscriptions: array<VSCode.Disposable.t>,
  }

  let make = state => {
    state,
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

let handleSelection = async (self: QP.t, selection: VSCode.QuickPickItem.t) => {
  switch selection.label {
  | "Open download folder" =>
    self->QP.destroy
    await openGlobalStorageFolder(self.state)
  | _ =>
    let path = selection.detail
    switch await Connection.Target.getPicked(self.state) {
    | None => self->QP.destroy
    | Some(target) => {
        self->QP.destroy
        let selectionChanged = path !== Some(Connection.Target.getPath(target))
        if selectionChanged {
          // remember the selected connection as the "picked" connection
          switch path {
          | None => ()
          | Some(path) =>
            switch await Connection.Target.probePath(path) {
            | Error(_) => ()
            | Ok(newTarget) => 
                await Connection.Target.setPicked(self.state, newTarget)
                await switchAgdaVersion(self.state)
            }
          }
        }
      }
    }
  }
}

let run = async state => {
  let qp = QP.make(state)

  // events
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(selectedItems => {
    selectedItems[0]->Option.forEach(item => handleSelection(qp, item)->ignore)
  })
  ->Util.Disposable.add(qp.subscriptions)
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)

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

  // converting a target to a quick pick item
  let targetToItem = target =>
    switch target {
    | Ok(Connection.Target.Agda(version, path)) => {
        VSCode.QuickPickItem.label: "Agda v" ++ version,
        // description: path,
        detail: path,
        iconPath: VSCode.IconPath.fromDarkAndLight({
          "dark": VSCode.Uri.joinPath(VSCode.Uri.file(state.extensionPath), ["asset/dark.png"]),
          "light": VSCode.Uri.joinPath(VSCode.Uri.file(state.extensionPath), ["asset/light.png"]),
        }),
      }
    | Ok(ALS(alsVersion, agdaVersion, method)) => {
        VSCode.QuickPickItem.label: "$(squirrel)  Language Server v" ++ alsVersion,
        description: "Agda v" ++ agdaVersion,
        detail: switch method {
        | Error(path) => path
        | Ok(ViaTCP(port, host, _)) => host ++ ":" ++ string_of_int(port)
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

  let installationTargets = await Connection.Target.getAll()
  let picked = await Connection.Target.getPicked(state)

  let isPicked = target =>
    switch picked {
    | None => false
    | Some(picked) =>
      switch target {
      | Error(_) => false
      | Ok(target) => Connection.Target.getPath(picked) === Connection.Target.getPath(target)
      }
    }

  // "selected" items to be placed at the top of the list
  let selectedItemsSeperator = [
    {
      VSCode.QuickPickItem.label: "Selected",
      kind: Separator,
    },
  ]
  let selectedItems = installationTargets->Array.filter(isPicked)->Array.map(targetToItem)

  // "others" items to be placed below the "selected" items
  let otherInstallationItemsSeperator = [
    {
      VSCode.QuickPickItem.label: "Others",
      kind: Separator,
    },
  ]
  let installationItems =
    installationTargets->Array.filter(x => !isPicked(x))->Array.map(targetToItem)

  let items = Array.flat([
    // selected
    selectedItemsSeperator,
    selectedItems,
    // others
    otherInstallationItemsSeperator,
    installationItems,
    // misc operations
    miscItems,
  ])
  qp.items = items
  qp->QP.render
}
