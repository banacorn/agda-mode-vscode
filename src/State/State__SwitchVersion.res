let openGlobalStorageFolder = async (state: State__Type.t) => {
  let _result = await VSCode.Env.openExternal(state.globalStorageUri)
}

// let switchAgdaVersion = async (state, newAgdaVersion) => {
//   // preserve the original version, in case the new one fails
//   let existingAgdaVersion = Config.Connection.getAgdaVersion()
//   let existingAgdaPaths = Config.Connection.getAgdaPaths()

//   // don't connect to the LSP server
//   let useLSP = false

//   await Config.Connection.setAgdaPath("")
//   // set the name of executable to `newAgdaVersion` in the settings
//   await Config.Connection.setAgdaVersion(newAgdaVersion)
//   await State.View.Panel.display(
//     state,
//     View.Header.Plain("Switching to '" ++ newAgdaVersion ++ "'"),
//     [],
//   )
//   // stop the old connection
//   let _ = await Connection.stop()
//   switch await Connection.start(state.globalStorageUri, useLSP, State.onDownload(state, ...)) {
//   | Ok(Agda(version, path)) =>
//     // update the connection status
//     await State.View.Panel.displayStatus(state, "Agda v" ++ version)
//     await State.View.Panel.display(
//       state,
//       View.Header.Success("Switched to version '" ++ version ++ "'"),
//       [Item.plainText("Found '" ++ newAgdaVersion ++ "' at: " ++ path)],
//     )

//     // update the state.agdaVersion to the new version
//     state.agdaVersion = Some(version)
//   | Ok(ALS(version, _)) =>
//     // should not happen
//     await State.View.Panel.display(
//       state,
//       View.Header.Success("Panic, Switched to ALS '" ++ version ++ "'"),
//       [Item.plainText("Should have switched to an Agda executable, please file an issue")],
//     )
//   | Error(error) =>
//     let (errorHeader, errorBody) = Connection.Error.toString(error)
//     let header = View.Header.Error(
//       "Cannot switch Agda version '" ++ newAgdaVersion ++ "' : " ++ errorHeader,
//     )
//     let body = [Item.plainText(errorBody ++ "\n\n" ++ "Switching back to " ++ existingAgdaPath)]
//     await Config.Connection.setAgdaPath(existingAgdaPath)
//     await Config.Connection.setAgdaVersion(existingAgdaVersion)
//     await State.View.Panel.display(state, header, body)
//   }
// }

// let showInputBoxForSwitchingAgdaVersion = async (state: State__Type.t) => {
//   let existingAgdaVersion = Config.Connection.getAgdaVersion()

//   let result = await VSCode.Window.showInputBox(
//     ~option={
//       value: existingAgdaVersion,
//       placeHolder: "For example: agda-2.7.0.1",
//       validateInput: name => {
//         let name = String.trim(name)
//         let promise = Connection.Resolver.search(
//           FromCommand(name),
//         )->Promise.thenResolve(result => {
//           switch result {
//           | Ok(result) =>
//             let location = switch result {
//             | ViaPipe(path, _, _, _) => "at " ++ path
//             | ViaTCP(port, host, _) => "at " ++ host ++ ":" ++ string_of_int(port)
//             }

//             let msg = {
//               VSCode.InputBoxValidationMessage.message: "Found '" ++ name ++ "' at: " ++ location,
//               severity: VSCode.InputBoxValidationSeverity.Info,
//             }
//             Some(VSCode.StringOr.make(Others(msg)))
//           | Error(error) =>
//             let msg = Connection.Resolver.Error.toString(error)
//             Some(
//               VSCode.StringOr.make(String("Cannot switch Agda version '" ++ name ++ "' : " ++ msg)),
//             )
//           }
//         })
//         VSCode.PromiseOr.make(Promise(promise))
//       },
//     },
//   )

//   switch result {
//   | None => ()
//   | Some(result) =>
//     let newAgdaVersion = String.trim(result)
//     await switchAgdaVersion(state, newAgdaVersion)
//   }
// }

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
    // await openGlobalStorageFolder(self.state)
    // self->QP.destroy
    Js.log("Open download folder")
    ()
  | _ =>
    let path = selection.detail

    switch await Connection.Target.getPicked(self.state) {
    | None =>
      Js.log("No target picked before")
      ()
    | Some(target) => {
        Js.log3("Target picked before", selection.detail, target)
        let selectionChanged = path !== Some(Connection.Target.getPath(target))
        if selectionChanged {
          // remember the selected connection as the "picked" connection
          switch path {
          | None => ()
          | Some(path) =>
            switch await Connection.Target.probePath(path) {
            | Error(_) => ()
            | Ok(newTarget) => await Connection.Target.setPicked(self.state, newTarget)
            }
          }

          // self->QP.destroy
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
    Js.log2("onDidChangeSelection", selectedItems)
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
    | Error(Connection__Error.CannotResolvePath(path)) => {
        VSCode.QuickPickItem.label: "$(error)  Error",
        description: "unable to resolve the given path",
        detail: path,
      }
    | Ok(Connection.Target.Agda(version, path)) => {
        VSCode.QuickPickItem.label: "Agda v" ++ version,
        // description: path,
        detail: path,
        iconPath: VSCode.IconPath.fromDarkAndLight({
          "dark": VSCode.Uri.joinPath(VSCode.Uri.file(state.extensionPath), ["asset/dark.png"]),
          "light": VSCode.Uri.joinPath(VSCode.Uri.file(state.extensionPath), ["asset/light.png"]),
        }),
      }
    | Ok(ALS(alsVersion, agdaVersion, Error(path))) => {
        VSCode.QuickPickItem.label: "$(squirrel)  Language Server v" ++ alsVersion,
        description: "Agda v" ++ agdaVersion,
        detail: path,
      }
    // | Error(Agda(error)) =>
    //   Js.log(target)
    //   {
    //     VSCode.QuickPickItem.label: "$(error)  Error",
    //     description: "unable to resolve the given path",
    //     detail: path,
    //   }
    | _ =>
      Js.log(target)
      {
        VSCode.QuickPickItem.label: "Unknown",
        description: "???",
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
