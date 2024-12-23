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
    // set the connection we picked last time as the selected item
    let pickedConnection: option<string> =
      self.state.memento->State__Type.Memento.get("pickedConnection")
    let selectedItems =
      self.items->Array.filter(item => item.VSCode.QuickPickItem.detail === pickedConnection)

    self.quickPick->VSCode.QuickPick.setItems(self.items)
    self.quickPick->VSCode.QuickPick.setSelectedItems(selectedItems)
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
    await openGlobalStorageFolder(self.state)
    self->QP.destroy
  | _ =>
    let path = selection.detail

    switch await Connection.Target.getPicked(self.state) {
    | None => ()
    | Some(target) => {
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

          self->QP.destroy
        }
      }
    }
  }
}

let run = async state => {
  let qp = QP.make(state)

  // events
  qp.quickPick
  ->VSCode.QuickPick.onDidChangeSelection(items => {
    items[0]->Option.forEach(item => handleSelection(qp, item)->ignore)
  })
  ->Util.Disposable.add(qp.subscriptions)

  // set placeholder
  qp.quickPick->VSCode.QuickPick.setPlaceholder("Switch Agda Version")

  // items to be shown in the quick pick
  let otherItems = [
    {
      VSCode.QuickPickItem.label: "",
      kind: Separator,
    },
    {
      VSCode.QuickPickItem.label: "Open download folder",
      description: "Where the language servers are downloaded",
    },
  ]

  // quickPick
  // ->VSCode.QuickPick.onDidChangeActive(items => Js.log2("onDidChangeActive", items))
  // ->Util.Disposable.add(subscriptions)

  // onDidChangeSelection
  // quickPick
  // ->VSCode.QuickPick.onDidChangeValue(value => Js.log2("onDidChangeValue", value))
  // ->Util.Disposable.add(subscriptions)

  // quickPick
  // ->VSCode.QuickPick.onDidTriggerButton(button => Js.log2("onDidTriggerButton", button))
  // ->Util.Disposable.add(subscriptions)

  // quickPick
  // ->VSCode.QuickPick.onDidAccept(() => Js.log("onDidAccept"))
  // ->Util.Disposable.add(subscriptions)

  // onDidHide
  qp.quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    qp->QP.destroy
  })
  ->Util.Disposable.add(qp.subscriptions)

  qp.items = Array.flat([otherItems])
  qp->QP.render

  // // fetch the latest release manifest of the language server and show them
  // let agdaLanguageServerRepo = Connection.makeAgdaLanguageServerRepo(
  //   VSCode.Uri.fsPath(state.globalStorageUri),
  // )
  // let (result, _isFromCache) = await Connection.Resolver.GitHub.getReleaseManifest(
  //   agdaLanguageServerRepo,
  // )

  // let languageServerItemsSeparator = [
  //   {
  //     VSCode.QuickPickItem.label: "Language Servers",
  //     kind: Separator,
  //   },
  // ]
  // let languageServerItems = switch result {
  // | Ok(releases) =>
  //   releases->Array.map(release => {
  //     let version = release.tag_name
  //     let description = switch release.body {
  //     | Some(body) => body
  //     | None => "No description"
  //     }
  //     let label = "Agda Language Server " ++ version
  //     {
  //       VSCode.QuickPickItem.label,
  //       description,
  //     }
  //   })
  // | Error(_) => []
  // }

  //
  //  Installations
  //

  let installationItemsSeperator = [
    {
      VSCode.QuickPickItem.label: "Installations",
      kind: Separator,
    },
  ]

  let installationItems = Connection.Target.getRawPathsFromConfig()->Array.map(path => {
    {
      VSCode.QuickPickItem.label: path,
    }
  })

  qp.items = Array.flat([installationItemsSeperator, installationItems, otherItems])
  qp->QP.render

  //
  //  Resolved Installations
  //

  let installationTargets = await Connection.Target.getAll()

  let installationItems = installationTargets->Array.map(result =>
    switch result {
    | Error(CannotResolvePath(path)) => {
        VSCode.QuickPickItem.label: "$(error)  Error",
        description: "unable to resolve the given path",
        detail: path,
      }
    // | Error(Agda(Validation(error))) => {
    //     VSCode.QuickPickItem.label: "$(error)  Validation Error",
    //     description: error,
    //     detail: path,
    //   }
    | Ok(Agda(version, path)) => {
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
    | _ =>
      Js.log(result)
      {
        VSCode.QuickPickItem.label: "Unknown",
        description: "???",
      }
    }
  )

  let items = Array.flat([installationItemsSeperator, installationItems, otherItems])
  qp.items = items
  qp->QP.render
}
