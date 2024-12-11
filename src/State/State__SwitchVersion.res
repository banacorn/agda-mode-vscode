let openGlobalStorage = async state => {
  let uri = state.State__Type.globalStorageUri->VSCode.Uri.with_({
    "authority": None,
    "fragment": None,
    "path": None,
    "prompt": None,
    "scheme": Some("file"),
  })
  let _result = await VSCode.Env.openExternal(uri)
}

let switchAgdaVersion = async (state, newAgdaVersion) => {
  // preserve the original version, in case the new one fails
  let existingAgdaVersion = Config.Connection.getAgdaVersion()
  let existingAgdaPath = Config.Connection.getAgdaPath()

  // don't connect to the LSP server
  let useLSP = false

  await Config.Connection.setAgdaPath("")
  // set the name of executable to `newAgdaVersion` in the settings
  await Config.Connection.setAgdaVersion(newAgdaVersion)
  await State.View.Panel.display(
    state,
    View.Header.Plain("Switching to '" ++ newAgdaVersion ++ "'"),
    [],
  )
  // stop the old connection
  let _ = await Connection.stop()
  switch await Connection.start(state.globalStorageUri, useLSP, State.onDownload(state, ...)) {
  | Ok(Emacs(version, path)) =>
    // update the connection status
    await State.View.Panel.displayStatus(state, "Emacs v" ++ version)
    await State.View.Panel.display(
      state,
      View.Header.Success("Switched to version '" ++ version ++ "'"),
      [Item.plainText("Found '" ++ newAgdaVersion ++ "' at: " ++ path)],
    )

    // update the state.agdaVersion to the new version
    state.agdaVersion = Some(version)
  | Ok(LSP(version, _)) =>
    // should not happen
    await State.View.Panel.display(
      state,
      View.Header.Success("Panic, Switched to LSP server '" ++ version ++ "'"),
      [Item.plainText("Should have switched to an Agda executable, please file an issue")],
    )
  | Error(error) =>
    let (errorHeader, errorBody) = Connection.Error.toString(error)
    let header = View.Header.Error(
      "Cannot switch Agda version '" ++ newAgdaVersion ++ "' : " ++ errorHeader,
    )
    let body = [Item.plainText(errorBody ++ "\n\n" ++ "Switching back to " ++ existingAgdaPath)]
    await Config.Connection.setAgdaPath(existingAgdaPath)
    await Config.Connection.setAgdaVersion(existingAgdaVersion)
    await State.View.Panel.display(state, header, body)
  }
}


let switchAgdaVersionOld2 = async (state: State__Type.t) => {
  let existingAgdaVersion = Config.Connection.getAgdaVersion()

  let result = await VSCode.Window.showInputBox(
    ~option={
      value: existingAgdaVersion,
      placeHolder: "For example: agda-2.7.0.1",
      validateInput: name => {
        let name = String.trim(name)
        let promise = LanguageServerMule.Source.search(
          FromCommand(name),
        )->Promise.thenResolve(result => {
          switch result {
          | Ok(result) =>
            let location = switch result {
            | ViaPipe(path, _, _, _) => "at " ++ path
            | ViaTCP(port, host, _) => "at " ++ host ++ ":" ++ string_of_int(port)
            }

            let msg = {
              VSCode.InputBoxValidationMessage.message: "Found '" ++ name ++ "' at: " ++ location,
              severity: VSCode.InputBoxValidationSeverity.Info,
            }
            Some(VSCode.StringOr.make(Others(msg)))
          | Error(error) =>
            let msg = LanguageServerMule.Source.Error.toString(error)
            Some(
              VSCode.StringOr.make(String("Cannot switch Agda version '" ++ name ++ "' : " ++ msg)),
            )
          }
        })
        VSCode.PromiseOr.make(Promise(promise))
      },
    },
  )

  switch result {
    | None => ()
    | Some(result) => 
        let newAgdaVersion = String.trim(result)
        await switchAgdaVersion(state, newAgdaVersion)
  }
}

let handleSelection = async (state, selection: VSCode.QuickPickItem.t) => {
  switch selection.label {
  | "Open download folder" => await openGlobalStorage(state)
  | "Change Agda command name" => await switchAgdaVersionOld2(state)
  | _ => Js.log("Unknown selection")
  }
}

let run = async state => {
  let quickPick = VSCode.Window.createQuickPick()
  let subscriptions = []

  quickPick->VSCode.QuickPick.setPlaceholder("Switch Agda Version")

  quickPick->VSCode.QuickPick.setItems([
    // {
    //   VSCode.QuickPickItem.label: "Language Servers",
    //   kind: Separator,
    // },
    // {
    //   VSCode.QuickPickItem.label: "2.6.3",
    //   description: "The previous version of Agda",
    // },
    // {
    //   VSCode.QuickPickItem.label: "Other operations",
    //   kind: Separator,
    // },
    {
      VSCode.QuickPickItem.label: "Change Agda command name",
      description: "Execute Agda with given command name",
    },
    {
      VSCode.QuickPickItem.label: "Open download folder",
      description: "Where the language servers are downloaded",
    },
  ])

  // quickPick
  // ->VSCode.QuickPick.onDidChangeActive(items => Js.log2("onDidChangeActive", items))
  // ->Util.Disposable.add(subscriptions)

  // onDidChangeSelection
  quickPick
  ->VSCode.QuickPick.onDidChangeSelection(items => {
    // Js.log2("selected item:", items)
    items->Array.forEach(item => handleSelection(state, item)->ignore)
  })
  ->Util.Disposable.add(subscriptions)

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
  quickPick
  ->VSCode.QuickPick.onDidHide(() => {
    quickPick->VSCode.QuickPick.dispose
    subscriptions->Array.forEach(sub => sub->VSCode.Disposable.dispose)
  })
  ->Util.Disposable.add(subscriptions)

  quickPick->VSCode.QuickPick.show
}
