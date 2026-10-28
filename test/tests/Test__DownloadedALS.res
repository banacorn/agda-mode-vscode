open Mocha
open Test__Util

// https://github.com/banacorn/agda-mode-vscode/issues/87
// With no local Agda, Load downloads ALS from the channel in
// AGDA_TEST_DOWNLOADED_ALS ("dev" or "latest"), and must connect to the
// native ALS, not fall back to WASM. Skipped when the variable is unset, since
// it downloads from GitHub. Run it with AGDA_TEST_EXTENSION_HOST_PATH, so the
// downloaded ALS cannot borrow runtime libraries from developer tools.
let channel = switch NodeJs.Process.process
->NodeJs.Process.env
->Dict.get("AGDA_TEST_DOWNLOADED_ALS") {
| None => None
| Some("dev") => Some("dev")
| Some("latest") => Some("latest")
| Some(value) =>
  raise(Invalid_argument(`AGDA_TEST_DOWNLOADED_ALS must be "dev" or "latest", got "${value}"`))
}

// the asset tag of the native ALS for this host, as in its release asset names
let platformTag = switch (NodeJs.Os.platform(), NodeJs.Os.arch()) {
| ("win32", _) => "windows"
| ("linux", _) => "ubuntu"
| ("darwin", "arm64") => "macos-arm64"
| ("darwin", _) => "macos-x64"
| (platform, arch) => raise(Failure(`no native ALS for ${platform} ${arch}`))
}

// the extension's global storage, under the --user-data-dir that
// RunTestFromCLI writes to test-user-data-dir.txt
let globalStorageUri = () => {
  let userDataDir =
    NodeJs.Fs.readFileSync(
      NodeJs.Path.join([VSCode.Uri.fsPath(Path.extensionUri), "test-user-data-dir.txt"]),
    )->NodeJs.Buffer.toString
  VSCode.Uri.file(NodeJs.Path.join([userDataDir, "User", "globalStorage", "banacorn.agda-mode"]))
}

let directoryNames = async uri =>
  switch await FS.readDirectory(uri) {
  | Ok(entries) => entries->Array.map(((name, _)) => name)
  | Error(_) => []
  }

// the path of every native ALS downloaded from the release of `channel`
let downloadedNativePaths = async channel => {
  let releasesUri = VSCode.Uri.joinPath(globalStorageUri(), ["releases"])
  let tags = (await directoryNames(releasesUri))->Array.filter(tag =>
    channel == "dev" ? tag == "dev" : tag != "dev"
  )
  let paths = await Promise.all(
    tags->Array.map(async tag => {
      let names = await directoryNames(VSCode.Uri.joinPath(releasesUri, [tag]))
      names
      ->Array.filter(name => name->String.endsWith("-" ++ platformTag))
      ->Array.map(name =>
        VSCode.Uri.joinPath(releasesUri, [tag, name, platformTag == "windows" ? "als.exe" : "als"])->VSCode.Uri.fsPath
      )
    }),
  )
  paths->Array.flat
}

let updateConnectionSetting = (key, value) =>
  VSCode.Workspace.getConfiguration(Some("agdaMode"), None)->VSCode.WorkspaceConfiguration.updateGlobalSettings(
    key,
    value,
    None,
  )

let getConnectionSetting = (key): option<JSON.t> =>
  VSCode.Workspace.getConfiguration(Some("agdaMode"), None)->VSCode.WorkspaceConfiguration.get(key)

describe("issue #87: Load through the downloaded ALS", () => {
  // a download, an extraction and a first Load
  This.timeout(600000)

  let filepath = Path.asset("DownloadedALS.agda")
  let interfaceUri = VSCode.Uri.file(Path.asset("DownloadedALS.agdai"))

  // what the test changes, restored afterwards for the test files that follow
  let restore: ref<option<unit => promise<unit>>> = ref(None)

  Async.after(async () => {
    switch restore.contents {
    | None => ()
    | Some(restore) => await restore()
    }
    let _ = await FS.deleteRecursive(interfaceUri)
  })

  Async.it("connects to the native ALS and finishes Load with no errors", async () => {
    switch channel {
    | None => This.skip()
    | Some(channel) =>
      // activated directly, not through `activateExtension`, which would fix
      // the preferred candidate of every test file that follows to None
      let exports: Main.activationExports = switch VSCode.Extensions.getExtension(
        "banacorn.agda-mode",
      ) {
      | None => raise(Failure("Extension banacorn.agda-mode not found"))
      | Some(extension) => await extension->VSCode.Extension.activate
      }
      let memento = exports.memento

      let previousCandidate = Memento.PreferredCandidate.get(memento)
      let previousChannel = Memento.SelectedChannel.get(memento)
      let previousPaths = getConnectionSetting("connection.paths")
      let previousPolicy = getConnectionSetting("connection.downloadPolicy")
      restore :=
        Some(
          async () => {
            await Memento.PreferredCandidate.set(memento, previousCandidate)
            switch previousChannel {
            | None => await Memento.SelectedChannel.clear(memento)
            | Some(previous) => await Memento.SelectedChannel.set(memento, previous)
            }
            await updateConnectionSetting("connection.paths", previousPaths)
            await updateConnectionSetting("connection.downloadPolicy", previousPolicy)
          },
        )

      // no candidate to connect to, so Load falls back to downloading,
      // without asking
      await Memento.PreferredCandidate.set(memento, None)
      await Memento.SelectedChannel.set(memento, channel)
      await updateConnectionSetting("connection.paths", Some(JSON.Array([])))
      await updateConnectionSetting("connection.downloadPolicy", Some(JSON.String("Yes")))

      let created = []
      let stopListening = exports.channels.log->Chan.on(log =>
        switch log {
        | Log.Connection(EstablishFlow(ConnectionCreated(path, kind))) =>
          created->Array.push((path, kind))
        | _ => ()
        }
      )

      let (loaded, resolveLoaded, _) = Util.Promise_.pending()
      let stopWaiting = exports.channels.commandHandled->Chan.on(command =>
        if command == Command.Load {
          resolveLoaded()
        }
      )
      let _ = await File.open_(filepath)
      let state = switch await executeCommand("agda-mode.load") {
      | None => raise(Failure("Cannot load " ++ filepath))
      | Some(Ok(state)) =>
        await loaded
        state
      | Some(Error(error)) =>
        let (header, body) = Connection.Error.toString(error)
        raise(Failure(header ++ "\n" ++ body))
      }
      stopWaiting()
      stopListening()

      let header = state.panelCache.display->Option.map(((header, _body)) => header)
      let expected = (await downloadedNativePaths(channel))->Array.map(path => (
        path,
        Log.Connection.EstablishFlow.ALS,
      ))
      await AgdaMode.quit({filepath, channels: exports.channels, state})

      Assert.deepStrictEqual(
        (created, header),
        (expected, Some(View.Header.Plain("*All Done*"))),
      )
    }
  })
})
