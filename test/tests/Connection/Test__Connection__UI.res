open Mocha

@module("node:fs/promises") external mkdtemp: string => Js.Promise.t<string> = "mkdtemp"

let makeMockConnection = (_path, _version): Connection.t => {
  %raw(`{
    TAG: "Agda",
    _0: {
      chan: { removeAllListeners: () => {} },
      process: { status: "Destroyed" },
      encountedFirstPrompt: false,
      version: _version,
      path: _path
    },
    _1: _path,
    _2: _version
  }`)
}

describe("Connection UI", () => {
  This.timeout(10000)

  let availableDownload = (
    ~downloaded=false,
    ~versionString,
    ~variant: Connection__Download.SelectionVariant.t,
  ): Connection__Download__Availability.availableDownload => {
    downloaded,
    versionString,
    variant,
  }

  describe("Picker and handlers", () => {
    let makeMockPlatform = (): Platform.t => Mock.Platform.makeWithAgda()

    let createTestStateWithPlatformAndStorage = (
      platform: Platform.t,
      storageUri: VSCode.Uri.t,
    ) => {
      let channels = {
        State.inputMethod: Chan.make(),
        responseHandled: Chan.make(),
        commandHandled: Chan.make(),
        log: Chan.make(),
      }

      let mockEditor = %raw(`{
        document: { fileName: "test.agda" }
      }`)
      let mockExtensionUri = VSCode.Uri.file(NodeJs.Process.cwd(NodeJs.Process.process))

      State.make(
        "test-id",
        platform,
        channels,
        storageUri,
        mockExtensionUri,
        Memento.make(None),
        mockEditor,
        None,
      )
    }

    let createTestStateWithPlatform = (platform: Platform.t) =>
      createTestStateWithPlatformAndStorage(platform, VSCode.Uri.file(NodeJs.Os.tmpdir()))

    let createTestState = () => createTestStateWithPlatform(makeMockPlatform())

    let makePickerItem = (state: State.t, itemData: Connection__UI__ItemData.t) =>
      Connection__UI__Item.fromItemData(itemData, state.extensionUri)

    let selectionVariantTag = (variant: Connection__Download.SelectionVariant.t) =>
      switch variant {
      | Native => "native"
      | WASM => "wasm"
      }

    let hasSelectionChanged = (memento, selectedPath) =>
      switch Memento.PreferredCandidate.get(memento) {
      | Some(path) =>
        !Connection__Candidate.equal(
          Connection__Candidate.make(selectedPath),
          Connection__Candidate.make(path),
        )
      | None => true
      }

    let withTempStorage = async (prefix: string, run: (string, VSCode.Uri.t) => promise<'a>) => {
      let storagePath = await mkdtemp(NodeJs.Path.join([NodeJs.Os.tmpdir(), prefix]))
      let storageUri = VSCode.Uri.file(storagePath)
      let cleanup = async () => {
        let _ = await FS.deleteRecursive(storageUri)
      }
      try {
        let result = await run(storagePath, storageUri)
        await cleanup()
        result
      } catch {
      | exn =>
        await cleanup()
        raise(exn)
      }
    }

    Async.it(
      "candidate rows should route selection through the candidate seam",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let switchCalled = ref(None)
        let sawDestroyed = ref(false)
        let sawSelectionCompleted = ref(false)
        let entry: Memento.ResolvedMetadata.entry = {
          kind: Memento.ResolvedMetadata.Agda(Some("2.7.0.1")),
          timestamp: Date.make(),
          error: None,
        }
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.Destroyed) => sawDestroyed := true
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted) =>
            sawSelectionCompleted := true
          | _ => ()
          }
        )

        let selectedItem =
          makePickerItem(state, Candidate("/tmp/agda", "/tmp/agda", entry, false))

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [selectedItem],
          ~hasSelectionChanged=_ => true,
          ~switchCandidate=path => {
            switchCalled := Some(path)
            Promise.resolve()
          },
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(switchCalled.contents, Some("/tmp/agda"))
        Assert.deepStrictEqual(sawDestroyed.contents, true)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, true)
      },
    )

    Async.it(
      "non-candidate picker items should not trigger candidate selection",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let sawSelectedCandidate = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectedCandidate(_, _, _)) =>
            sawSelectedCandidate := true
          | _ => ()
          }
        )

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [makePickerItem(state, Separator("__unexpected_item__"))],
          ~hasSelectionChanged=_ => true,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
      },
    )

    Async.it(
      "download rows should route selection through download handling",
      async () => {
        let downloadedPath = "/tmp/connection-ui-downloaded-als.wasm"
        let platform = Mock.Platform.makeWithSuccessfulDownload(downloadedPath)
        let state = createTestStateWithPlatform(platform)
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let sawDestroyed = ref(false)
        let sawSelectionCompleted = ref(false)
        let sawSelectedDownloadAction = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.Destroyed) => sawDestroyed := true
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted) =>
            sawSelectionCompleted := true
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectedDownloadAction(false, _)) =>
            sawSelectedDownloadAction := true
          | _ => ()
          }
        )

        let selectedItem = makePickerItem(
          state,
          DownloadAction(false, "Agda v2.8.0 Language Server (dev build)", "wasm"),
        )

        Connection__UI__Handlers.onSelection(
          state,
          platform,
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [selectedItem],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedDownloadAction.contents, true)
        Assert.deepStrictEqual(sawDestroyed.contents, true)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, true)
        Assert.deepStrictEqual(
          Config.Connection.getAgdaPaths()->Array.some(path => path == downloadedPath),
          true,
        )
      },
    )

    Async.it(
      "download selection should not modify PreferredCandidate",
      async () => {
        let cases = [
          Connection__UI__Labels.downloadNativeALS,
          Connection__UI__Labels.downloadWasmALS,
        ]

        let runCase = async (label: string) => {
          let state = createTestState()
          let view = Connection__UI__Picker.make(state.channels.log)

          await Config.Connection.setAgdaPaths(state.channels.log, [])

          let selectedVariant =
            label == Connection__UI__Labels.downloadWasmALS
              ? Connection__Download.SelectionVariant.WASM
              : Connection__Download.SelectionVariant.Native

          let expectedDownloadPath = switch selectedVariant {
          | Connection__Download.SelectionVariant.Native =>
            NodeJs.Path.join([
              VSCode.Uri.fsPath(state.globalStorageUri),
              "releases",
              "dev",
              "als-dev-Agda-2.8.0-macos-arm64",
              "als",
            ])
          | Connection__Download.SelectionVariant.WASM =>
            NodeJs.Path.join([
              VSCode.Uri.fsPath(state.globalStorageUri),
              "releases",
              "dev",
              "als-dev-Agda-2.8.0-wasm",
              "als.wasm",
            ])
          }

          let previouslyPicked = Some("/usr/bin/agda")
          await Memento.PreferredCandidate.set(state.memento, previouslyPicked)
          let activePath = "/opt/homebrew/bin/agda"
          Registry__Connection.status :=
            Active({
              connection: makeMockConnection(activePath, "2.6.3"),
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          let sawSelectedCandidate = ref(false)
          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(Log.SwitchVersion.SelectedCandidate(_, _, _)) =>
              sawSelectedCandidate := true
            | _ => ()
            }
          )

          let onOperationComplete = Log.on(
            state.channels.log,
            log =>
              switch log {
              | Log.SwitchVersionUI(SelectionCompleted) => true
              | _ => false
              },
          )

          Connection__UI__Handlers.onSelection(
            state,
            Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath),
            ref(Connection__Download.Channel.DevALS),
            _downloadItems => Promise.resolve(),
            view,
            [
              makePickerItem(
                state,
                DownloadAction(
                  false,
                  "ALS v1.0.0",
                  selectionVariantTag(selectedVariant),
                ),
              ),
            ],
            ~hasSelectionChanged=selectedPath => hasSelectionChanged(state.memento, selectedPath),
            ~switchCandidate=_selectedPath => Promise.resolve(),
            ~getDownloadItems=_channel => Promise.resolve([]),
          )

          await onOperationComplete

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), previouslyPicked)
          Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
          view->Connection__UI__Picker.destroy
          Registry__Connection.status := Empty
        }

        for i in 0 to Array.length(cases) - 1 {
          await runCase(cases[i]->Option.getExn)
        }
      },
    )

    Async.it(
      "SelectOtherChannels should route through the channel submenu seam",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.LatestALS)
        let capturedItems: ref<array<Connection__UI__Channel.pickerItem>> = ref([])
        let showCallCount = ref(0)
        let patchShow: (Connection__UI__Picker.t, ref<int>) => unit = %raw(`function(view, counter) {
          var orig = view.quickPick.show.bind(view.quickPick);
          view.quickPick.show = function() { counter.contents++; return orig(); };
        }`)
        patchShow(view, showCallCount)

        let selectedItem = makePickerItem(state, SelectOtherChannels)

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [selectedItem],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
          ~showChannelPicker=async (items, _placeholder) => {
            capturedItems := items
            Some("dev")
          },
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(capturedItems.contents->Array.map(item => item.value), ["latest", "dev"])
        Assert.deepStrictEqual(showCallCount.contents, 1)

        view->Connection__UI__Picker.destroy
      },
    )

    Async.it(
      "candidate selection should treat alias-equivalent paths as unchanged",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let fsPath = "/tmp/dev-als/als.wasm"
        let uriPath = "file:///tmp/dev-als/als.wasm"
        let sawSelectedCandidate = ref(false)
        let sawSelectionCompleted = ref(false)
        let switchCalled = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectedCandidate(_, _, _)) =>
            sawSelectedCandidate := true
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted) =>
            sawSelectionCompleted := true
          | _ => ()
          }
        )
        await Memento.PreferredCandidate.set(state.memento, Some(uriPath))

        let selectedItem = makePickerItem(
          state,
          Candidate(
            fsPath,
            fsPath,
            {kind: Memento.ResolvedMetadata.ALS(Native, None), timestamp: Date.make(), error: None},
            false,
          ),
        )

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [selectedItem],
          ~hasSelectionChanged=selectedPath =>
            switch Memento.PreferredCandidate.get(state.memento) {
            | Some(path) =>
              !Connection__Candidate.equal(
                Connection__Candidate.make(selectedPath),
                Connection__Candidate.make(path),
              )
            | None => true
            },
          ~switchCandidate=_ => {
            switchCalled := true
            Promise.resolve()
          },
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Test__Util.wait(50)
        view->Connection__UI__Picker.destroy

        Assert.deepStrictEqual(switchCalled.contents, false)
        Assert.deepStrictEqual(sawSelectedCandidate.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, true)
        Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(uriPath))
      },
    )

    Async.it(
      "DeleteDownloads should route through the delete seam",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let sawDestroyed = ref(false)
        let sawSelectionCompleted = ref(false)
        let deleteCalled = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.Destroyed) => sawDestroyed := true
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted) =>
            sawSelectionCompleted := true
          | _ => ()
          }
        )

        let selectedItem = makePickerItem(state, DeleteDownloads)

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [selectedItem],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
          ~deleteDownloads=() => {
            deleteCalled := true
            Promise.resolve({
              Connection__Download__Delete.cleanedDirectories: [],
              failedUris: [],
              deletedInFlightFiles: [],
              failedInFlightFiles: [],
            })
          },
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(deleteCalled.contents, true)
        Assert.deepStrictEqual(sawDestroyed.contents, true)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, true)
      },
    )

    Async.it(
      "DeleteDownloads should invoke the real delete action and remove in-flight downloads",
      async () => {
        await withTempStorage("agda-connection-ui-delete-", async (storagePath, storageUri) => {
          let inFlightPath = NodeJs.Path.join([storagePath, "in-flight.download"])
          NodeJs.Fs.writeFileSync(inFlightPath, NodeJs.Buffer.fromString("partial download"))

          let state = createTestStateWithPlatformAndStorage(makeMockPlatform(), storageUri)
          let view = Connection__UI__Picker.make(state.channels.log)
          let selectedChannel = ref(Connection__Download.Channel.DevALS)
          let sawDestroyed = ref(false)
          let onSelectionCompleted = Log.on(
            state.channels.log,
            log =>
              switch log {
              | Log.SwitchVersionUI(SelectionCompleted) => true
              | _ => false
              },
          )
          let _ = state.channels.log->Chan.on(logEvent =>
            switch logEvent {
            | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
            | _ => ()
            }
          )

          Connection__UI__Handlers.onSelection(
            state,
            makeMockPlatform(),
            selectedChannel,
            _downloadItems => Promise.resolve(),
            view,
            [makePickerItem(state, DeleteDownloads)],
            ~hasSelectionChanged=_ => false,
            ~switchCandidate=_ => Promise.resolve(),
            ~getDownloadItems=_channel => Promise.resolve([]),
            ~deleteDownloads=() => Connection__Switch.deleteDownloads(state),
          )

          await onSelectionCompleted

          Assert.deepStrictEqual(sawDestroyed.contents, true)
          Assert.deepStrictEqual(NodeJs.Fs.existsSync(inFlightPath), false)
        })
      },
    )

    Async.it(
      "hide/reopen behavior should consume pending hides before destroying",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let sawDestroyed = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.Destroyed) => sawDestroyed := true
          | _ => ()
          }
        )

        view.pendingHides = 1
        Connection__UI__Handlers.onHide(view)
        Assert.deepStrictEqual(sawDestroyed.contents, false)

        Connection__UI__Handlers.onHide(view)
        Assert.deepStrictEqual(sawDestroyed.contents, true)
      },
    )

    Async.it(
      "checking-availability placeholder should not trigger selection handling",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let sawSelectionCompleted = ref(false)
        let sawSelectedDownloadAction = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectionCompleted) =>
            sawSelectionCompleted := true
          | Log.SwitchVersionUI(Log.SwitchVersion.SelectedDownloadAction(_, _)) =>
            sawSelectedDownloadAction := true
          | _ => ()
          }
        )

        let selectedItem = makePickerItem(
          state,
          DownloadAction(
            false,
            Connection__UI__Labels.checkingAvailability,
            "native",
          ),
        )

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [selectedItem],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Test__Util.wait(200)

        Assert.deepStrictEqual(sawSelectedDownloadAction.contents, false)
        Assert.deepStrictEqual(sawSelectionCompleted.contents, false)
      },
    )

    Async.it(
      "channel submenu should expose full picker items and placeholder text",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let capturedItems: ref<array<Connection__UI__Channel.pickerItem>> = ref([])
        let capturedPlaceholder = ref("")

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [makePickerItem(state, SelectOtherChannels)],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
          ~showChannelPicker=async (items, placeholder) => {
            capturedItems := items
            capturedPlaceholder := placeholder
            None
          },
        )

        await Test__Util.wait(50)

        Assert.deepStrictEqual(
          capturedItems.contents,
          [
            {label: "Latest", description: "", detail: "Tracks the latest stable release", value: "latest"},
            {
              label: "Development",
              description: "selected",
              detail: "Tracks the latest commit of the master branch",
              value: "dev",
            },
          ],
        )
        Assert.deepStrictEqual(capturedPlaceholder.contents, "Select download channel")

        view->Connection__UI__Picker.destroy
      },
    )

    Async.it(
      "runtime channel picker should receive full QuickPick items",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        let capturedItems: array<{"label": string, "description": string, "detail": string}> =
          %raw(`[]`)
        let resolvePickerCalled = ref((_: unit) => ())
        let pickerCalled = Promise.make((resolve, _) => resolvePickerCalled := resolve)

        let mockShowQuickPick: (
          array<{"label": string, "description": string, "detail": string}>,
          ref<unit => unit>,
        ) => unit = %raw(`function(capturedItems, resolvePickerCalled) {
          globalThis.__savedShowQuickPick2 = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = function(items) {
            if (Array.isArray(items)) items.forEach(function(item) { capturedItems.push(item); });
            resolvePickerCalled.contents();
            return Promise.resolve(undefined);
          };
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick2;
          delete globalThis.__savedShowQuickPick2;
        }`)
        mockShowQuickPick(capturedItems, resolvePickerCalled)

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [makePickerItem(state, SelectOtherChannels)],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await pickerCalled
        restoreShowQuickPick()

        Assert.deepStrictEqual(
          capturedItems->Array.map(i => (i["label"], i["description"], i["detail"])),
          [
            ("Latest", "", "Tracks the latest stable release"),
            ("Development", "selected", "Tracks the latest commit of the master branch"),
          ],
        )

        view->Connection__UI__Picker.destroy
      },
    )

    Async.it(
      "channel submenu rejection should re-show the main picker and preserve future hide cleanup",
      async () => {
        let state = createTestState()
        let view = Connection__UI__Picker.make(state.channels.log)
        let selectedChannel = ref(Connection__Download.Channel.DevALS)

        view->Connection__UI__Picker.onHide(() => Connection__UI__Handlers.onHide(view))

        let showCallCount = ref(0)
        let patchShow: (Connection__UI__Picker.t, ref<int>) => unit = %raw(`function(view, counter) {
          var orig = view.quickPick.show.bind(view.quickPick);
          view.quickPick.show = function() { counter.contents++; return orig(); };
        }`)
        patchShow(view, showCallCount)

        let sawDestroyed = ref(false)
        let _ = state.channels.log->Chan.on(logEvent =>
          switch logEvent {
          | Log.SwitchVersionUI(Destroyed) => sawDestroyed := true
          | _ => ()
          }
        )

        let mockShowQuickPick: (Connection__UI__Picker.t) => unit = %raw(`function(view) {
          globalThis.__savedShowQuickPick = require("vscode").window.showQuickPick;
          require("vscode").window.showQuickPick = function() {
            view.quickPick.hide();
            return Promise.reject(new Error("picker disposed"));
          };
        }`)
        let restoreShowQuickPick: unit => unit = %raw(`function() {
          require("vscode").window.showQuickPick = globalThis.__savedShowQuickPick;
          delete globalThis.__savedShowQuickPick;
        }`)
        mockShowQuickPick(view)

        Connection__UI__Handlers.onSelection(
          state,
          makeMockPlatform(),
          selectedChannel,
          _downloadItems => Promise.resolve(),
          view,
          [makePickerItem(state, SelectOtherChannels)],
          ~hasSelectionChanged=_ => false,
          ~switchCandidate=_ => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Test__Util.wait(200)
        restoreShowQuickPick()

        Assert.deepStrictEqual(sawDestroyed.contents, false)
        Assert.deepStrictEqual(showCallCount.contents, 1)

        view.quickPick->VSCode.QuickPick.show
        view.quickPick->VSCode.QuickPick.hide

        await Test__Util.wait(50)
        Assert.deepStrictEqual(sawDestroyed.contents, true)
      },
    )

    Async.it(
      "candidate selection should preserve raw bare commands in PreferredCandidate",
      async () => {
        let mockAgda =
          await Test__Util.Candidate.Agda.mock(~version="2.7.0.1", ~name="agda-ui-raw")
        let platform: Platform.t = {
          module MockPlatform = {
            include Desktop.Desktop
            let findCommand = (command, ~timeout as _timeout=1000) =>
              switch command {
              | "agda" => Promise.resolve(Ok(mockAgda))
              | "als" => Promise.resolve(Ok(mockAgda))
              | _ => Promise.resolve(Error(Connection__Command.Error.NotFound))
              }
          }
          module(MockPlatform)
        }

        let runSelectionAndAssert = async (selectedPath: string) => {
          let state = createTestStateWithPlatform(platform)
          let view = Connection__UI__Picker.make(state.channels.log)
          let onOperationComplete = Log.on(
            state.channels.log,
            log =>
              switch log {
              | Log.SwitchVersionUI(SelectionCompleted) => true
              | _ => false
              },
          )

          Connection__UI__Handlers.onSelection(
            state,
            platform,
            ref(Connection__Download.Channel.DevALS),
            _downloadItems => Promise.resolve(),
            view,
            [
              makePickerItem(
                state,
                Candidate(
                  selectedPath,
                  selectedPath,
                  {
                    kind: selectedPath == "agda"
                      ? Memento.ResolvedMetadata.Agda(None)
                      : Memento.ResolvedMetadata.ALS(Native, None),
                    timestamp: Date.make(),
                    error: None,
                  },
                  false,
                ),
              ),
            ],
            ~hasSelectionChanged=selectedPath => hasSelectionChanged(state.memento, selectedPath),
            ~switchCandidate=selectedPath =>
              Memento.PreferredCandidate.set(state.memento, Some(selectedPath)),
            ~getDownloadItems=_channel => Promise.resolve([]),
          )

          await onOperationComplete
          view->Connection__UI__Picker.destroy

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), Some(selectedPath))
        }

        await runSelectionAndAssert("agda")
        await runSelectionAndAssert("als")
        await Test__Util.Candidate.Agda.destroy(mockAgda)
      },
    )

    Async.it(
      "handleDownload should not modify PreferredCandidate",
      async () => {
        let testCases = [
          (Some("/usr/bin/agda"), Connection__Download.SelectionVariant.Native, false),
          (Some("/usr/bin/agda"), Connection__Download.SelectionVariant.WASM, false),
          (None, Connection__Download.SelectionVariant.Native, false),
          (None, Connection__Download.SelectionVariant.WASM, false),
        ]

        let runCase = async (
          initialPicked: option<string>,
          variant: Connection__Download.SelectionVariant.t,
          downloaded: bool,
        ) => {
          let state = createTestState()

          await Memento.PreferredCandidate.set(state.memento, initialPicked)

          let activePath = "/opt/homebrew/bin/agda"
          Registry__Connection.status :=
            Active({
              connection: makeMockConnection(activePath, "2.6.3"),
              users: Belt.Set.String.empty,
              currentOwnerId: None,
              queue: Promise.resolve(),
            })

          let expectedDownloadPath = switch variant {
          | Connection__Download.SelectionVariant.Native =>
            NodeJs.Path.join([
              VSCode.Uri.fsPath(state.globalStorageUri),
              "releases",
              "dev",
              "als-dev-Agda-2.8.0-macos-arm64",
              "als",
            ])
          | Connection__Download.SelectionVariant.WASM =>
            VSCode.Uri.toString(
              VSCode.Uri.joinPath(state.globalStorageUri, [
                "releases",
                "dev",
                "als-dev-Agda-2.8.0-wasm",
                "als.wasm",
              ]),
            )
          }
          let platform = Mock.Platform.makeWithSuccessfulDownload(expectedDownloadPath)
          let versionString = "Agda v2.8.0 Language Server (dev build)"

          await Connection__UI__Handlers.handleDownload(
            state,
            platform,
            variant,
            downloaded,
            versionString,
            ~refreshUI=None,
          )

          Assert.deepStrictEqual(Memento.PreferredCandidate.get(state.memento), initialPicked)
          let hasDownloadedPath =
            Config.Connection.getAgdaPaths()->Array.some(path => path == expectedDownloadPath)
          Assert.deepStrictEqual(hasDownloadedPath, true)

          Registry__Connection.status := Empty
        }

        for i in 0 to Array.length(testCases) - 1 {
          let (initialPicked, variant, downloaded) = testCases[i]->Option.getExn
          await runCase(initialPicked, variant, downloaded)
        }
      },
    )

    Async.it(
      "handleDownload downloaded=true WASM on desktop should add managed WASM path to config",
      async () =>
        await withTempStorage("sv-wasm-handoff-", async (tempDir, globalStorageUri) => {
          let logChannel = Chan.make()
          await Config.Connection.setAgdaPaths(logChannel, [])

          let artifactDir = NodeJs.Path.join([tempDir, "releases", "dev", "als-dev-Agda-2.8.0-wasm"])
          let wasmFile = NodeJs.Path.join([artifactDir, "als.wasm"])
          await NodeJs.Fs.mkdir(artifactDir, {recursive: true, mode: 0o777})
          NodeJs.Fs.writeFileSync(wasmFile, NodeJs.Buffer.fromString("mock wasm"))

          let platform: Platform.t = module(Mock.Platform.Basic)
          let state = createTestStateWithPlatformAndStorage(platform, globalStorageUri)

          await Connection__UI__Handlers.handleDownload(
            state,
            platform,
            Connection__Download.SelectionVariant.WASM,
            true,
            "Agda v2.8.0 Language Server (dev build)",
            ~channel=Connection__Download.Channel.DevALS,
            ~refreshUI=None,
          )

          let expectedPath = Connection__Download.uriToPath(VSCode.Uri.file(wasmFile))
          let paths = Config.Connection.getAgdaPaths()
          Assert.deepStrictEqual(paths->Array.some(p => p == expectedPath), true)
        }),
    )

    Async.it(
      "handleChannelSwitch should persist channel selection in memento",
      async () => {
        let state = createTestState()
        let selectedChannel = ref(Connection__Download.Channel.DevALS)

        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), None)

        await Connection__UI__Handlers.handleChannelSwitch(
          state,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(Memento.SelectedChannel.get(state.memento), Some("dev"))
      },
    )

    Async.it(
      "handleChannelSwitch should preserve connection.paths",
      async () => {
        let state = createTestState()
        let logChannel = state.channels.log
        let selectedChannel = ref(Connection__Download.Channel.DevALS)

        let existingPaths = ["/usr/bin/agda", "/downloaded/als-v1", "/downloaded/als-dev"]
        await Config.Connection.setAgdaPaths(logChannel, existingPaths)

        await Connection__UI__Handlers.handleChannelSwitch(
          state,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        Assert.deepStrictEqual(selectedChannel.contents, Connection__Download.Channel.DevALS)
        Assert.deepStrictEqual(Config.Connection.getAgdaPaths(), existingPaths)
      },
    )

    Async.it(
      "handleChannelSwitch plus handleDownload should use the selected channel for manual downloads",
      async () => {
        let state = createTestState()
        let downloadedChannel = ref(None)
        let downloadedPath = VSCode.Uri.toString(
          VSCode.Uri.joinPath(state.globalStorageUri, [
            "releases",
            "dev",
            "als-dev-Agda-2.8.0-wasm",
            "als.wasm",
          ]),
        )

        let platform: Platform.t = {
          module MockPlatform = {
            let determinePlatform = async () => Ok(Connection__Download__Platform.MacOS_Arm)
            let askUserAboutDownloadPolicy = async () => Config.Connection.DownloadPolicy.Yes
            let alreadyDownloaded = _globalStorageUri => Promise.resolve(None)
            let resolveDownloadChannel = Mock.DownloadDescriptor.mockWith(channel =>
              switch channel {
              | Connection__Download.Channel.DevALS =>
                Ok(
                  Connection__Download.Source.FromURL(
                    Connection__Download.Channel.DevALS,
                    "https://example.invalid/dev-als.wasm",
                    "dev-als",
                  ),
                )
              | _ => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
              }
            )
            let download = (_globalStorageUri, source, ~trace as _=Connection__Download__Trace.noop) => {
              downloadedChannel := switch source {
              | Connection__Download.Source.FromURL(ch, _, _) => Some(ch)
              | Connection__Download.Source.FromGitHub(ch, _) => Some(ch)
              }
              Promise.resolve(Ok(downloadedPath))
            }
            let findCommand = (_command, ~timeout as _=1000) =>
              Promise.resolve(Error(Connection__Command.Error.NotFound))
          }
          module(MockPlatform)
        }

        let selectedChannel = ref(Connection__Download.Channel.DevALS)
        await Connection__UI__Handlers.handleChannelSwitch(
          state,
          selectedChannel,
          Connection__Download.Channel.DevALS,
          _downloadItems => Promise.resolve(),
          ~getDownloadItems=_channel => Promise.resolve([]),
        )

        await Connection__UI__Handlers.handleDownload(
          state,
          platform,
          Connection__Download.SelectionVariant.WASM,
          false,
          "ALS vTest",
          ~channel=Connection__Download.Channel.DevALS,
        )

        Assert.deepStrictEqual(downloadedChannel.contents, Some(Connection__Download.Channel.DevALS))
      },
    )

    it("should show DevALS WASM downloaded candidate as dev build before probing", () => {
      let state = createTestState()
      let devALSWasmPath = "/tmp/dev-als/als-dev-Agda-2.8.0-wasm/als.wasm"
      let entry: Memento.ResolvedMetadata.entry = {
        kind: Memento.ResolvedMetadata.ALS(WASM, Some(("dev", "2.8.0", None))),
        timestamp: %raw(`new Date(0)`),
        error: None,
      }
      let item = makePickerItem(
        state,
        Connection__UI__ItemData.Candidate(devALSWasmPath, devALSWasmPath, entry, false),
      )
      Assert.deepStrictEqual(
        item.label,
        "$(squirrel)  Agda 2.8.0 Language Server (dev build) WASM",
      )
    })

    Async.it(
      "should render channel switch item when only unavailable downloads are present",
      async () => {
        let itemData: array<Connection__UI__ItemData.t> =
          Connection__UI__ItemData.entriesToItemData(
            [],
            None,
            [availableDownload(~versionString="ALS v1.0.0", ~variant=Native)],
          )

        let hasSelectOtherChannels =
          itemData->Array.some(item =>
            switch item {
            | SelectOtherChannels => true
            | _ => false
            }
          )

        Assert.deepStrictEqual(hasSelectOtherChannels, true)
      },
    )
  })

  describe("Background update", () => {
    Async.it(
      "backgroundUpdateFailureFallback should swallow updateUI failure",
      async () => {
        module MockDesktopPlatform = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = _ => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let throwingUpdateUI = async (
          _items: array<Connection__Download__Availability.availableDownload>,
        ) =>
          raise(Failure("updateUI failure"))

        await Connection__UI__Handlers.backgroundUpdateFailureFallback(
          module(MockDesktopPlatform),
          throwingUpdateUI,
        )
      },
    )

    Async.it(
      "backgroundUpdateFailureFallback should show unavailable native and WASM items on platform error",
      async () => {
        module MockErrorPlatform = {
          let determinePlatform = () =>
            Promise.resolve(
              Error(
                %raw(`{ os: "unknown", dist: "unknown", codename: "unknown", release: "unknown" }`),
              ),
            )
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = _ => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let capturedItems: ref<array<Connection__Download__Availability.availableDownload>> =
          ref([])
        let mockUpdateUI = async items => capturedItems := items

        await Connection__UI__Handlers.backgroundUpdateFailureFallback(
          module(MockErrorPlatform),
          mockUpdateUI,
        )

        Assert.deepStrictEqual(
          capturedItems.contents,
          [
            availableDownload(
              ~versionString=Connection__UI__Labels.downloadUnavailable,
              ~variant=Native,
            ),
            availableDownload(
              ~versionString=Connection__UI__Labels.downloadUnavailable,
              ~variant=WASM,
            ),
          ],
        )
      },
    )

    Async.it(
      "runBackgroundUpdate should route desktop failures to unavailable native and WASM fallback",
      async () => {
        module MockDesktopPlatform = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Ubuntu))
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = _ => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let failingPromise: promise<array<Connection__Download__Availability.availableDownload>> =
          Promise.reject(Failure("simulated getAllAvailableDownloads failure"))
        let capturedItems: ref<array<Connection__Download__Availability.availableDownload>> =
          ref([])
        let mockUpdateUI = async items => capturedItems := items

        await Connection__UI__Handlers.runBackgroundUpdate(
          failingPromise,
          module(MockDesktopPlatform),
          ~probeVersions=() => Promise.resolve(false),
          mockUpdateUI,
        )

        Assert.deepStrictEqual(
          capturedItems.contents,
          [
            availableDownload(
              ~versionString=Connection__UI__Labels.downloadUnavailable,
              ~variant=Native,
            ),
            availableDownload(
              ~versionString=Connection__UI__Labels.downloadUnavailable,
              ~variant=WASM,
            ),
          ],
        )
      },
    )

    Async.it(
      "runBackgroundUpdate should route web failures to unavailable WASM-only fallback",
      async () => {
        module MockWebPlatform = {
          let determinePlatform = () => Promise.resolve(Ok(Connection__Download__Platform.Web))
          let findCommand = (_, ~timeout as _=1000) =>
            Promise.resolve(Error(Connection__Command.Error.NotFound))
          let alreadyDownloaded = _ => Promise.resolve(None)
          let resolveDownloadChannel = (_, _) =>
            async (_, _, _) => Error(Connection__Download.Error.CannotFindCompatibleALSRelease)
          let download = (_, _, ~trace as _=Connection__Download__Trace.noop) =>
            Promise.resolve(Error(Connection__Download.Error.CannotFindCompatibleALSRelease))
          let askUserAboutDownloadPolicy = () =>
            Promise.resolve(Config.Connection.DownloadPolicy.Yes)
        }

        let failingPromise: promise<array<Connection__Download__Availability.availableDownload>> =
          Promise.reject(Failure("simulated getAllAvailableDownloads failure"))
        let capturedItems: ref<array<Connection__Download__Availability.availableDownload>> =
          ref([])
        let mockUpdateUI = async items => capturedItems := items

        await Connection__UI__Handlers.runBackgroundUpdate(
          failingPromise,
          module(MockWebPlatform),
          ~probeVersions=() => Promise.resolve(false),
          mockUpdateUI,
        )

        Assert.deepStrictEqual(
          capturedItems.contents,
          [
            availableDownload(
              ~versionString=Connection__UI__Labels.downloadUnavailable,
              ~variant=WASM,
            ),
          ],
        )
      },
    )
  })

  describe("Labels", () => {
    open Connection__UI__Labels

    let makeEntry = (kind: Memento.ResolvedMetadata.kind, ~error=None): Memento.ResolvedMetadata.entry => {
      kind,
      timestamp: Date.make(),
      error,
    }

    describe("download string constants", () => {
      it("downloadNativeALS", () =>
        Assert.deepStrictEqual(
          downloadNativeALS,
          "$(cloud-download)  Download Agda Language Server (native)",
        )
      )
      it("downloadWasmALS", () =>
        Assert.deepStrictEqual(
          downloadWasmALS,
          "$(cloud-download)  Download Agda Language Server (WASM)",
        )
      )
      it("downloadUnavailable", () =>
        Assert.deepStrictEqual(downloadUnavailable, "Not available for this platform")
      )
      it("checkingAvailability", () =>
        Assert.deepStrictEqual(checkingAvailability, "Checking availability...")
      )
      it("downloadedAndInstalled", () =>
        Assert.deepStrictEqual(downloadedAndInstalled, "Downloaded and installed")
      )
      it("deleteDownloads", () =>
        Assert.deepStrictEqual(deleteDownloads, "$(trash)  Delete downloads")
      )
      it("selectOtherChannels", () =>
        Assert.deepStrictEqual(selectOtherChannels, "$(tag)  Select other channels")
      )
    })

    describe("candidateDisplayInfo", () => {
      it("Agda with known version", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("agda", makeEntry(Agda(Some("2.8.0")))),
          ("Agda 2.8.0", None),
        )
      )
      it("Agda with unknown version", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("agda", makeEntry(Agda(None))),
          ("Agda (version unknown)", None),
        )
      )
      it("ALS native dev build", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(ALS(Native, Some(("dev", "2.8.0", None))))),
          ("$(squirrel)  Agda 2.8.0 Language Server (dev build)", None),
        )
      )
      it("ALS native versioned", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(ALS(Native, Some(("v6", "2.8.0", None))))),
          ("$(squirrel)  Agda 2.8.0 Language Server v6", None),
        )
      )
      it("ALS WASM dev build", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(ALS(WASM, Some(("dev", "2.8.0", None))))),
          ("$(squirrel)  Agda 2.8.0 Language Server (dev build) WASM", None),
        )
      )
      it("ALS WASM versioned", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(ALS(WASM, Some(("v6", "2.8.0", None))))),
          ("$(squirrel)  Agda 2.8.0 Language Server v6 WASM", None),
        )
      )
      it("ALS native version unknown", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(ALS(Native, None))),
          ("$(squirrel)  Agda Language Server (version unknown)", None),
        )
      )
      it("ALS WASM version unknown", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(ALS(WASM, None))),
          ("$(squirrel)  Agda Language Server (version unknown) WASM", None),
        )
      )
      it("Unknown with error", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(Unknown, ~error=Some("bad binary"))),
          ("$(error) als", Some("Error: bad binary")),
        )
      )
      it("Unknown without error", () =>
        Assert.deepStrictEqual(
          candidateDisplayInfo("als", makeEntry(Unknown)),
          ("$(question) als", Some("Unknown executable")),
        )
      )
    })

    describe("channel labels and details", () => {
      it("channelLabel LatestALS", () =>
        Assert.deepStrictEqual(channelLabel(Connection__Download.Channel.LatestALS), "Latest")
      )
      it("channelLabel DevALS", () =>
        Assert.deepStrictEqual(channelLabel(Connection__Download.Channel.DevALS), "Development")
      )
      it("channelDetail LatestALS", () =>
        Assert.deepStrictEqual(
          channelDetail(Connection__Download.Channel.LatestALS),
          "Tracks the latest stable release",
        )
      )
      it("channelDetail DevALS", () =>
        Assert.deepStrictEqual(
          channelDetail(Connection__Download.Channel.DevALS),
          "Tracks the latest commit of the master branch",
        )
      )
    })

    describe("download header", () => {
      it("DevALS", () =>
        Assert.deepStrictEqual(
          downloadHeader(Connection__Download.Channel.DevALS),
          "Download (Development)",
        )
      )
      it("LatestALS", () =>
        Assert.deepStrictEqual(
          downloadHeader(Connection__Download.Channel.LatestALS),
          "Download (Latest)",
        )
      )
    })
  })

})
