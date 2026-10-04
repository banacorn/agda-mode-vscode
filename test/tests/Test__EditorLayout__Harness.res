open Mocha

// Harness for tests that run the real `agda-mode.load` against a prepared
// editor-group layout and compare the layout that results.
//
// Groups are named after the file they hold (`Load.agda` -> "Load"), the Agda
// panel is named "Agda", and a group without any tab is named "Empty".

module Layout = Test__EditorLayout__Util

////////////////////////////////////////////////////////////////////////////////
// layout trees
////////////////////////////////////////////////////////////////////////////////

// `vscode.getEditorLayout` / `vscode.setEditorLayout` payloads
type rec rawGroup = {groups?: array<rawGroup>, size?: float}
type rawLayout = {orientation: int, groups: array<rawGroup>}

let rec leaves = (tree: EditorLayout.t<'a>): array<'a> =>
  switch tree {
  | Group(x) => [x]
  | Split(_, children) => children->Array.flatMap(child => leaves(child.tree))
  }

let rec mapLeaves = (tree: EditorLayout.t<'a>, f: 'a => 'b): EditorLayout.t<'b> =>
  switch tree {
  | Group(x) => Group(f(x))
  | Split(orientation, children) =>
    Split(orientation, children->Array.map(child => {...child, tree: mapLeaves(child.tree, f)}))
  }

let round2 = x => Math.round(x *. 100.0) /. 100.0

// proportions per sibling list, rounded to two decimals
let rec normalize = (tree: EditorLayout.t<'a>): EditorLayout.t<'a> =>
  switch tree {
  | Group(_) => tree
  | Split(orientation, children) =>
    let total = children->Array.reduce(0.0, (acc, child) => acc +. child.size)
    Split(
      orientation,
      children->Array.map(child => {
        EditorLayout.size: round2(child.size /. total),
        tree: normalize(child.tree),
      }),
    )
  }

let flip = (orientation: EditorLayout.orientation): EditorLayout.orientation =>
  switch orientation {
  | LeftRight => TopBottom
  | TopBottom => LeftRight
  }

let orientationOfInt = n => n == 0 ? EditorLayout.LeftRight : EditorLayout.TopBottom
let intOfOrientation = (orientation: EditorLayout.orientation) =>
  switch orientation {
  | LeftRight => 0
  | TopBottom => 1
  }

// leaves are numbered 1, 2, 3 ... in depth-first order, which is also the
// order of the view columns
let parseLayout = (raw: rawLayout): EditorLayout.t<int> => {
  let counter = ref(0)
  let rec go = (orientation, groups: array<rawGroup>): EditorLayout.t<int> =>
    Split(
      orientation,
      groups->Array.map(group => {
        let tree = switch group.groups {
        | Some(children) if Array.length(children) > 0 => go(flip(orientation), children)
        | _ =>
          counter := counter.contents + 1
          EditorLayout.Group(counter.contents)
        }
        {EditorLayout.size: group.size->Option.getOr(1.0), tree}
      }),
    )
  go(orientationOfInt(raw.orientation), raw.groups)
}

let rec toRawGroup = (sized: EditorLayout.sized<'a>): rawGroup =>
  switch sized.tree {
  | Group(_) => {size: sized.size}
  | Split(_, children) => {groups: children->Array.map(toRawGroup), size: sized.size}
  }

let toRawLayout = (tree: EditorLayout.t<'a>): rawLayout =>
  switch tree {
  | Group(_) => {orientation: 0, groups: [{size: 1.0}]}
  | Split(orientation, children) => {
      orientation: intOfOrientation(orientation),
      groups: children->Array.map(toRawGroup),
    }
  }

////////////////////////////////////////////////////////////////////////////////
// observing the editor
////////////////////////////////////////////////////////////////////////////////

type tab = {kind: string, label: string}
type tabGroup = {viewColumn: int, isActive: bool, tabs: array<tab>}

let snapshotTabGroups: unit => array<tabGroup> = %raw(`function() {
  const vscode = require("vscode");
  return vscode.window.tabGroups.all.map(group => ({
    viewColumn: group.viewColumn,
    isActive: group.isActive,
    tabs: group.tabs.map(tab => ({
      kind: tab.input instanceof vscode.TabInputWebview
        ? "webview"
        : tab.input instanceof vscode.TabInputText
          ? "text"
          : "other",
      label: tab.label,
    })),
  }));
}`)

let nameOfGroup = (group: tabGroup) =>
  switch group.tabs {
  | [] => "Empty"
  | tabs => tabs->Array.map(tab => tab.label->String.replace(".agda", ""))->Array.join("+")
  }

type snapshot = {
  // the layout with its groups named, as proportions
  layout: EditorLayout.t<string>,
  // the name of the group that has focus
  activeGroup: string,
  // the file name of the active text editor
  activeEditor: option<string>,
}

// Pairs the depth-first leaves of `getEditorLayout` with the view columns of
// `window.tabGroups` in ascending order (the order of `tabGroups.all` is not
// documented to follow the layout tree).
let capture = async () => {
  let raw: rawLayout = await VSCode.Commands.executeCommand0("vscode.getEditorLayout")
  let groups =
    snapshotTabGroups()->Array.toSorted((a, b) => Int.compare(a.viewColumn, b.viewColumn))
  let indexed = parseLayout(raw)
  if Array.length(leaves(indexed)) != Array.length(groups) {
    raise(
      Failure(
        `the layout has ${leaves(indexed)->Array.length->Int.toString} groups but window.tabGroups has ${groups
          ->Array.length
          ->Int.toString}`,
      ),
    )
  }
  let layout = indexed->mapLeaves(index =>
    switch groups[index - 1] {
    | Some(group) => nameOfGroup(group)
    | None => raise(Failure(`no tab group for leaf ${Int.toString(index)}`))
    }
  )
  {
    layout: normalize(layout),
    activeGroup: groups->Array.find(group => group.isActive)->Option.mapOr("", nameOfGroup),
    activeEditor: VSCode.Window.activeTextEditor->Option.map(editor =>
      editor
      ->VSCode.TextEditor.document
      ->VSCode.TextDocument.fileName
      ->NodeJs.Path.basename
    ),
  }
}

// VS Code has no event for layout changes, but `vscode.setEditorLayout`
// applies the layout synchronously, so the promise it returns resolves exactly
// when the layout is in place. The extension drops that promise, so the
// observer records it by wrapping `vscode.commands.executeCommand`. The Agda
// panel is opened asynchronously, and `onDidChangeTabs` says when that is done.
//
// `vscode.getEditorLayout` is recorded as well. An extension that reads the
// layout before it sets one only calls `setEditorLayout` once the read has
// resolved, and waiting for the read keeps that later call from being missed.
module Observer = {
  type t

  let make: unit => t = %raw(`function() {
    const vscode = require("vscode");
    const commands = vscode.commands;
    const original = commands.executeCommand;
    const layoutCalls = [];
    let panelOpened = false;
    let announcePanelOpened;
    const panelOpenedPromise = new Promise(resolve => { announcePanelOpened = resolve; });

    commands.executeCommand = function(id, ...args) {
      const result = original.call(commands, id, ...args);
      if (id === "vscode.setEditorLayout" || id === "vscode.getEditorLayout") {
        layoutCalls.push(Promise.resolve(result));
      }
      return result;
    };

    const subscription = vscode.window.tabGroups.onDidChangeTabs(event => {
      const opened = event.opened.some(tab =>
        tab.input instanceof vscode.TabInputWebview && tab.label === "Agda");
      if (opened) {
        panelOpened = true;
        announcePanelOpened();
      }
    });

    return {
      // Resolves once every recorded layout call has resolved, including the
      // ones made after an earlier one resolved, and, if asked to, the Agda
      // tab has opened. The timeout only guards against a hang; it never
      // decides that the layout is ready.
      settled: function(timeoutMs, waitForPanel) {
        const work = (async () => {
          if (waitForPanel) {
            await panelOpenedPromise;
          }
          let seen = 0;
          while (seen < layoutCalls.length) {
            const batch = layoutCalls.slice(seen);
            seen = layoutCalls.length;
            await Promise.all(batch);
          }
        })();
        let timer;
        const guard = new Promise((_, reject) => {
          timer = setTimeout(() => reject(new Error(
            "the layout did not settle within " + timeoutMs + "ms (Agda tab opened: " +
            panelOpened + ", layout calls: " + layoutCalls.length + ")")), timeoutMs);
        });
        return Promise.race([work, guard]).finally(() => clearTimeout(timer));
      },
      stop: function() {
        commands.executeCommand = original;
        subscription.dispose();
      },
    };
  }`)

  @send external settled: (t, int, bool) => promise<unit> = "settled"
  @send external stop: t => unit = "stop"
}

let hangGuardMs = 20000

// Resolves once no Agda panel tab is left. The panel is disposed
// synchronously, but its tab is closed asynchronously.
let panelClosed: int => promise<unit> = %raw(`function(timeoutMs) {
  const vscode = require("vscode");
  const exists = () => vscode.window.tabGroups.all.some(group =>
    group.tabs.some(tab =>
      tab.input instanceof vscode.TabInputWebview && tab.label === "Agda"));
  if (!exists()) {
    return Promise.resolve();
  }
  return new Promise((resolve, reject) => {
    const timer = setTimeout(() => {
      subscription.dispose();
      const tabs = vscode.window.tabGroups.all.map(group => ({
        column: group.viewColumn,
        tabs: group.tabs.map(tab => ({
          label: tab.label,
          input: tab.input && tab.input.constructor ? tab.input.constructor.name : typeof tab.input,
        })),
      }));
      reject(new Error("the Agda panel tab did not close within " + timeoutMs + "ms; tabs: " + JSON.stringify(tabs)));
    }, timeoutMs);
    const subscription = vscode.window.tabGroups.onDidChangeTabs(() => {
      if (!exists()) {
        clearTimeout(timer);
        subscription.dispose();
        resolve();
      }
    });
  });
}`)

////////////////////////////////////////////////////////////////////////////////
// driving the editor
////////////////////////////////////////////////////////////////////////////////

// a global setting to set for the length of a scenario
type setting = {section: string, key: string, value: Obj.t}
let stringSetting = (section, key, value: string): setting => {
  section,
  key,
  value: Obj.magic(value),
}
let boolSetting = (section, key, value: bool): setting => {section, key, value: Obj.magic(value)}

// resolves to the previous global value, `undefined` if there was none
let updateGlobalSetting: (string, string, Obj.t) => promise<Obj.t> = %raw(`
  async function(section, key, value) {
    const vscode = require("vscode");
    const configuration = vscode.workspace.getConfiguration(section);
    const inspected = configuration.inspect(key);
    const previous = inspected ? inspected.globalValue : undefined;
    await configuration.update(key, value, vscode.ConfigurationTarget.Global);
    return previous;
  }`)

let showInColumn: (string, int) => promise<unit> = %raw(`
  async function(path, column) {
    const vscode = require("vscode");
    await vscode.window.showTextDocument(vscode.Uri.file(path), {
      viewColumn: column,
      preview: false,
      preserveFocus: false,
    });
  }`)

let describeExn: exn => string = %raw(`function(e) {
  return (e && e._1 && e._1.message) || (e && e.message) || String(e);
}`)

let positionName = (position: Config.View.mountAt) =>
  switch position {
  | Bottom => "bottom"
  | Right => "right"
  }

let fileOf = name => Test__Util.Path.asset(name ++ ".agda")

let resetLayout = async () => {
  let _ = await VSCode.Commands.executeCommand0("workbench.action.closeAllEditors")
  let _ = await VSCode.Commands.executeCommand1(
    "vscode.setEditorLayout",
    {orientation: 0, groups: [{size: 1.0}]},
  )
}

// Rejects if `promise` has not settled within the hang guard.
let withHangGuard = (~what, promise) => {
  let guard = Promise.make((_, reject) => {
    let _ = Js.Global.setTimeout(
      () => reject(Failure(`timed out waiting for ${what}`)),
      hangGuardMs,
    )
  })
  Promise.race([promise, guard])
}

// logged once for every state the extension has finished destroying
let isDestructionComplete = log =>
  switch log {
  | Log.Others("State.destroy: Connection released, destruction complete") => true
  | _ => false
  }

// The files `agda-mode.load` has run on since the last reset.
let loadedFiles: ref<array<string>> = ref([])

// Quits through the command, as a user would: the extension runs from its own
// webpack bundle, so the `Registry` and `Singleton` imported here are not the
// ones holding its states and panel.
let quit = async (channels: State.channels, filepath) => {
  let _ = await Test__Util.File.open_(filepath)
  let destroyCompleted = Log.on(channels.log, isDestructionComplete)
  switch await Test__Util.executeCommand("agda-mode.quit") {
  | None | Some(Ok(_)) => ()
  | Some(Error(error)) =>
    let (header, body) = Connection.Error.toString(error)
    raise(Failure(header ++ "\n" ++ body))
  }
  await withHangGuard(~what=`${filepath} to be destroyed`, destroyCompleted)
}

// Closes the Agda panel tab, as a user would. The extension then destroys what
// it holds when the panel is disposed.
let closePanelTabs: unit => promise<unit> = %raw(`async function() {
  const vscode = require("vscode");
  const tabs = vscode.window.tabGroups.all.flatMap(group =>
    group.tabs.filter(tab =>
      tab.input instanceof vscode.TabInputWebview && tab.label === "Agda"));
  if (tabs.length > 0) {
    await vscode.window.tabGroups.close(tabs);
  }
}`)

// Quits every loaded file, then closes the panel tab and restores one group.
// Quitting alone does not remove the panel while the registry still holds an
// entry for another Agda file that was merely opened, so the tab is closed
// explicitly.
let reset = async (channels: State.channels) => {
  let files = loadedFiles.contents
  loadedFiles := []
  let unique = files->Array.filterWithIndex((file, index) => files->Array.indexOf(file) == index)
  for index in 0 to Array.length(unique) - 1 {
    switch unique[index] {
    | Some(file) => await quit(channels, file)
    | None => ()
    }
  }
  await closePanelTabs()
  await panelClosed(hangGuardMs)
  await resetLayout()
}

// Keep the sidebar and the bottom panel out of the way, so that every group
// stays above VS Code's minimum group size and the proportions asked for are
// the proportions we get.
let clearWorkbench = async () => {
  let _ = await VSCode.Commands.executeCommand0("workbench.action.closeSidebar")
  let _ = await VSCode.Commands.executeCommand0("workbench.action.closePanel")
  let _ = await VSCode.Commands.executeCommand0("workbench.action.closeAuxiliaryBar")
}

// VS Code will not shrink a group below roughly this width, and doing so
// would bend the proportions a test asks for. The narrowest slot any case asks
// for is a quarter of the editor area.
let minGroupWidth = 220.0
let smallestSlot = 0.25

// The width of the editor area in pixels, read from a layout of two columns:
// `getEditorLayout` reports sizes in pixels.
let measureEditorWidth = async () => {
  let _ = await VSCode.Commands.executeCommand1(
    "vscode.setEditorLayout",
    {orientation: 0, groups: [{size: 1.0}, {size: 1.0}]},
  )
  let raw: rawLayout = await VSCode.Commands.executeCommand0("vscode.getEditorLayout")
  raw.groups->Array.reduce(0.0, (total, group) => total +. group.size->Option.getOr(0.0))
}

let assertEditorWideEnough = async () => {
  let width = await measureEditorWidth()
  await resetLayout()
  let required = minGroupWidth /. smallestSlot
  if width < required {
    raise(
      Failure(
        `the editor area is ${Float.toString(width)}px wide but ${Float.toString(
            required,
          )}px is needed for a group to get a quarter of it without falling below VS Code's minimum group width`,
      ),
    )
  }
}

// Sets the mount position, `workbench.editor.splitSizing` (pinned so that the
// sizes VS Code chooses are the same on every machine) and any `settings`,
// resets before and after the body, and restores the settings even when the
// body fails.
let withScenario = async (~position, ~splitSizing="split", ~settings=[], body) => {
  let candidate = await Test__Util.AgdaMode.commandExists("agda")
  let channels = await Test__Util.activateExtension(Some(candidate))
  let applied = Array.concat(
    [
      stringSetting("agdaMode", "view.panelMountPosition", positionName(position)),
      stringSetting("workbench.editor", "splitSizing", splitSizing),
    ],
    settings,
  )
  let previous: array<(setting, Obj.t)> = []
  for index in 0 to Array.length(applied) - 1 {
    switch applied[index] {
    | Some(setting) =>
      let before = await updateGlobalSetting(setting.section, setting.key, setting.value)
      previous->Array.push((setting, before))
    | None => ()
    }
  }
  let failure = try {
    await reset(channels)
    await clearWorkbench()
    await assertEditorWideEnough()
    await body(channels)
    None
  } catch {
  | exn => Some(exn)
  }
  let cleanupFailure = try {
    await reset(channels)
    None
  } catch {
  | exn => Some(exn)
  }
  for index in Array.length(previous) - 1 downto 0 {
    switch previous[index] {
    | Some((setting, before)) =>
      let _ = await updateGlobalSetting(setting.section, setting.key, before)
    | None => ()
    }
  }
  switch (failure, cleanupFailure) {
  | (Some(exn), Some(cleanupExn)) =>
    // the body's failure is the one to report, but do not lose the other
    Js.log2("[ issue #129 harness: cleanup also failed ]", describeExn(cleanupExn))
    raise(exn)
  | (Some(exn), None) => raise(exn)
  | (None, Some(exn)) => raise(exn)
  | (None, None) => ()
  }
}

// creates the groups of `layout` and opens a file in each non-empty one
let build = async (layout: EditorLayout.t<string>) => {
  let names = leaves(layout)
  let _ = await VSCode.Commands.executeCommand1(
    "vscode.setEditorLayout",
    toRawLayout(normalize(layout)),
  )
  for index in 0 to Array.length(names) - 1 {
    switch names[index] {
    | Some("Empty") | None => ()
    | Some(name) => await showInColumn(fileOf(name), index + 1)
    }
  }
}

// gives focus to the group holding `name`
let focus = async (layout: EditorLayout.t<string>, name) => {
  let column = leaves(layout)->Array.findIndex(candidate => candidate == name) + 1
  await showInColumn(fileOf(name), column)
}

// runs the real `agda-mode.load` on the active editor
let load = async (channels: State.channels) => {
  VSCode.Window.activeTextEditor->Option.forEach(editor =>
    loadedFiles :=
      Array.concat(
        loadedFiles.contents,
        [editor->VSCode.TextEditor.document->VSCode.TextDocument.fileName],
      )
  )
  let (handled, resolve, _) = Util.Promise_.pending()
  let stop = channels.commandHandled->Chan.on(command =>
    if command == Command.Load {
      resolve()
    }
  )
  switch await Test__Util.executeCommand("agda-mode.load") {
  | None => raise(Failure("agda-mode.load did not return a state"))
  | Some(Error(error)) =>
    let (header, body) = Connection.Error.toString(error)
    raise(Failure(header ++ "\n" ++ body))
  | Some(Ok(_)) => await handled
  }
  stop()
}

// Runs `agda-mode.load`, waits until the layout it asked for is in place and,
// unless the panel is expected to be reused, the Agda panel has opened, then
// captures the layout.
let loadAndSettle = async (~panelOpens=true, channels: State.channels) => {
  let observer = Observer.make()
  let failure = try {
    await load(channels)
    await observer->Observer.settled(hangGuardMs, panelOpens)
    None
  } catch {
  | exn => Some(exn)
  }
  observer->Observer.stop
  switch failure {
  | Some(exn) => raise(exn)
  | None => await capture()
  }
}

// Builds `layout`, focuses `active`, checks that the setup is what we meant
// (so a wrong assumption fails here and not in the assertion), loads, and
// compares the settled layout with `expected`, with or without the sizes.
let check = async (
  ~layout,
  ~active,
  ~position,
  ~splitSizing="split",
  ~settings=[],
  ~shapeOnly=false,
  ~expected,
) =>
  await withScenario(~position, ~splitSizing, ~settings, async channels => {
    await build(layout)
    await focus(layout, active)
    let before = await capture()
    Assert.deepStrictEqual(Layout.show(before.layout), Layout.show(normalize(layout)))
    Assert.deepStrictEqual(before.activeGroup, active)

    let after = await loadAndSettle(channels)
    Assert.deepStrictEqual(
      shapeOnly ? Layout.showShape(after.layout) : Layout.show(after.layout),
      expected,
    )
  })
