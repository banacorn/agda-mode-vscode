// bindings of module '@vscode/test-electron'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
  launchArgs?: array<string>,
}

@module("@vscode/test-electron")
external runTests: options => promise<unit> = "runTests"

let testSuiteAdapterFileName = "TestSuiteAdapter.bs.js"

// The folder containing the Extension Manifest package.json
// Passed to `--extensionDevelopmentPath`
let extensionDevelopmentPath = NodeJs.Path.resolve([NodeJs.Global.dirname, "../../../"])

// The path to the extension test script
// Passed to --extensionTestsPath
let extensionTestsPath = NodeJs.Path.resolve([NodeJs.Global.dirname, testSuiteAdapterFileName])

// Keep the per-run temp root short. VS Code creates IPC socket paths under
// `--user-data-dir`, and long test directory names can exceed the Unix socket
// path limit and abort the test run before the extension host is usable.
let tempRunId = {
  let timestamp = Js.Date.now()->Float.toString->Js.String2.slice(~from=0, ~to_=10)
  let random = Js.Math.random()->Float.toString->Js.String2.slice(~from=2, ~to_=8)
  "agda-" ++ timestamp ++ "-" ++ random
}

// VS Code creates IPC sockets below --user-data-dir. On Unix-like systems,
// os.tmpdir() can be long enough to exceed the socket path limit, so prefer /tmp.
let tempRootBase = if OS.onUnix && NodeJs.Fs.existsSync("/tmp") {
  "/tmp"
} else {
  NodeJs.Os.tmpdir()
}
let tempTestRoot = NodeJs.Path.join([tempRootBase, tempRunId])
let testUserDataDir = NodeJs.Path.join([tempTestRoot, "user-data"])
let testExtensionsDir = NodeJs.Path.join([tempTestRoot, "extensions"])

Js.log(
  "Running from the CLI, with\n  extensionDevelopmentPath: " ++
  (extensionDevelopmentPath ++
  ("\n  extensionTestsPath: " ++ extensionTestsPath)),
)

NodeJs.Fs.mkdirSyncWith(tempTestRoot, {recursive: true})
NodeJs.Fs.mkdirSyncWith(testUserDataDir, {recursive: true})
NodeJs.Fs.mkdirSyncWith(testExtensionsDir, {recursive: true})

runTests(
  {
    extensionDevelopmentPath,
    extensionTestsPath,
    launchArgs: [
      "--user-data-dir=" ++ testUserDataDir,
      "--extensions-dir=" ++ testExtensionsDir,
    ],
  },
)->Promise.done
