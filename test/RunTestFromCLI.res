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
let tempRunId = {
  let timestamp = Js.Date.now()->Float.toString
  let random = Js.Math.random()->Float.toString->Js.String2.sliceToEnd(~from=2)
  "agda-mode-vscode-test-" ++ timestamp ++ "-" ++ random
}
let tempTestRoot = NodeJs.Path.join([NodeJs.Os.tmpdir(), tempRunId])
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
