// bindings of module '@vscode/test-electron'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
}

@module("@vscode/test-electron")
external runTests: options => promise<bool> = "runTests"

let testSuiteAdapterFileName = "TestSuiteAdapter.bs.js"

// The folder containing the Extension Manifest package.json
// Passed to `--extensionDevelopmentPath`
let extensionDevelopmentPath = NodeJs.Path.resolve([NodeJs.Global.dirname, "../"])

// The path to the extension test script
// Passed to --extensionTestsPath
let extensionTestsPath = NodeJs.Path.resolve([NodeJs.Global.dirname, testSuiteAdapterFileName])

Js.log(
  "Running from the CLI, with\n  extensionDevelopmentPath: " ++
  (extensionDevelopmentPath ++
  ("\n  extensionTestsPath: " ++ extensionTestsPath)),
)

try {
  runTests({
    extensionDevelopmentPath,
    extensionTestsPath,
  })->Promise.finally(() => {
    NodeJs.Process.process->NodeJs.Process.exitWithCode(0)
  })->ignore
} catch {
| error =>
  Js.log(error)
  Js.log("Failed to run tests")
  NodeJs.Process.process->NodeJs.Process.exitWithCode(1)
}

