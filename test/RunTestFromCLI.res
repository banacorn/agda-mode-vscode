// bindings of module 'vscode-test'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
}

@bs.module("vscode-test")
external runTests: options => Js.Promise.t<bool> = "runTests"

let dirname: option<string> = %bs.node(__dirname)

let testSuiteAdapterFileName = "TestSuiteAdapter.bs.js"

// The folder containing the Extension Manifest package.json
// Passed to `--extensionDevelopmentPath`
let extensionDevelopmentPath = switch dirname {
| None => Node.Path.resolve(Node.Process.cwd(), "../")
| Some(dirname) => Node.Path.resolve(dirname, "../")
}

// The path to the extension test script
// Passed to --extensionTestsPath
let extensionTestsPath = switch dirname {
| None => Node.Path.resolve(Node.Process.cwd(), testSuiteAdapterFileName)
| Some(dirname) => Node.Path.resolve(dirname, testSuiteAdapterFileName)
}

Js.log(
  "Running from the CLI, with\n  extensionDevelopmentPath: " ++
  (extensionDevelopmentPath ++
  ("\n  extensionTestsPath: " ++ extensionTestsPath)),
)

Js.Promise.catch(error => {
  Js.log(error)
  Js.log("Failed to run tests")
  Node.Process.exit(1)
}, runTests({
  extensionDevelopmentPath: extensionDevelopmentPath,
  extensionTestsPath: extensionTestsPath,
}))->ignore
