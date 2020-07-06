// bindings of module 'vscode-test'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
};

[@bs.module "vscode-test"]
external runTests: options => Promise.t(int) = "runTests";

// this script should returns a promise that resolves to some exit status
let (promise, resolve) = Promise.pending();

let dirname: option(string) = [%bs.node __dirname];

switch (dirname) {
| None =>
  Js.log("Failed to read __dirname");
  resolve(1);
| Some(dirname) =>
  // The folder containing the Extension Manifest package.json
  // Passed to `--extensionDevelopmentPath`
  let extensionDevelopmentPath = Node.Path.resolve(dirname, "../");
  // The path to the extension test script
  // Passed to --extensionTestsPath
  let extensionTestsPath = Node.Path.resolve(dirname, "./suite/index");
  // Download VS Code, unzip it and run the integration test
  runTests({extensionDevelopmentPath, extensionTestsPath})
  ->Promise.get(resolve);
};

promise;
