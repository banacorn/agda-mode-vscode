// bindings of module 'vscode-test'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
};

[@bs.module "vscode-test"]
external runTests: options => Js.Promise.t(int) = "runTests";

let dirname: option(string) = [%bs.node __dirname];

let testSuiteAdapterFileName = "TestSuiteAdapter.bs.js";

let run = dirname => {
  // The folder containing the Extension Manifest package.json
  // Passed to `--extensionDevelopmentPath`
  let extensionDevelopmentPath = Node.Path.resolve(dirname, "../");
  // The path to the extension test script
  // Passed to --extensionTestsPath
  let extensionTestsPath =
    Node.Path.resolve(dirname, testSuiteAdapterFileName);

  // Download VS Code, unzip it and run the integration test
  runTests({extensionDevelopmentPath, extensionTestsPath});
};

switch (dirname) {
| None =>
  Js.log("Failed to read __dirname");
  Node.Process.exit(1);
| Some(dirname) =>
  run(dirname)
  |> Js.Promise.catch(error => {
       Js.log2("Promise rejection ", error);
       Js.Promise.resolve(1);
     })
  |> Js.Promise.then_(Node.Process.exit)
};
