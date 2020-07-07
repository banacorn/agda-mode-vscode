// bindings of module 'vscode-test'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
};

[@bs.module "vscode-test"]
external runTests: options => Promise.t(result(unit, exn)) = "runTests";

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

Node.Process.exit(1);

// switch (dirname) {
// | None =>
//   Js.log("Failed to read __dirname");
//   Node.Process.exit(1);
// | Some(dirname) =>
//   switch (
//     run(dirname)
//     ->Promise.get(
//         fun
//         | Error(_exn) => Node.Process.exit(1)
//         | Ok () => Node.Process.exit(0),
//       )
//   ) {
//   | () => ()
//   | exception _ => Node.Process.exit(42)
//   }
// |> Js.Promise.catch(error => {
//      Js.log2("Promise rejection ", error);
//      Js.Promise.resolve(1);
//    })
// |> Js.Promise.then_(Node.Process.exit)
// };
