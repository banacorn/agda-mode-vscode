// bindings of module 'vscode-test'
type options = {
  extensionDevelopmentPath: string,
  extensionTestsPath: string,
};

[@bs.module "vscode-test"]
external runTests: options => Js.Promise.t(bool) = "runTests";

let dirname: option(string) = [%bs.node __dirname];

let testSuiteAdapterFileName = "TestSuiteAdapter.bs.js";

// The folder containing the Extension Manifest package.json
// Passed to `--extensionDevelopmentPath`
let extensionDevelopmentPath =
  switch (dirname) {
  | None => Node.Path.resolve(Node.Process.cwd(), "../")
  | Some(dirname) => Node.Path.resolve(dirname, "../")
  };

// The path to the extension test script
// Passed to --extensionTestsPath
let extensionTestsPath =
  switch (dirname) {
  | None => Node.Path.resolve(Node.Process.cwd(), testSuiteAdapterFileName)
  | Some(dirname) => Node.Path.resolve(dirname, testSuiteAdapterFileName)
  };

runTests({extensionDevelopmentPath, extensionTestsPath})
|> Js.Promise.catch(error => {
     Js.log(error);
     Js.log("Failed to run tests");
     Node.Process.exit(1);
     //  Js.Promise.resolve(1);
   });
// |> Js.Promise.then_(
//      fun
//      | true => Node.Process.exit(0)
//      | false => Node.Process.exit(1),
//    );

// let run = dirname => {
//   // The folder containing the Extension Manifest package.json
//   // Passed to `--extensionDevelopmentPath`
//   let extensionDevelopmentPath = Node.Path.resolve(dirname, "../");
//   // The path to the extension test script
//   // Passed to --extensionTestsPath
//   let extensionTestsPath =
//     Node.Path.resolve(dirname, testSuiteAdapterFileName);

//   // Download VS Code, unzip it and run the integration test
//   runTests({extensionDevelopmentPath, extensionTestsPath});
// };

// switch (dirname) {
// | None =>
//   Js.log("Failed to read __dirname");
//   Node.Process.exit(1);
// | Some(dirname) =>
//   switch (run(dirname)) {
//   | promise =>
//     promise
//     |> Js.Promise.catch(error => {
//          Js.log2("Unhandled promise rejection: ", error);
//          Js.Promise.resolve(false);
//        })
//     |> Js.Promise.then_(
//          fun
//          | true => Node.Process.exit(0)
//          | false => Node.Process.exit(1),
//        )
//   | exception exn =>
//     Js.log2("Catched exception: ", exn);
//     Node.Process.exit(1);
//   }
// };
