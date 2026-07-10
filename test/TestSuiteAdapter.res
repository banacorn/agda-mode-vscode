// bindings for using mocha programmatically
module Mocha = {
  type t
  type options = {
    ui: string,
    color: bool,
    timeout: int,
  }

  @module @new external make: options => t = "mocha"
  @send external addFile: (t, string) => unit = "addFile"
  @send external run: (t, int => unit) => unit = "run"
}

// bindings for glob
module Glob = {
  type options = {cwd: string}

  @module
  external glob: (string, options, (Nullable.t<'error>, array<string>) => unit) => unit = "glob"
}

exception TestFailure(string)

// Permanent, general test-runner feature: restrict which test files are
// loaded for a given `npm test` invocation. Selecting a single test file
// also gives that test its own fresh extension-host process, since
// `npm test` spawns one VS Code instance per invocation.
//   AGDA_TEST_GLOB -- glob restricting which test files are loaded, relative
//                     to `test/tests` (default "**/*.js").
let defaultTimeout = 4000

let readTestGlob = () =>
  switch NodeJs.Process.process->NodeJs.Process.env->Dict.get("AGDA_TEST_GLOB") {
  | Some(glob) => glob
  | None => "**/*.js"
  }

let run = () => {
  let testGlob = readTestGlob()

  // Create the mocha test
  let mocha = Mocha.make({ui: "bdd", color: true, timeout: defaultTimeout})

  // dirname: ./lib/js/test
  // testsRoot: ./lib/js/test/tests

  let testsRoot = NodeJs.Path.resolve([NodeJs.Global.dirname, "tests"])

  Promise.make((resolve, reject) =>
    Glob.glob(testGlob, {cwd: testsRoot}, (err, files) =>
      switch Nullable.toOption(err) {
      | Some(err) => reject(err)
      | None =>
        // Add files to the test suite
        files->Array.forEach(file => mocha->Mocha.addFile(NodeJs.Path.resolve([testsRoot, file])))

        // Run the mocha test
        switch mocha->Mocha.run(
          failures =>
            if failures > 0 {
              reject(TestFailure(string_of_int(failures) ++ " tests failed."))
            } else {
              resolve(true)
            },
        ) {
        | () => ()
        | exception exn =>
          Js.Console.error(exn)
          reject(exn)
        }
      }
    )
  )
}
