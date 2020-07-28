open Belt;

// bindings for Node.js __dirname
let dirname: option(string) = [%bs.node __dirname];

// bindings for using mocha programmatically
module Mocha = {
  type t;
  type options = {
    ui: string,
    color: bool,
  };

  [@bs.module] [@bs.new] external make: options => t = "mocha";
  [@bs.send] external addFile: (t, string) => unit = "addFile";
  [@bs.send] external run: (t, int => unit) => unit = "run";
};

// bindings for glob
module Glob = {
  type options = {cwd: string};

  [@bs.module]
  external glob:
    (string, options, (Js.nullable('error), array(string)) => unit) => unit =
    "glob";
};

exception TestFailure(string);

let run = () => {
  // Create the mocha test
  let mocha = Mocha.make({ui: "bdd", color: true});

  // dirname: ./lib/js/test
  // testsRoot: ./lib/js/test/tests

  let testsRoot =
    dirname->Option.mapWithDefault("./", dirname =>
      Node.Path.resolve(dirname, "tests")
    );

  Js.Promise.make((~resolve, ~reject) => {
    Glob.glob("**/*.js", {cwd: testsRoot}, (err, files) => {
      switch (Js.Nullable.toOption(err)) {
      | Some(err) => reject(. err)
      | None =>
        // Add files to the test suite
        files->Array.forEach(file =>
          mocha->Mocha.addFile(Node.Path.resolve(testsRoot, file))
        );

        // Run the mocha test
        switch (
          mocha->Mocha.run(failures =>
            if (failures > 0) {
              reject(.
                TestFailure(string_of_int(failures) ++ " tests failed."),
              );
            } else {
              resolve(. true);
            }
          )
        ) {
        | () => ()
        | exception exn =>
          Js.Console.error(exn);
          reject(. exn);
        };
      }
    })
  });
};
