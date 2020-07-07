open Belt;

open Js.Promise;

exception Exn(string);

// module Assert = {
//   let equal = (~message=?, expected, actual) =>
//     BsMocha.Assert.equal(~message?, actual, expected);
//   let yes = equal(true);
//   let no = equal(false);
//   let fail = BsMocha.Assert.fail;
//   let ok = _ => BsMocha.Assert.ok(true);
//   // let not_equal = (~message=?, expected, actual) =>
//   //   Assert.not_equal(~message?, actual, expected);
// };

module Golden = {
  // bindings for jsdiff
  module Diff = {
    type t =
      | Added(string)
      | Removed(string)
      | NoChange(string);

    let getValue =
      fun
      | Added(string) => string
      | Removed(string) => string
      | NoChange(string) => string;

    type changeObject = {
      .
      "value": string,
      "added": bool,
      "removed": bool,
    };

    // [@bs.module "diff"]
    // external lines: (string, string) => array(t) = "diffLines";

    [@bs.module "diff"]
    external wordsWithSpace_: (string, string) => array(changeObject) =
      "diffWordsWithSpace";

    let fromChangeObject = obj =>
      if (obj##added) {
        Added(obj##value);
      } else if (obj##removed) {
        Removed(obj##value);
      } else {
        NoChange(obj##value);
      };

    let wordsWithSpace = (a, b) => {
      wordsWithSpace_(a, b)->Array.map(fromChangeObject);
    };

    // given a list of Diff.t, return the first Added or Removed and the character count before it
    let firstChange = diffs => {
      // the count of charactors before the first change occured
      let count = ref(0);
      let change = ref(None);
      diffs->Array.forEach(diff =>
        if (Option.isNone(change^)) {
          switch (diff) {
          | Added(s) => change := Some(Added(s))
          | Removed(s) => change := Some(Removed(s))
          | NoChange(s) => count := count^ + String.length(s)
          };
        }
      );

      (change^)->Option.map(change => (change, count^));
    };
  };
  // get all filepaths of golden tests (asynchronously)
  let getGoldenFilepaths = dirname => {
    let readdir = N.Fs.readdir |> N.Util.promisify;
    let isInFile = Js.String.endsWith(".in");
    let toBasename = path =>
      Node.Path.join2(dirname, Node.Path.basename_ext(path, ".in"));
    readdir(. dirname)
    |> then_(paths =>
         paths->Array.keep(isInFile)->Array.map(toBasename)->resolve
       );
  };

  // get all filepaths of golden tests (synchronously)
  let getGoldenFilepathsSync = dirname => {
    let readdir = Node.Fs.readdirSync;
    let isInFile = Js.String.endsWith(".in");
    let toBasename = path =>
      Node.Path.join2(dirname, Node.Path.basename_ext(path, ".in"));
    readdir(dirname)->Array.keep(isInFile)->Array.map(toBasename);
  };

  exception FileMissing(string);

  type filepath = string;
  type actual = string;
  type t('expected) =
    | Golden(filepath, 'expected, actual);

  // (A -> B) -> Golden A -> Golden B
  let map = (Golden(filepath, expected, actual), f) => {
    Golden(filepath, f(expected), actual);
  };

  // FilePath -> Promise (Golden String)
  let readFile = filepath => {
    let readFile = N.Fs.readFile |> N.Util.promisify;

    [|readFile(. filepath ++ ".in"), readFile(. filepath ++ ".out")|]
    |> all
    |> then_(
         fun
         | [|input, output|] =>
           resolve(
             Golden(
               filepath,
               Node.Buffer.toString(input),
               Node.Buffer.toString(output),
             ),
           )
         | _ => reject(FileMissing(filepath)),
       );
  };

  // Golden String -> Promise ()
  let compare = (Golden(_path, actual, expected)) => {
    let actual = Js.String.trim(actual);
    let expected = Js.String.trim(expected);
    Diff.wordsWithSpace(actual, expected)
    ->Diff.firstChange
    ->Option.forEach(((diff, count)) => {
        open Diff;
        let value = Diff.getValue(diff);

        let change =
          Js.String.length(value) > 100
            ? Js.String.substrAtMost(~from=0, ~length=100, value) ++ " ..."
            : value;

        let expected' =
          Js.String.substrAtMost(
            ~from=max(0, count - 50),
            ~length=50 + String.length(value) + 50,
            expected,
          );

        let actual' =
          Js.String.substrAtMost(
            ~from=max(0, count - 50),
            ~length=50 + String.length(value) + 50,
            actual,
          );

        let message =
          "\n\nexpected => "
          ++ expected'
          ++ "\n\nactual   => "
          ++ actual'
          ++ "\n\nchange => ";

        switch (diff) {
        | Added(_) =>
          BsMocha.Assert.fail(
            message
            ++ " added \""
            ++ change
            ++ "\"\n at position "
            ++ string_of_int(count),
          )
        | Removed(_) =>
          BsMocha.Assert.fail(
            message
            ++ " removed \""
            ++ change
            ++ "\"\n\n at position "
            ++ string_of_int(count),
          )
        | NoChange(_) => ()
        };
      });
    Js.Promise.resolve();
  };
};

let serialize = xs => xs->Array.map(x => x ++ "\n")->Js.String.concatMany("");

let serializeWith = (f, xs) => xs->Array.map(f)->serialize;
