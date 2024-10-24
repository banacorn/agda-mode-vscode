open Mocha

open Js.Promise

exception Exn(string)

// wrapper around BsMocha's Assertions
let runner: (unit => unit) => promise<result<'a, exn>> = %raw(` function(f) {
    var tmp
    try {
      var result = f();
      tmp = {
        TAG: 0,
        _0: result,
        [Symbol.for("name")]: "Ok"
      };
    }
    catch (raw_exn){
      tmp = 
        {
          TAG: 1,
          _0: raw_exn,
          [Symbol.for("name")]: "Error"
        };
    }
    return $$Promise.resolved(tmp);
  }`)

// Paths of the extension and assets
module Path = {
  let toAbsolute = filepath => NodeJs.Path.resolve([NodeJs.Global.dirname, filepath])

  // replacement of ExtensionContext.getExtensionPath as ExtensionContext.t is out of reach
  let extensionPath = toAbsolute("../../../../")

  // replacement of ExtensionContext.globalStoragePath as ExtensionContext.t is out ofreach
  let globalStoragePath = toAbsolute("../../../../test/globalStoragePath")

  let asset = filepath => NodeJs.Path.join([extensionPath, "test/tests/assets", filepath])
}

// to prevent an extension from being activated twice
let activationSingleton = ref(None)

let activateExtension = (): State__Type.channels => {
  switch activationSingleton.contents {
  | None =>
    // activate the extension
    let disposables = []
    let extensionPath = Path.extensionPath
    let globalStoragePath = Path.globalStoragePath
    let channels = Main.activateWithoutContext(disposables, extensionPath, globalStoragePath)
    // store the singleton of activation
    activationSingleton := Some(channels)
    channels
  | Some(channels) => channels
  }
}

let openFile = (fileName): Promise.t<VSCode.TextEditor.t> =>
  VSCode.Window.showTextDocumentWithUri(VSCode.Uri.file(fileName), None)

let activateExtensionAndOpenFile = async fileName => {
  let channels = activateExtension()
  let editor = await openFile(fileName)
  (editor, channels)
}

@module("vscode") @scope("commands")
external executeCommand: string => Promise.t<option<result<State.t, Connection.Error.t>>> =
  "executeCommand"

let wait = ms => Promise.make((resolve, _) => Js.Global.setTimeout(resolve, ms)->ignore)

module Strings = {
  // trim and replace all occurences of line breaks with "\n"
  let normalize = string => {
    open Js.String
    replaceByRe(%re("/\r\n|\n/g"), "\n", trim(string))
  }

  let unlinesWith = (f, xs) => xs->Array.map(f)->Util.String.unlines

  let breakInput = (input: string, breakpoints: array<int>) => {
    let breakpoints' = Array.concat([0], breakpoints)

    breakpoints'
    ->Array.mapWithIndex((x: int, i) =>
      switch breakpoints'[i + 1] {
      | Some(next) => (x, next - x)
      | None => (x, Js.String.length(input) - x)
      }
    )
    ->Array.map(((from, length)) => Js.String.substrAtMost(~from, ~length, input))
  }
}

module Golden = {
  // bindings for jsdiff
  module Diff = {
    type t =
      | Added(string)
      | Removed(string)
      | NoChange(string)

    let getValue = x =>
      switch x {
      | Added(string) => string
      | Removed(string) => string
      | NoChange(string) => string
      }

    type changeObject = {"value": string, "added": bool, "removed": bool}

    // [@bs.module "diff"]
    // external lines: (string, string) => array(t) = "diffLines";

    @module("diff")
    external wordsWithSpace_: (string, string) => array<changeObject> = "diffWordsWithSpace"

    let fromChangeObject = obj =>
      if obj["added"] {
        Added(obj["value"])
      } else if obj["removed"] {
        Removed(obj["value"])
      } else {
        NoChange(obj["value"])
      }

    let wordsWithSpace = (a, b) => wordsWithSpace_(a, b)->Array.map(fromChangeObject)

    // given a list of Diff.t, return the first Added or Removed and the character count before it
    let firstChange = diffs => {
      // the count of charactors before the first change occured
      let count = ref(0)
      let change = ref(None)
      diffs->Array.forEach(diff =>
        if Option.isNone(change.contents) {
          switch diff {
          | Added(s) => change := Some(Added(s))
          | Removed(s) => change := Some(Removed(s))
          | NoChange(s) => count := count.contents + String.length(s)
          }
        }
      )

      change.contents->Option.map(change => (change, count.contents))
    }
  }
  // get all filepaths of golden tests (asynchronously)
  let getGoldenFilepaths = directoryPath => {
    let directoryPath = Path.toAbsolute(directoryPath)
    let readdir = N.Util.promisify(N.Fs.readdir, ...)
    let isInFile = x => Js.String.endsWith(".in", x)
    let toBasename = path => NodeJs.Path.join2(directoryPath, NodeJs.Path.basenameExt(path, ".in"))
    then_(
      paths => paths->Array.filter(isInFile)->Array.map(toBasename)->resolve,
      readdir(directoryPath),
    )
  }

  // get all filepaths of golden tests (synchronously)
  let getGoldenFilepathsSync = directoryPath => {
    let directoryPath = Path.toAbsolute(directoryPath)
    let readdir = NodeJs.Fs.readdirSync
    let isInFile = x => Js.String.endsWith(".in", x)
    let toBasename = path => NodeJs.Path.join2(directoryPath, NodeJs.Path.basenameExt(path, ".in"))
    readdir(directoryPath)->Array.filter(isInFile)->Array.map(toBasename)
  }

  exception FileMissing(string)

  type filepath = string
  type expected = string
  // parameterized only by 'actual;
  type t<'actual> = Golden(filepath, 'actual, expected)

  let toString = (Golden(filepath, actual, expected)) =>
    "Golden file at: " ++
    filepath ++
    "\n" ++
    "Expected: \n" ++
    expected ++
    "\n" ++
    "Actual: \n" ++
    actual

  // (A -> B) -> Golden A -> Golden B
  let map = (Golden(filepath, actual, expected), f) => Golden(filepath, f(actual), expected)

  // FilePath -> Promise (Golden String)
  let readFile = async filepath => {
    let filepath = Path.toAbsolute(filepath)
    let inFile = await Node__Fs.readFile(filepath ++ ".in")
    let outFile = await Node__Fs.readFile(filepath ++ ".out")

    Golden(filepath, inFile, outFile)
  }

  // Golden String -> Promise ()
  let compare = (Golden(_path, actual, expected)) => {
    let actual = Strings.normalize(actual)
    let expected = Strings.normalize(expected)

    Diff.wordsWithSpace(actual, expected)
    ->Diff.firstChange
    ->Option.forEach(((diff, count)) => {
      open Diff
      let value = Diff.getValue(diff)

      let change =
        Js.String.length(value) > 100
          ? Js.String.substrAtMost(~from=0, ~length=100, value) ++ " ..."
          : value

      let expected = Js.String.substrAtMost(
        ~from=max(0, count - 50),
        ~length=50 + String.length(value) + 50,
        expected,
      )

      let actual = Js.String.substrAtMost(
        ~from=max(0, count - 50),
        ~length=50 + String.length(value) + 50,
        actual,
      )

      let message = change =>
        "\n\nchange => " ++ change ++ "\n\nexpected => " ++ expected ++ "\n\nactual   => " ++ actual

      switch diff {
      | Added(_) => Assert.fail(message(change))
      | Removed(_) => Assert.fail(message(change))
      | NoChange(_) => ()
      }
    })
  }
}

let onUnix = switch N.OS.type_() {
| "Windows_NT" => false
| _ => true
}

module AgdaMode = {
  module Error = {
    type t =
      | LanguageServerMuleErrors(array<LanguageServerMule.Source.Error.t>)
      | EmacsConnectionError(Connection.Emacs.Error.t)
    let toString = x =>
      switch x {
      | LanguageServerMuleErrors(errors) =>
        Js.Array.joinWith(",", errors->Array.map(LanguageServerMule.Source.Error.toString))
      | EmacsConnectionError(error) =>
        let (header, body) = Connection.Emacs.Error.toString(error)
        "EmacsConnectionError: " ++ header ++ ": " ++ body
      }
  }

  let exists = async command => {
    let (result, errors) = await LanguageServerMule.Source.Module.searchUntilSuccess([
      FromCommand(command),
    ])
    switch result {
    | None =>
      let msg = Js.Array.joinWith(",", errors->Array.map(LanguageServerMule.Source.Error.toString))
      raise(Failure("Cannot find \"Agda\" in PATH: " ++ msg))
    | Some(_method) => ()
    }
  }

  type t = {
    filepath: string,
    channels: State__Type.channels,
  }

  let make = async (~als=false, filepath) => {
    let filepath = Path.asset(filepath)
    // for mocking Configs
    Config.inTestingMode := true
    // set name for searching Agda
    await Config.Connection.setAgdaVersion("agda")
    await Config.Connection.setUseAgdaLanguageServer(als)
    let _ = await exists("agda")
    {
      filepath,
      channels: activateExtension(),
    }
  }

  let load = async self => {
    let (promise, resolve, _) = Util.Promise_.pending()

    // agda-mode:load is consider finished
    // when `CompleteHighlightingAndMakePromptReappear` has been handled
    let disposable = self.channels.responseHandled->Chan.on(response => {
      switch response {
      | CompleteHighlightingAndMakePromptReappear => resolve()
      | _ => ()
      }
    })

    let _ = await openFile(self.filepath)
    switch await executeCommand("agda-mode.load") {
    | None => raise(Failure("Cannot load " ++ self.filepath))
    | Some(Ok(state)) =>
      await promise
      disposable() // stop listening to responses
      state
    | Some(Error(error)) =>
      let (header, body) = Connection.Error.toString(error)
      raise(Failure(header ++ "\n" ++ body))
    }
  }

  let case = async (self, cursorAndPayload, state: State__Type.t) => {
    let editor = await openFile(self.filepath)

    switch cursorAndPayload {
    | None => ()
    | Some(cursor, payload) =>
      let succeed = await Editor.Text.insert(state.document, cursor, payload)
      if !succeed {
        raise(Failure("Failed to insert text"))
      }
      Editor.Cursor.set(editor, cursor)
    }
    switch await executeCommand("agda-mode.case") {
    | None => raise(Failure("Cannot case split " ++ self.filepath))
    | Some(Ok(state)) => state
    | Some(Error(error)) =>
      let (header, body) = Connection.Error.toString(error)
      raise(Failure(header ++ "\n" ++ body))
    }
  }

  let refine = async (self, cursorAndPayload, state: State__Type.t): AgdaModeVscode.State.t => {
    let editor = await openFile(self.filepath)
    switch cursorAndPayload {
    | None => ()
    | Some(cursor, None) => Editor.Cursor.set(editor, cursor)
    | Some(cursor, Some(payload)) =>
      let _ = await Editor.Text.insert(state.document, cursor, payload)
      Editor.Cursor.set(editor, cursor)
    }
    switch await executeCommand("agda-mode.refine") {
    | None => raise(Failure("Cannot case refine " ++ self.filepath))
    | Some(Ok(state)) => state
    | Some(Error(error)) =>
      let (header, body) = Connection.Error.toString(error)
      raise(Failure(header ++ "\n" ++ body))
    }
  }
}

// store file content before testing so that we can restore it later
let readFile = async (filepath, var) => {
  let editor = await openFile(filepath)
  var := Editor.Text.getAll(VSCode.TextEditor.document(editor))
}

let restoreFile = async (filepath, var) => {
  let editor = await openFile(filepath)
  let document = VSCode.TextEditor.document(editor)
  let lineCount = document->VSCode.TextDocument.lineCount
  let replaceRange = VSCode.Range.make(
    VSCode.Position.make(0, 0),
    VSCode.Position.make(lineCount, 0),
  )
  let succeed = await Editor.Text.replace(document, replaceRange, var.contents)
  if succeed {
    let _ = await VSCode.TextDocument.save(document)
  } else {
    raise(Failure("Failed to restore the file"))
  }
}

module R = {
  let unwrap = (x: result<'a, 'e>): 'a =>
    switch x {
    | Ok(x) => x
    | Error(error) => raise(error)
    }
}

// for handling promise<result<'a, 'error>> in tests
module P = {
  let unwrap = async (promise: promise<result<'a, 'e>>): 'a => {
    let result = await promise
    R.unwrap(result)
  }
}
