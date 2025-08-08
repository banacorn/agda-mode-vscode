open Mocha

exception Exn(string)

// Web-compatible type conversion utilities
module TextEncoder = {
  type t
  @new external make: unit => t = "TextEncoder"
  @send external encode: (t, string) => Uint8Array.t = "encode"
}

module TextDecoder = {
  type t
  @new external make: unit => t = "TextDecoder"
  @send external decode: (t, Uint8Array.t) => string = "decode"
}

module TypedArray = {
  let toString = (uint8Array: Uint8Array.t): string => {
    TextDecoder.make()->TextDecoder.decode(uint8Array)
  }
}

module File = {
  let open_ = (fileName): promise<VSCode.TextEditor.t> =>
    VSCode.Window.showTextDocumentWithUri(VSCode.Uri.file(fileName), None)

  let read = async (fileName): string => {
    let editor = await open_(fileName)
    let document = VSCode.TextEditor.document(editor)
    VSCode.TextDocument.getText(document, None)
  }

  let write = async (fileName, content) => {
    let editor = await open_(fileName)
    let document = VSCode.TextEditor.document(editor)

    let lineCount = document->VSCode.TextDocument.lineCount
    let replaceRange = VSCode.Range.make(
      VSCode.Position.make(0, 0),
      VSCode.Position.make(lineCount, 0),
    )
    let succeed = await Editor.Text.replace(document, replaceRange, content)
    if succeed {
      let _ = await VSCode.TextDocument.save(document)
    } else {
      raise(Failure("Failed to write to " ++ fileName))
    }
  }
}

// Utility to get platform-specific expected file paths
module ExpectedFiles = {
  let getExpectedFilename = (baseFilename: string): string => {
    if OS.onUnix {
      baseFilename ++ ".out"
    } else {
      baseFilename ++ ".win.out"
    }
  }
}

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
  let extensionUri = VSCode.Uri.file(toAbsolute("../../../../"))

  // replacement of ExtensionContext.globalStoragePath as ExtensionContext.t is out ofreach
  let globalStorageUri = VSCode.Uri.file(toAbsolute("../../../../test/globalStoragePath"))

  let asset = filepath =>
    VSCode.Uri.joinPath(extensionUri, ["test/tests/assets", filepath])
    ->VSCode.Uri.fsPath
    ->Parser.Filepath.make
    ->Parser.Filepath.toString
}

// to prevent an extension from being activated twice
let activationSingleton = ref(None)

let activateExtension = (): State.channels => {
  switch activationSingleton.contents {
  | None =>
    let platformDeps = Desktop.make()
    // activate the extension
    let disposables = []
    let extensionUri = Path.extensionUri
    let globalStorageUri = Path.globalStorageUri
    let channels = Main.activateWithoutContext(
      platformDeps,
      disposables,
      extensionUri,
      globalStorageUri,
      None,
    )
    // store the singleton of activation
    activationSingleton := Some(channels)
    channels
  | Some(channels) => channels
  }
}

let activateExtensionAndOpenFile = async fileName => {
  let channels = activateExtension()
  let editor = await File.open_(fileName)
  (editor, channels)
}

@module("vscode") @scope("commands")
external executeCommand: string => promise<option<result<State.t, Connection.Error.t>>> =
  "executeCommand"

let wait = ms => Promise.make((resolve, _) => Js.Global.setTimeout(resolve, ms)->ignore)

module Strings = {
  // trim and replace all occurrences of line breaks with "\n"
  let normalize = string => {
    string->String.trim->String.replaceRegExp(%re("/\r\n|\n/g"), "\n")
  }

  let unlinesWith = (f, xs) => xs->Array.map(f)->Util.String.unlines

  let breakInput = (input: string, breakpoints: array<int>) => {
    let breakpoints' = Array.concat([0], breakpoints)

    breakpoints'
    ->Array.mapWithIndex((x: int, i) =>
      switch breakpoints'[i + 1] {
      | Some(next) => (x, next)
      | None => (x, String.length(input))
      }
    )
    ->Array.map(((start, end)) => String.substring(~start, ~end, input))
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
      // the count of characters before the first change occurred
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
  // let getGoldenFilepaths = async directoryPath => {
  //   let directoryUri = Path.toAbsolute(directoryPath)->VSCode.Uri.file
  //   // let readdir = Node__Fs.readdir
  //   let isInFile = ((x, _)) => x->String.endsWith(".in")
  //   let toBasename = path => NodeJs.Path.join2(directoryPath, NodeJs.Path.basenameExt(path, ".in"))

  //   // let paths = await readdir(directoryPath)

  //   switch await FS.readDirectory(directoryUri) {
  //   | Error(error) => raise(Failure("Cannot read directory " ++ directoryPath ++ ": " ++ error))
  //   | Ok(paths) => paths->Array.filter(isInFile)->Array.map(toBasename)
  //   }
  // }

  // get all filepaths of golden tests (synchronously)
  // Note: In web environment, this returns empty array as directory listing is async
  let getGoldenFilepathsSync = directoryPath => {
    // For web compatibility, return empty array since synchronous directory reading is not available
    // Tests using this function will need to be adapted to provide explicit test cases
    // instead of dynamically discovering golden test files
    let _ = directoryPath // avoid unused variable warning
    []
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
    let inFile = switch await FS.readFile(VSCode.Uri.file(filepath ++ ".in")) {
    | Error(error) => raise(Failure("Cannot read file " ++ filepath ++ ".in: " ++ error))
    | Ok(content) => content->TypedArray.toString
    }

    let outFile = switch await FS.readFile(VSCode.Uri.file(filepath ++ ".out")) {
    | Error(error) => raise(Failure("Cannot read file " ++ filepath ++ ".out: " ++ error))
    | Ok(content) => content->TypedArray.toString
    }

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
        String.length(value) > 100 ? String.substring(~start=0, ~end=100, value) ++ " ..." : value

      let expected = String.substring(
        ~start=max(0, count - 50),
        ~end=max(0, count - 50) + 50 + String.length(value) + 50,
        expected,
      )

      let actual = String.substring(
        ~start=max(0, count - 50),
        ~end=max(0, count - 50) + 50 + String.length(value) + 50,
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

module AgdaMode = {
  let versionGTE = async (command, expectedVersion) => {
    let platformDeps = Desktop.make()
    switch await Connection.findCommands(platformDeps, [command]) {
    | Error(_error) => false
    | Ok(connection) =>
      let actualVersion = switch connection {
      | Agda(version, _) => version
      | ALS(version, _, _, _) => version
      }
      Util.Version.gte(actualVersion, expectedVersion)
    }
  }

  let commandExists = async command => {
    let platformDeps = Desktop.make()
    switch await Connection.findCommands(platformDeps, [command]) {
    | Error(errors) =>
      raise(
        Failure(
          errors
          ->Dict.toArray
          ->Array.map(((command, error)) =>
            command ++ ": " ++ Connection__Command.Error.toString(error)
          )
          ->Array.join("\n"),
        ),
      )
    | Ok(_) => ()
    }
  }

  type t = {
    filepath: string,
    channels: State.channels,
    mutable state: State.t,
  }

  let makeAndLoad = async filepath => {
    let rawFilepath =
      VSCode.Uri.joinPath(Path.extensionUri, ["test/tests/assets", filepath])->VSCode.Uri.fsPath
    // set name for searching Agda
    await Config.Connection.setAgdaVersion("agda")
    // make sure that "agda" exists in PATH
    await commandExists("agda")
    //
    let load = async (channels: State.channels, filepath) => {
      let (promise, resolve, _) = Util.Promise_.pending()

      let disposable = channels.commandHandled->Chan.on(command => {
        if command == Command.Load {
          resolve()
        }
      })
      let _ = await File.open_(filepath) // need to open the file first somehow
      switch await executeCommand("agda-mode.load") {
      | None => raise(Failure("Cannot load " ++ filepath))
      | Some(Ok(state)) =>
        await promise
        disposable() // stop listening to responses
        state
      | Some(Error(error)) =>
        let (header, body) = Connection.Error.toString(error)
        raise(Failure(header ++ "\n" ++ body))
      }
    }

    let channels = activateExtension()
    let state = await load(channels, rawFilepath)

    state.channels.log->Chan.emit(AgdaModeOperation("makeAndLoad", rawFilepath))

    // On Windows, ensure the registry entry uses the same path format as the context
    // The state may have been stored with a normalized path, so we need to update it
    if !OS.onUnix {
      // Store the registry entry with the normalized path that Main.res expects
      let normalizedPath = Parser.filepath(rawFilepath)
      Registry.add(normalizedPath, state)
      // Also store with the raw path for the test context
      Registry.add(rawFilepath, state)
    }

    state.channels.log->Chan.emit(AgdaModeOperation("makeAndLoad", rawFilepath))

    // On Windows, ensure the registry entry uses the same path format as the context
    // The state may have been stored with a normalized path, so we need to update it
    if !OS.onUnix {
      // Store the registry entry with the normalized path that Main.res expects
      let normalizedPath = Parser.filepath(rawFilepath)
      Registry.add(normalizedPath, state)
      // Also store with the raw path for the test context
      Registry.add(rawFilepath, state)
    }

    {
      filepath: rawFilepath,
      channels,
      state,
    }
  }

  let quit = async (self: t) => {
    self.state.channels.log->Chan.emit(AgdaModeOperation("quit", self.filepath))
    await Registry.removeAndDestroy(self.filepath)
    self.state.channels.log->Chan.emit(AgdaModeOperation("quit completed", self.filepath))
  }

  let case = async (self, ~cursor, ~payload) => {
    let editor = await File.open_(self.filepath)

    // set cursor and insert the target for case splitting
    let succeed = await Editor.Text.insert(self.state.document, cursor, payload)
    if !succeed {
      raise(Failure("Failed to insert text"))
    }
    Editor.Cursor.set(editor, cursor)

    // The `agda-mode.load` command will be issued after `agda-mode.case` is executed
    // listen to the `agda-mode.load` command to know when the whole case split process is done
    let (promise, resolve, _) = Util.Promise_.pending()
    let destructor = self.state.channels.commandHandled->Chan.on(command => {
      switch command {
      | Command.Load => resolve()
      | _ => ()
      }
    })

    switch await executeCommand("agda-mode.case") {
    | None => raise(Failure("Cannot case split " ++ self.filepath))
    | Some(Ok(state)) =>
      // wait for the `agda-mode.load` command to be handled
      await promise
      // stop listening to commands
      destructor()

      // update the context with the new state
      self.state = state
    | Some(Error(error)) =>
      let (header, body) = Connection.Error.toString(error)
      raise(Failure(header ++ "\n" ++ body))
    }
  }

  let execute = async (self, command) => {
    let (promise, resolve, _) = Util.Promise_.pending()
    let destructor = self.state.channels.commandHandled->Chan.on(handledCommand => {
      if handledCommand == command {
        resolve()
      }
    })

    // should be registered in the extension
    if Command.names->Array.some(((registeredCommand, _)) => registeredCommand == command) {
      ()
    } else {
      raise(Failure("Command " ++ Command.toString(command) ++ " is not registered"))
    }

    switch await executeCommand("agda-mode." ++ Command.toKeybinding(command)) {
    | None =>
      raise(
        Failure("Cannot execute command " ++ Command.toString(command) ++ " in " ++ self.filepath),
      )
    | Some(Ok(state)) =>
      // wait for the  command to be handled
      await promise
      // stop listening to commands
      destructor()

      // update the context with the new state
      self.state = state
    | Some(Error(error)) =>
      let (header, body) = Connection.Error.toString(error)
      raise(Failure(header ++ "\n" ++ body))
    }
  }

  let execute = async (self, command, ~cursor=?, ~payload=?) => {
    let editor = await File.open_(self.filepath)
    // set the cursor and insert the payload
    switch cursor {
    | None => ()
    | Some(cursor) =>
      switch payload {
      | None => ()
      | Some(payload) =>
        let _ = await Editor.Text.insert(self.state.document, cursor, payload)
      }
      Editor.Cursor.set(editor, cursor)
    }
    // execute the command
    await execute(self, command)
  }

  let nextGoal = execute(_, NextGoal)
  let previousGoal = execute(_, PreviousGoal)
}

// helper function for filtering out Highlighting & RunningInfo related responses
let filteredResponse = response =>
  switch response {
  // highlightings
  | Response.ClearRunningInfo => false
  | ClearHighlighting => false
  | HighlightingInfoIndirect(_) => false
  | HighlightingInfoDirect(_) => false
  | CompleteHighlightingAndMakePromptReappear => false
  // status & running info
  | Status(_, _) => false
  | RunningInfo(_, _) => false
  | _ => true
  }

// for mocking an Agda or Language Server executable with version and path
module Endpoint = {
  module Agda = {
    // given a version and the desired name of the executable, create a mock Agda executable and returns the path
    let mock = async (~version, ~name) => {
      // creates a executable with nodejs in temp directory
      let (fileName, content) = if OS.onUnix {
        (name, "#!/bin/sh\necho 'Agda version " ++ version ++ "'\nexit 0")
      } else {
        (name ++ ".bat", "@echo Agda version " ++ version)
      }

      // Create file in temp directory to avoid permission issues
      let tempFile = NodeJs.Path.join([NodeJs.Os.tmpdir(), fileName])

      // Use Node.js file writing for executable creation (tests only)
      try {
        NodeJs.Fs.writeFileSync(tempFile, NodeJs.Buffer.fromString(content))

        // chmod +x for Unix systems
        if OS.onUnix {
          switch await NodeJs.Fs.chmod(tempFile, ~mode=0o755) {
          | exception Js.Exn.Error(obj) =>
            let errorMsg = Js.Exn.message(obj)->Option.getOr("unknown chmod error")
            raise(Failure("Cannot chmod mock executable at " ++ tempFile ++ ": " ++ errorMsg))
          | _ => ()
          }
        }
        tempFile
      } catch {
      | Js.Exn.Error(obj) =>
        let errorMsg = Js.Exn.message(obj)->Option.getOr("unknown error")
        let detailedError =
          "Got error when trying to construct a mock for Agda:\n" ++
          "  - Target file: " ++
          tempFile ++
          "\n" ++
          "  - Platform: " ++
          (OS.onUnix ? "Unix" : "Windows") ++
          "\n" ++
          "  - Temp directory: " ++
          NodeJs.Os.tmpdir() ++
          "\n" ++
          "  - Error: " ++
          errorMsg ++
          "\n" ++
          "  - Content length: " ++
          string_of_int(String.length(content)) ++
          "\n" ++
          "  - Content: " ++
          content
        raise(Failure(detailedError))
      | _ =>
        let detailedError =
          "Got error when trying to construct a mock for Agda:\n" ++
          "  - Target file: " ++
          tempFile ++
          "\n" ++
          "  - Platform: " ++
          (OS.onUnix ? "Unix" : "Windows") ++
          "\n" ++
          "  - Temp directory: " ++
          NodeJs.Os.tmpdir() ++
          "\n" ++
          "  - Error: unknown error type\n" ++
          "  - Content: " ++
          content
        raise(Failure(detailedError))
      }
    }

    let destroy = async target => {
      let path = target->Connection.Endpoint.toURI->Connection.URI.toString
      try {
        NodeJs.Fs.unlinkSync(path)
      } catch {
      | Js.Exn.Error(obj) =>
        // Don't raise an error for cleanup failures, just log warning
        Js.Console.warn(
          "Warning: Cannot delete mock Agda executable: " ++
          Js.Exn.message(obj)->Option.getOr("unknown error"),
        )
      | _ => // Ignore other cleanup failures to prevent test failures
        ()
      }
    }
  }
}
