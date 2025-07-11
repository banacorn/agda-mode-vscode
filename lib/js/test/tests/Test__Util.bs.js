// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Caml = require("rescript/lib/js/caml.js");
var Diff = require("diff");
var Assert = require("assert");
var Vscode = require("vscode");
var Nodefs = require("node:fs");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Nodepath = require("node:path");
var Caml_option = require("rescript/lib/js/caml_option.js");
var Core__Option = require("@rescript/core/lib/js/src/Core__Option.bs.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");
var OS$AgdaModeVscode = require("../../src/Util/OS.bs.js");
var Chan$AgdaModeVscode = require("../../src/Util/Chan.bs.js");
var Main$AgdaModeVscode = require("../../src/Main.bs.js");
var Util$AgdaModeVscode = require("../../src/Util/Util.bs.js");
var Config$AgdaModeVscode = require("../../src/Config.bs.js");
var Editor$AgdaModeVscode = require("../../src/Editor.bs.js");
var Parser$AgdaModeVscode = require("../../src/Parser/Parser.bs.js");
var Command$AgdaModeVscode = require("../../src/Command.bs.js");
var Node__Fs$AgdaModeVscode = require("../../src/Node/Node__Fs.bs.js");
var Registry$AgdaModeVscode = require("../../src/Registry.bs.js");
var Connection$AgdaModeVscode = require("../../src/Connection/Connection.bs.js");
var Connection__URI$AgdaModeVscode = require("../../src/Connection/Connection__URI.bs.js");
var Connection__Error$AgdaModeVscode = require("../../src/Connection/Connection__Error.bs.js");
var Connection__Target$AgdaModeVscode = require("../../src/Connection/Connection__Target.bs.js");
var Connection__Command$AgdaModeVscode = require("../../src/Connection/Connection__Command.bs.js");

var Exn = /* @__PURE__ */Caml_exceptions.create("Test__Util-AgdaModeVscode.Exn");

function open_(fileName) {
  return Vscode.window.showTextDocument(Vscode.Uri.file(fileName), undefined);
}

async function read(fileName) {
  var editor = await Vscode.window.showTextDocument(Vscode.Uri.file(fileName), undefined);
  var $$document = editor.document;
  return $$document.getText(undefined);
}

async function write(fileName, content) {
  var editor = await Vscode.window.showTextDocument(Vscode.Uri.file(fileName), undefined);
  var $$document = editor.document;
  var lineCount = $$document.lineCount;
  var replaceRange = new Vscode.Range(new Vscode.Position(0, 0), new Vscode.Position(lineCount, 0));
  var succeed = await Editor$AgdaModeVscode.$$Text.replace($$document, replaceRange, content);
  if (succeed) {
    await $$document.save();
    return ;
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Failed to write to " + fileName,
        Error: new Error()
      };
}

var $$File = {
  open_: open_,
  read: read,
  write: write
};

function getExpectedFilename(baseFilename) {
  if (OS$AgdaModeVscode.onUnix) {
    return baseFilename + ".out";
  } else {
    return baseFilename + ".win.out";
  }
}

var ExpectedFiles = {
  getExpectedFilename: getExpectedFilename
};

var runner = (function(f) {



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



  });

function toAbsolute(filepath) {
  return Nodepath.resolve(__dirname, filepath);
}

var extensionPath = Nodepath.resolve(__dirname, "../../../../");

var globalStorageUri = Vscode.Uri.file(Nodepath.resolve(__dirname, "../../../../test/globalStoragePath"));

function asset(filepath) {
  return Parser$AgdaModeVscode.Filepath.toString(Parser$AgdaModeVscode.Filepath.make(Nodepath.join(extensionPath, "test/tests/assets", filepath)));
}

var Path = {
  toAbsolute: toAbsolute,
  extensionPath: extensionPath,
  globalStorageUri: globalStorageUri,
  asset: asset
};

var activationSingleton = {
  contents: undefined
};

function activateExtension() {
  var channels = activationSingleton.contents;
  if (channels !== undefined) {
    return channels;
  }
  var disposables = [];
  var channels$1 = Main$AgdaModeVscode.activateWithoutContext(disposables, extensionPath, globalStorageUri, undefined);
  activationSingleton.contents = channels$1;
  return channels$1;
}

async function activateExtensionAndOpenFile(fileName) {
  var channels = activateExtension();
  var editor = await Vscode.window.showTextDocument(Vscode.Uri.file(fileName), undefined);
  return [
          editor,
          channels
        ];
}

function wait(ms) {
  return new Promise((function (resolve, param) {
                setTimeout(resolve, ms);
              }));
}

function normalize(string) {
  return string.trim().replace(/\r\n|\n/g, "\n");
}

function unlinesWith(f, xs) {
  return Util$AgdaModeVscode.$$String.unlines(xs.map(f));
}

function breakInput(input, breakpoints) {
  var breakpoints$p = [0].concat(breakpoints);
  return breakpoints$p.map(function (x, i) {
                var next = breakpoints$p[i + 1 | 0];
                if (next !== undefined) {
                  return [
                          x,
                          next
                        ];
                } else {
                  return [
                          x,
                          input.length
                        ];
                }
              }).map(function (param) {
              return input.substring(param[0], param[1]);
            });
}

var Strings = {
  normalize: normalize,
  unlinesWith: unlinesWith,
  breakInput: breakInput
};

function getValue(x) {
  return x._0;
}

function fromChangeObject(obj) {
  if (obj.added) {
    return {
            TAG: "Added",
            _0: obj.value
          };
  } else if (obj.removed) {
    return {
            TAG: "Removed",
            _0: obj.value
          };
  } else {
    return {
            TAG: "NoChange",
            _0: obj.value
          };
  }
}

function wordsWithSpace(a, b) {
  return Diff.diffWordsWithSpace(a, b).map(fromChangeObject);
}

function firstChange(diffs) {
  var count = {
    contents: 0
  };
  var change = {
    contents: undefined
  };
  diffs.forEach(function (diff) {
        if (!Core__Option.isNone(change.contents)) {
          return ;
        }
        switch (diff.TAG) {
          case "Added" :
              change.contents = {
                TAG: "Added",
                _0: diff._0
              };
              return ;
          case "Removed" :
              change.contents = {
                TAG: "Removed",
                _0: diff._0
              };
              return ;
          case "NoChange" :
              count.contents = count.contents + diff._0.length | 0;
              return ;
          
        }
      });
  return Core__Option.map(change.contents, (function (change) {
                return [
                        change,
                        count.contents
                      ];
              }));
}

var Diff$1 = {
  getValue: getValue,
  fromChangeObject: fromChangeObject,
  wordsWithSpace: wordsWithSpace,
  firstChange: firstChange
};

async function getGoldenFilepaths(directoryPath) {
  var directoryPath$1 = Nodepath.resolve(__dirname, directoryPath);
  var isInFile = function (x) {
    return x.endsWith(".in");
  };
  var toBasename = function (path) {
    return Nodepath.join(directoryPath$1, Nodepath.basename(path, ".in"));
  };
  var paths = await Node__Fs$AgdaModeVscode.readdir(directoryPath$1);
  return paths.filter(isInFile).map(toBasename);
}

function getGoldenFilepathsSync(directoryPath) {
  var directoryPath$1 = Nodepath.resolve(__dirname, directoryPath);
  var isInFile = function (x) {
    return x.endsWith(".in");
  };
  var toBasename = function (path) {
    return Nodepath.join(directoryPath$1, Nodepath.basename(path, ".in"));
  };
  return Nodefs.readdirSync(directoryPath$1).filter(isInFile).map(toBasename);
}

var FileMissing = /* @__PURE__ */Caml_exceptions.create("Test__Util-AgdaModeVscode.Golden.FileMissing");

function toString(param) {
  return "Golden file at: " + param._0 + "\nExpected: \n" + param._2 + "\nActual: \n" + param._1;
}

function map(param, f) {
  return {
          TAG: "Golden",
          _0: param._0,
          _1: f(param._1),
          _2: param._2
        };
}

async function readFile(filepath) {
  var filepath$1 = Nodepath.resolve(__dirname, filepath);
  var inFile = await Node__Fs$AgdaModeVscode.readFile(filepath$1 + ".in");
  var outFile = await Node__Fs$AgdaModeVscode.readFile(filepath$1 + ".out");
  return {
          TAG: "Golden",
          _0: filepath$1,
          _1: inFile,
          _2: outFile
        };
}

function compare(param) {
  var actual = normalize(param._1);
  var expected = normalize(param._2);
  Core__Option.forEach(firstChange(wordsWithSpace(actual, expected)), (function (param) {
          var count = param[1];
          var diff = param[0];
          var value = diff._0;
          var change = value.length > 100 ? value.substring(0, 100) + " ..." : value;
          var expected$1 = expected.substring(Caml.int_max(0, count - 50 | 0), ((Caml.int_max(0, count - 50 | 0) + 50 | 0) + value.length | 0) + 50 | 0);
          var actual$1 = actual.substring(Caml.int_max(0, count - 50 | 0), ((Caml.int_max(0, count - 50 | 0) + 50 | 0) + value.length | 0) + 50 | 0);
          var message = function (change) {
            return "\n\nchange => " + change + "\n\nexpected => " + expected$1 + "\n\nactual   => " + actual$1;
          };
          switch (diff.TAG) {
            case "Added" :
            case "Removed" :
                Assert.fail(message(change));
                return ;
            case "NoChange" :
                return ;
            
          }
        }));
}

var Golden = {
  Diff: Diff$1,
  getGoldenFilepaths: getGoldenFilepaths,
  getGoldenFilepathsSync: getGoldenFilepathsSync,
  FileMissing: FileMissing,
  toString: toString,
  map: map,
  readFile: readFile,
  compare: compare
};

async function versionGTE(command, expectedVersion) {
  var _error = await Connection$AgdaModeVscode.findCommands([command]);
  if (_error.TAG === "Ok") {
    return Util$AgdaModeVscode.Version.gte(_error._0._0, expectedVersion);
  } else {
    return false;
  }
}

async function commandExists(command) {
  var error = await Connection$AgdaModeVscode.findCommands([command]);
  if (error.TAG === "Ok") {
    return ;
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: error._0.map(Connection__Command$AgdaModeVscode.$$Error.toString).join("\n"),
        Error: new Error()
      };
}

async function makeAndLoad(filepath) {
  var filepath$1 = asset(filepath);
  await Config$AgdaModeVscode.Connection.setAgdaVersion("agda");
  await commandExists("agda");
  var load = async function (channels, filepath) {
    var match = Util$AgdaModeVscode.Promise_.pending();
    var resolve = match[1];
    var disposable = Chan$AgdaModeVscode.on(channels.commandHandled, (function (command) {
            if (command === "Load") {
              return resolve();
            }
            
          }));
    await Vscode.window.showTextDocument(Vscode.Uri.file(filepath), undefined);
    var match$1 = await Vscode.commands.executeCommand("agda-mode.load");
    if (match$1 !== undefined) {
      if (match$1.TAG === "Ok") {
        await match[0];
        disposable();
        return match$1._0;
      }
      var match$2 = Connection__Error$AgdaModeVscode.toString(match$1._0);
      throw {
            RE_EXN_ID: "Failure",
            _1: match$2[0] + "\n" + match$2[1],
            Error: new Error()
          };
    }
    throw {
          RE_EXN_ID: "Failure",
          _1: "Cannot load " + filepath,
          Error: new Error()
        };
  };
  var channels = activateExtension();
  var state = await load(channels, filepath$1);
  return {
          filepath: filepath$1,
          channels: channels,
          state: state
        };
}

async function quit(self) {
  return await Registry$AgdaModeVscode.removeAndDestroy(self.filepath);
}

async function $$case(self, cursor, payload) {
  var editor = await Vscode.window.showTextDocument(Vscode.Uri.file(self.filepath), undefined);
  var succeed = await Editor$AgdaModeVscode.$$Text.insert(self.state.document, cursor, payload);
  if (!succeed) {
    throw {
          RE_EXN_ID: "Failure",
          _1: "Failed to insert text",
          Error: new Error()
        };
  }
  Editor$AgdaModeVscode.Cursor.set(editor, cursor);
  var match = Util$AgdaModeVscode.Promise_.pending();
  var resolve = match[1];
  var destructor = Chan$AgdaModeVscode.on(self.state.channels.commandHandled, (function (command) {
          if (typeof command !== "object" && command === "Load") {
            return resolve();
          }
          
        }));
  var match$1 = await Vscode.commands.executeCommand("agda-mode.case");
  if (match$1 !== undefined) {
    if (match$1.TAG === "Ok") {
      await match[0];
      destructor();
      self.state = match$1._0;
      return ;
    }
    var match$2 = Connection__Error$AgdaModeVscode.toString(match$1._0);
    throw {
          RE_EXN_ID: "Failure",
          _1: match$2[0] + "\n" + match$2[1],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Cannot case split " + self.filepath,
        Error: new Error()
      };
}

async function execute(self, command) {
  var match = Util$AgdaModeVscode.Promise_.pending();
  var resolve = match[1];
  var destructor = Chan$AgdaModeVscode.on(self.state.channels.commandHandled, (function (handledCommand) {
          if (Caml_obj.equal(handledCommand, command)) {
            return resolve();
          }
          
        }));
  var match$1 = await Vscode.commands.executeCommand("agda-mode." + Command$AgdaModeVscode.toKeybinding(command));
  if (match$1 !== undefined) {
    if (match$1.TAG === "Ok") {
      await match[0];
      destructor();
      self.state = match$1._0;
      return ;
    }
    var match$2 = Connection__Error$AgdaModeVscode.toString(match$1._0);
    throw {
          RE_EXN_ID: "Failure",
          _1: match$2[0] + "\n" + match$2[1],
          Error: new Error()
        };
  }
  throw {
        RE_EXN_ID: "Failure",
        _1: "Cannot execute command " + Command$AgdaModeVscode.toString(command) + " in " + self.filepath,
        Error: new Error()
      };
}

async function execute$1(self, command, cursor, payload) {
  var editor = await Vscode.window.showTextDocument(Vscode.Uri.file(self.filepath), undefined);
  if (cursor !== undefined) {
    var cursor$1 = Caml_option.valFromOption(cursor);
    if (payload !== undefined) {
      await Editor$AgdaModeVscode.$$Text.insert(self.state.document, cursor$1, payload);
    }
    Editor$AgdaModeVscode.Cursor.set(editor, cursor$1);
  }
  return await execute(self, command);
}

function nextGoal(__x) {
  return execute$1(__x, "NextGoal", undefined, undefined);
}

function previousGoal(__x) {
  return execute$1(__x, "PreviousGoal", undefined, undefined);
}

var AgdaMode = {
  versionGTE: versionGTE,
  commandExists: commandExists,
  makeAndLoad: makeAndLoad,
  quit: quit,
  $$case: $$case,
  execute: execute$1,
  nextGoal: nextGoal,
  previousGoal: previousGoal
};

function filteredResponse(response) {
  if (typeof response !== "object") {
    switch (response) {
      case "ClearRunningInfo" :
      case "ClearHighlighting" :
      case "CompleteHighlightingAndMakePromptReappear" :
          return false;
      default:
        return true;
    }
  } else {
    switch (response.TAG) {
      case "HighlightingInfoDirect" :
      case "HighlightingInfoIndirect" :
      case "Status" :
      case "RunningInfo" :
          return false;
      default:
        return true;
    }
  }
}

async function mock(version, name) {
  var match = OS$AgdaModeVscode.onUnix ? [
      Nodepath.resolve(name),
      "#!/usr/bin/env node\nconsole.log('Agda version " + version + "')"
    ] : [
      Nodepath.resolve(name + ".bat"),
      "@echo off\nnode -e \"console.log('Agda version " + version + "')\""
    ];
  var path = match[0];
  Nodefs.writeFileSync(path, Buffer.from(match[1]));
  await Nodefs.promises.chmod(path, 493);
  return path;
}

function destroy(target) {
  Fs.unlink(Connection__URI$AgdaModeVscode.toString(Connection__Target$AgdaModeVscode.toURI(target)), (function (param) {
          
        }));
}

var Agda = {
  mock: mock,
  destroy: destroy
};

var Target = {
  Agda: Agda
};

exports.Exn = Exn;
exports.$$File = $$File;
exports.ExpectedFiles = ExpectedFiles;
exports.runner = runner;
exports.Path = Path;
exports.activationSingleton = activationSingleton;
exports.activateExtension = activateExtension;
exports.activateExtensionAndOpenFile = activateExtensionAndOpenFile;
exports.wait = wait;
exports.Strings = Strings;
exports.Golden = Golden;
exports.AgdaMode = AgdaMode;
exports.filteredResponse = filteredResponse;
exports.Target = Target;
/* extensionPath Not a pure module */
