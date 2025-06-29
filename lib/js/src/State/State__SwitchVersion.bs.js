// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Vscode = require("vscode");
var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Nodepath = require("node:path");
var Core__Array = require("@rescript/core/lib/js/src/Core__Array.bs.js");
var Core__Option = require("@rescript/core/lib/js/src/Core__Option.bs.js");
var Item$AgdaModeVscode = require("../View/Component/Item.bs.js");
var Util$AgdaModeVscode = require("../Util/Util.bs.js");
var Config$AgdaModeVscode = require("../Config.bs.js");
var Connection$AgdaModeVscode = require("../Connection/Connection.bs.js");
var State__View$AgdaModeVscode = require("./State__View.bs.js");
var Connection__URI$AgdaModeVscode = require("../Connection/Connection__URI.bs.js");
var Connection__Error$AgdaModeVscode = require("../Connection/Connection__Error.bs.js");
var State__Connection$AgdaModeVscode = require("./State__Connection.bs.js");
var Connection__Target$AgdaModeVscode = require("../Connection/Connection__Target.bs.js");
var Connection__Download$AgdaModeVscode = require("../Connection/Connection__Download.bs.js");
var Connection__Download__Platform$AgdaModeVscode = require("../Connection/Download/Connection__Download__Platform.bs.js");

async function openGlobalStorageFolder(state) {
  await Vscode.env.openExternal(state.globalStorageUri);
}

async function switchAgdaVersion(state) {
  var match = await Connection__Target$AgdaModeVscode.getPicked(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
  if (match.TAG === "Ok") {
    var match$1 = match._0;
    if (match$1.TAG === "Agda") {
      await State__View$AgdaModeVscode.Panel.displayStatus(state, "");
      await State__View$AgdaModeVscode.Panel.display(state, {
            TAG: "Plain",
            _0: "Switching to Agda v" + match$1._0
          }, []);
    } else {
      await State__View$AgdaModeVscode.Panel.displayStatus(state, "");
      await State__View$AgdaModeVscode.Panel.display(state, {
            TAG: "Plain",
            _0: "Switching to Agda v" + match$1._1 + " Language Server v" + match$1._0
          }, []);
    }
  }
  await Connection$AgdaModeVscode.destroy(state.connection);
  var platform = await Connection__Download__Platform$AgdaModeVscode.determine();
  var conn = await Connection$AgdaModeVscode.make(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths(), [
        "als",
        "agda"
      ], platform, State__Connection$AgdaModeVscode.askUserAboutDownloadPolicy, Connection$AgdaModeVscode.LatestALS.alreadyDownloaded(state.globalStorageUri), Connection$AgdaModeVscode.LatestALS.download(state.memento, state.globalStorageUri));
  if (conn.TAG === "Ok") {
    state.connection = conn._0;
    var match$2 = await Connection__Target$AgdaModeVscode.getPicked(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
    if (match$2.TAG !== "Ok") {
      return ;
    }
    var match$3 = match$2._0;
    if (match$3.TAG === "Agda") {
      var version = match$3._0;
      await State__View$AgdaModeVscode.Panel.displayStatus(state, "Agda v" + version);
      return await State__View$AgdaModeVscode.Panel.display(state, {
                  TAG: "Success",
                  _0: "Switched to Agda v" + version
                }, []);
    }
    var agdaVersion = match$3._1;
    var alsVersion = match$3._0;
    await State__View$AgdaModeVscode.Panel.displayStatus(state, "Agda v" + agdaVersion + " Language Server v" + alsVersion);
    return await State__View$AgdaModeVscode.Panel.display(state, {
                TAG: "Success",
                _0: "Switched to Agda v" + agdaVersion + " Language Server v" + alsVersion
              }, []);
  }
  var match$4 = Connection__Error$AgdaModeVscode.toString(conn._0);
  var header = {
    TAG: "Error",
    _0: "Failed to switch to a different installation: " + match$4[0]
  };
  var body = [Item$AgdaModeVscode.plainText(match$4[1])];
  return await State__View$AgdaModeVscode.Panel.display(state, header, body);
}

function make(state, run) {
  return {
          state: state,
          rerender: (function () {
              return run(state);
            }),
          quickPick: Vscode.window.createQuickPick(),
          items: [],
          subscriptions: []
        };
}

function render(self) {
  self.quickPick.items = self.items;
  self.quickPick.show();
}

function destroy(self) {
  self.quickPick.dispose();
  self.subscriptions.forEach(function (sub) {
        sub.dispose();
      });
}

var QP = {
  make: make,
  render: render,
  destroy: destroy
};

async function handleSelection(self, memento, globalStorageUri, selection) {
  var match = selection.label;
  switch (match) {
    case "$(cloud-download)  Download the latest Agda Language Server" :
        var platform = await Connection__Download__Platform$AgdaModeVscode.determine();
        var result;
        if (platform.TAG === "Ok") {
          var target = await Connection$AgdaModeVscode.LatestALS.alreadyDownloaded(globalStorageUri)();
          if (target !== undefined) {
            result = {
              TAG: "Ok",
              _0: [
                target,
                true
              ]
            };
          } else {
            var error = await Connection$AgdaModeVscode.LatestALS.download(memento, globalStorageUri)(platform._0);
            result = error.TAG === "Ok" ? ({
                  TAG: "Ok",
                  _0: [
                    error._0,
                    false
                  ]
                }) : ({
                  TAG: "Error",
                  _0: Connection__Download$AgdaModeVscode.$$Error.toString(error._0)
                });
          }
        } else {
          result = {
            TAG: "Error",
            _0: "Failed to determine the platform for downloading the Agda Language Server"
          };
        }
        if (result.TAG === "Ok") {
          var match$1 = result._0;
          var target$1 = match$1[0];
          var version;
          version = target$1.TAG === "Agda" ? "Agda v" + target$1._0 : "Agda v" + target$1._1 + " Language Server v" + target$1._0;
          if (match$1[1]) {
            await Vscode.window.showInformationMessage(version + " is already downloaded");
          } else {
            await self.rerender();
            await Vscode.window.showInformationMessage(version + " successfully downloaded");
          }
          return ;
        }
        await Vscode.window.showErrorMessage(result._0);
        return ;
    case "$(folder-opened)  Open download folder" :
        destroy(self);
        return await openGlobalStorageFolder(self.state);
    default:
      var original = await Connection__Target$AgdaModeVscode.getPicked(self.state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
      if (original.TAG !== "Ok") {
        return await self.rerender();
      }
      var rawPath = selection.detail;
      if (rawPath === undefined) {
        return ;
      }
      var selectionChanged = rawPath !== Connection__URI$AgdaModeVscode.toString(Connection__Target$AgdaModeVscode.toURI(original._0));
      if (!selectionChanged) {
        return ;
      }
      var url = Connection__URI$AgdaModeVscode.parse(rawPath);
      if (url.TAG === "Filepath") {
        var e = await Connection__Target$AgdaModeVscode.fromRawPath(url._0);
        if (e.TAG === "Ok") {
          await Connection__Target$AgdaModeVscode.setPicked(self.state.memento, e._0);
          await switchAgdaVersion(self.state);
          return await self.rerender();
        }
        console.log(e._0);
        return ;
      }
      console.log("Trying to connect with: " + url._0.toString());
      return ;
  }
}

async function run(state) {
  var qp = make(state, run);
  qp.quickPick.placeholder = "Switch Agda Version";
  var miscItems = [{
      description: "Where the language servers are downloaded to",
      label: "$(folder-opened)  Open download folder"
    }];
  var selected = await Connection__Target$AgdaModeVscode.getPicked(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
  var isSelected = function (target) {
    if (selected.TAG === "Ok" && target.TAG === "Ok") {
      return Caml_obj.equal(Connection__Target$AgdaModeVscode.toURI(selected._0), Connection__Target$AgdaModeVscode.toURI(target._0));
    } else {
      return false;
    }
  };
  var targetToItem = function (target) {
    if (target.TAG !== "Ok") {
      return {
              description: "",
              detail: Connection__Target$AgdaModeVscode.$$Error.toString(target._0),
              label: "$(error)  Bad path"
            };
    }
    var match = target._0;
    if (match.TAG === "Agda") {
      return {
              description: isSelected(target) ? "Selected" : "",
              detail: match._1,
              iconPath: {
                dark: Vscode.Uri.joinPath(Vscode.Uri.file(state.extensionPath), "asset/dark.png"),
                light: Vscode.Uri.joinPath(Vscode.Uri.file(state.extensionPath), "asset/light.png")
              },
              label: "Agda v" + match._0
            };
    }
    var method = match._2;
    var tmp;
    tmp = method.TAG === "ViaPipe" ? method._0 : method._0.toString();
    return {
            description: isSelected(target) ? "Selected" : "",
            detail: tmp,
            label: "$(squirrel)  Agda v" + match._1 + " Language Server v" + match._0
          };
  };
  var fetchSpecToItem = function (globalStoragePath, installedPaths) {
    return function (fetchSpec) {
      var getAgdaVersion = function (asset) {
        return asset.name.replace(/als-Agda-/, "").replace(/-.*/, "");
      };
      var agdaVersion = getAgdaVersion(fetchSpec.asset);
      var alsVersion = Core__Option.getOr(Core__Array.last(fetchSpec.release.name.split(".")), fetchSpec.release.name);
      var filename = Nodepath.join(globalStoragePath.fsPath, fetchSpec.saveAsFileName, "als");
      var downloaded = installedPaths.includes(filename);
      var versionString = "Agda v" + agdaVersion + " Language Server v" + alsVersion;
      return {
              description: downloaded ? "Downloaded and installed" : "",
              detail: versionString,
              label: "$(cloud-download)  Download the latest Agda Language Server"
            };
    };
  };
  var installedSeperator = [{
      kind: -1,
      label: "Installed"
    }];
  var installedTargets = await Connection$AgdaModeVscode.getInstalledTargetsAndPersistThem(state.globalStorageUri);
  var installedPaths = Core__Array.filterMap(Object.values(installedTargets), (function (x) {
          if (x.TAG === "Ok") {
            return Connection__URI$AgdaModeVscode.toString(Connection__Target$AgdaModeVscode.toURI(x._0));
          }
          
        }));
  var installedItemsFromSettings = Object.values(installedTargets).map(targetToItem);
  var downloadSeperator = [{
      kind: -1,
      label: "Download"
    }];
  var platform = await Connection__Download__Platform$AgdaModeVscode.determine();
  var downloadLatestALSFetchSpec;
  if (platform.TAG === "Ok") {
    var fetchSpec = await Connection$AgdaModeVscode.LatestALS.getFetchSpec(state.memento, state.globalStorageUri, platform._0);
    downloadLatestALSFetchSpec = fetchSpec.TAG === "Ok" ? fetchSpec._0 : undefined;
  } else {
    downloadLatestALSFetchSpec = undefined;
  }
  var downloadLatestALSItems = downloadLatestALSFetchSpec !== undefined ? [fetchSpecToItem(state.globalStorageUri, installedPaths)(downloadLatestALSFetchSpec)] : [];
  var items = [
      installedSeperator,
      installedItemsFromSettings,
      downloadSeperator,
      downloadLatestALSItems,
      miscItems
    ].flat();
  qp.items = items;
  render(qp);
  Util$AgdaModeVscode.Disposable.add(qp.quickPick.onDidChangeSelection(function (selectedItems) {
            Core__Option.forEach(selectedItems[0], (function (item) {
                    handleSelection(qp, state.memento, state.globalStorageUri, item);
                  }));
          }), qp.subscriptions);
  return Util$AgdaModeVscode.Disposable.add(qp.quickPick.onDidHide(function () {
                  destroy(qp);
                }), qp.subscriptions);
}

exports.openGlobalStorageFolder = openGlobalStorageFolder;
exports.switchAgdaVersion = switchAgdaVersion;
exports.QP = QP;
exports.handleSelection = handleSelection;
exports.run = run;
/* vscode Not a pure module */
