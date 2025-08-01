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
var Connection__URI$AgdaModeVscode = require("../Connection/Shared/Connection__URI.bs.js");
var Connection__Error$AgdaModeVscode = require("../Connection/Shared/Connection__Error.bs.js");
var Connection__Download$AgdaModeVscode = require("../Connection/Resolver/Connection__Download.bs.js");
var Connection__Endpoint$AgdaModeVscode = require("../Connection/Endpoint/Connection__Endpoint.bs.js");
var Connection__LatestALS$AgdaModeVscode = require("../Connection/Resolver/Connection__LatestALS.bs.js");

function formatAgdaVersion(version) {
  return "Agda v" + version;
}

function formatALSVersion(alsVersion, agdaVersion) {
  return "Agda v" + agdaVersion + " Language Server v" + alsVersion;
}

function formatSwitchingMessage(version) {
  return "Switching to " + version;
}

function formatSwitchedMessage(version) {
  return "Switched to " + version;
}

var VersionDisplay = {
  formatAgdaVersion: formatAgdaVersion,
  formatALSVersion: formatALSVersion,
  formatSwitchingMessage: formatSwitchingMessage,
  formatSwitchedMessage: formatSwitchedMessage
};

function parseSelection(label, detail) {
  switch (label) {
    case "$(cloud-download)  Download the latest Agda Language Server" :
        return "DownloadLatestALS";
    case "$(folder-opened)  Open download folder" :
        return "OpenFolder";
    default:
      if (detail !== undefined) {
        return {
                TAG: "SwitchToEndpoint",
                _0: detail
              };
      } else {
        return {
                TAG: "SwitchToEndpoint",
                _0: ""
              };
      }
  }
}

var SelectionParsing = {
  parseSelection: parseSelection
};

async function downloadLatestALS(platformDeps, memento, globalStorageUri) {
  var platform = await platformDeps.determinePlatform();
  if (platform.TAG !== "Ok") {
    return {
            TAG: "Failure",
            _0: "Failed to determine the platform for downloading the Agda Language Server"
          };
  }
  var endpoint = await platformDeps.alreadyDownloaded(globalStorageUri)();
  if (endpoint !== undefined) {
    return {
            TAG: "Success",
            _0: endpoint,
            _1: true
          };
  }
  var error = await platformDeps.downloadLatestALS(memento, globalStorageUri)(platform._0);
  if (error.TAG === "Ok") {
    return {
            TAG: "Success",
            _0: error._0,
            _1: false
          };
  } else {
    return {
            TAG: "Failure",
            _0: Connection__Download$AgdaModeVscode.$$Error.toString(error._0)
          };
  }
}

async function handleDownloadResult(result, rerender) {
  if (result.TAG === "Success") {
    var endpoint = result._0;
    var version;
    version = endpoint.TAG === "Agda" ? "Agda v" + endpoint._0 : formatALSVersion(endpoint._0, endpoint._1);
    if (result._1) {
      await Vscode.window.showInformationMessage(version + " is already downloaded");
    } else {
      await rerender();
      await Vscode.window.showInformationMessage(version + " successfully downloaded");
    }
    return ;
  }
  await Vscode.window.showErrorMessage(result._0);
}

var DownloadWorkflow = {
  downloadLatestALS: downloadLatestALS,
  handleDownloadResult: handleDownloadResult
};

function createSeparatorItem(label) {
  return {
          kind: -1,
          label: label
        };
}

function createFolderItem() {
  return {
          description: "Where the language servers are downloaded to",
          label: "$(folder-opened)  Open download folder"
        };
}

function createDownloadItem(downloaded, versionString) {
  return {
          description: downloaded ? "Downloaded and installed" : "",
          detail: versionString,
          label: "$(cloud-download)  Download the latest Agda Language Server"
        };
}

function createAgdaItem(version, path, isSelected, extensionUri) {
  return {
          description: isSelected ? "Selected" : "",
          detail: path,
          iconPath: {
            dark: Vscode.Uri.joinPath(extensionUri, "asset/dark.png"),
            light: Vscode.Uri.joinPath(extensionUri, "asset/light.png")
          },
          label: "Agda v" + version
        };
}

function createALSItem(alsVersion, agdaVersion, method, isSelected) {
  var tmp;
  tmp = method.TAG === "ViaPipe" ? method._0 : method._1.toString();
  return {
          description: isSelected ? "Selected" : "",
          detail: tmp,
          label: "$(squirrel)  " + formatALSVersion(alsVersion, agdaVersion)
        };
}

function createErrorItem(error) {
  return {
          description: "",
          detail: Connection__Endpoint$AgdaModeVscode.$$Error.toString(error),
          label: "$(error)  Bad path"
        };
}

var ItemCreation = {
  createSeparatorItem: createSeparatorItem,
  createFolderItem: createFolderItem,
  createDownloadItem: createDownloadItem,
  createAgdaItem: createAgdaItem,
  createALSItem: createALSItem,
  createErrorItem: createErrorItem
};

async function openGlobalStorageFolder(state) {
  await Vscode.env.openExternal(state.globalStorageUri);
}

async function switchAgdaVersion(state) {
  var match = await Connection__Endpoint$AgdaModeVscode.getPicked(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
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
            _0: "Switching to " + formatALSVersion(match$1._0, match$1._1)
          }, []);
    }
  }
  await Connection$AgdaModeVscode.destroy(state.connection);
  var conn = await Connection$AgdaModeVscode.make(state.platformDeps, state.memento, state.globalStorageUri, Config$AgdaModeVscode.Connection.getAgdaPaths(), [
        "als",
        "agda"
      ]);
  if (conn.TAG === "Ok") {
    state.connection = conn._0;
    var match$2 = await Connection__Endpoint$AgdaModeVscode.getPicked(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
    if (match$2.TAG !== "Ok") {
      return ;
    }
    var match$3 = match$2._0;
    if (match$3.TAG === "Agda") {
      var formattedVersion = "Agda v" + match$3._0;
      await State__View$AgdaModeVscode.Panel.displayStatus(state, formattedVersion);
      return await State__View$AgdaModeVscode.Panel.display(state, {
                  TAG: "Success",
                  _0: "Switched to " + formattedVersion
                }, []);
    }
    var formattedVersion$1 = formatALSVersion(match$3._0, match$3._1);
    await State__View$AgdaModeVscode.Panel.displayStatus(state, formattedVersion$1);
    return await State__View$AgdaModeVscode.Panel.display(state, {
                TAG: "Success",
                _0: "Switched to " + formattedVersion$1
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

async function handleSelection(self, platformDeps, memento, globalStorageUri, selection) {
  var rawPath = parseSelection(selection.label, selection.detail);
  if (typeof rawPath !== "object") {
    if (rawPath === "OpenFolder") {
      destroy(self);
      return await openGlobalStorageFolder(self.state);
    }
    var result = await downloadLatestALS(platformDeps, memento, globalStorageUri);
    return await handleDownloadResult(result, self.rerender);
  } else {
    var rawPath$1 = rawPath._0;
    var original = await Connection__Endpoint$AgdaModeVscode.getPicked(self.state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
    if (original.TAG !== "Ok") {
      return await self.rerender();
    }
    var selectionChanged = rawPath$1 !== Connection__URI$AgdaModeVscode.toString(Connection__Endpoint$AgdaModeVscode.toURI(original._0));
    if (!selectionChanged) {
      return ;
    }
    var match = Connection__URI$AgdaModeVscode.parse(rawPath$1);
    if (match.TAG === "FileURI") {
      var e = await Connection__Endpoint$AgdaModeVscode.fromVSCodeUri(match._1);
      if (e.TAG === "Ok") {
        await Connection__Endpoint$AgdaModeVscode.setPicked(self.state.memento, e._0);
        await switchAgdaVersion(self.state);
        return await self.rerender();
      }
      console.log(e._0);
      return ;
    }
    console.log("Trying to connect with: " + match._1.toString());
    return ;
  }
}

async function run(state, platformDeps) {
  var qp = make(state, (function (state) {
          return run(state, platformDeps);
        }));
  qp.quickPick.placeholder = "Switch Agda Version";
  var miscItems = [{
      description: "Where the language servers are downloaded to",
      label: "$(folder-opened)  Open download folder"
    }];
  var selected = await Connection__Endpoint$AgdaModeVscode.getPicked(state.memento, Config$AgdaModeVscode.Connection.getAgdaPaths());
  var isSelected = function (endpoint) {
    if (selected.TAG === "Ok" && endpoint.TAG === "Ok") {
      return Caml_obj.equal(Connection__Endpoint$AgdaModeVscode.toURI(selected._0), Connection__Endpoint$AgdaModeVscode.toURI(endpoint._0));
    } else {
      return false;
    }
  };
  var endpointToItem = function (endpoiont) {
    if (endpoiont.TAG !== "Ok") {
      return createErrorItem(endpoiont._0);
    }
    var match = endpoiont._0;
    if (match.TAG === "Agda") {
      return createAgdaItem(match._0, match._1, isSelected(endpoiont), state.extensionUri);
    } else {
      return createALSItem(match._0, match._1, match._2, isSelected(endpoiont));
    }
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
      var versionString = formatALSVersion(alsVersion, agdaVersion);
      return createDownloadItem(downloaded, versionString);
    };
  };
  var installedSeperator = [{
      kind: -1,
      label: "Installed"
    }];
  var installedEndpoints = await platformDeps.getInstalledEndpointsAndPersistThem(state.globalStorageUri);
  var installedPaths = Core__Array.filterMap(Object.values(installedEndpoints), (function (x) {
          if (x.TAG === "Ok") {
            return Connection__URI$AgdaModeVscode.toString(Connection__Endpoint$AgdaModeVscode.toURI(x._0));
          }
          
        }));
  var installedItemsFromSettings = Object.values(installedEndpoints).map(endpointToItem);
  var downloadSeperator = [{
      kind: -1,
      label: "Download"
    }];
  var platform = await platformDeps.determinePlatform();
  var downloadLatestALSFetchSpec;
  if (platform.TAG === "Ok") {
    var fetchSpec = await Connection__LatestALS$AgdaModeVscode.getFetchSpec(state.memento, state.globalStorageUri, platform._0);
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
                    handleSelection(qp, platformDeps, state.memento, state.globalStorageUri, item);
                  }));
          }), qp.subscriptions);
  return Util$AgdaModeVscode.Disposable.add(qp.quickPick.onDidHide(function () {
                  destroy(qp);
                }), qp.subscriptions);
}

exports.VersionDisplay = VersionDisplay;
exports.SelectionParsing = SelectionParsing;
exports.DownloadWorkflow = DownloadWorkflow;
exports.ItemCreation = ItemCreation;
exports.openGlobalStorageFolder = openGlobalStorageFolder;
exports.switchAgdaVersion = switchAgdaVersion;
exports.QP = QP;
exports.handleSelection = handleSelection;
exports.run = run;
/* vscode Not a pure module */
