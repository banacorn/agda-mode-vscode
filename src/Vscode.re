module Api = {
  type t;

  [@bs.val] external acquireVsCodeApi: unit => t = "acquireVsCodeApi";

  [@bs.send] external postMessage: (t, 'a) => unit = "postMessage";
  let onMessage = (callback: 'a => unit): unit => {
    Webapi.Dom.window
    |> Webapi.Dom.Window.addEventListener("message", _event => {
         callback([%raw "_event.data"])
       });
  };
};

module Disposable = {
  type t;
};

module Event = {
  type t('a) = ('a => unit) => Disposable.t;
};

module ExtensionContext = {
  type t = {
    extensionPath: string,
    subscriptions: array(Disposable.t),
  };
};

module Commands = {
  [@bs.module "vscode"] [@bs.scope "commands"]
  external registerCommand: (string, 'a => unit) => Disposable.t =
    "registerCommand";

  // [@bs.module "vscode"] [@bs.scope "commands"]
  // external executeCommand: string => Promise.t('a) = "executeCommand";

  module Layout = {
    [@unboxed]
    type group =
      | Group('a): group;

    type sized = {
      groups: array(group),
      size: float,
    };

    let simple = Group(Js.Dict.empty());
    let sized = (v: sized) => Group(v);

    type t = {
      orientation: int,
      groups: array(group),
    };
  };

  [@bs.module "vscode"] [@bs.scope "commands"]
  external executeCommand:
    (
    [@bs.string]
    [ | [@bs.as "vscode.setEditorLayout"] `setEditorLayout(Layout.t)]
    ) =>
    Promise.t('a) =
    "executeCommand";
};

module Uri = {
  type t;

  [@bs.module "vscode"] [@bs.scope "Uri"] external file: string => t = "file";
  [@bs.module "vscode"] [@bs.scope "Uri"]
  external parse: (string, option(bool)) => t = "file";

  [@bs.new]
  external make: (string, string, string, string, string) => t = "Uri";

  [@bs.get] external authority: t => string = "authority";
  [@bs.get] external fragment: t => string = "fragment";
  [@bs.get] external fsPath: t => string = "fsPath";
  [@bs.get] external path: t => string = "path";
  [@bs.get] external query: t => string = "query";
  [@bs.get] external scheme: t => string = "scheme";

  [@bs.send] external toJSON: t => Js.Json.t = "toJSON";
  [@bs.send] external toString: t => string = "toString";

  type change = {
    authority: option(string),
    fragment: option(string),
    path: option(string),
    query: option(string),
    scheme: option(string),
  };

  let makeChange =
      (~authority=?, ~fragment=?, ~path=?, ~query=?, ~scheme=?, ()): change => {
    authority,
    fragment,
    path,
    query,
    scheme,
  };
  [@bs.send] external with_: (t, change) => t = "with";
};

module ViewColumn = {
  type t = int;
};
module WebviewOptions = {
  type portMapping;
  type t = {
    enableCommandUris: option(bool),
    enableScripts: option(bool),
    localResourceRoots: option(array(Uri.t)),
    portMapping: option(array(portMapping)),
  };
};

// https://code.visualstudio.com/api/references/vscode-api#Webview
module Webview = {
  type t;
  // events
  [@bs.send]
  external onDidReceiveMessage: (t, 'a) => Disposable.t =
    "onDidReceiveMessage";
  // properties
  [@bs.get] external cspSource: t => string = "cspSource";
  [@bs.get] external html: t => string = "html";
  [@bs.set] external setHtml: (t, string) => unit = "html";

  [@bs.get] external options: t => WebviewOptions.t = "options";
  // methods
  [@bs.send] external asWebviewUri: (t, Uri.t) => Uri.t = "asWebviewUri";
  [@bs.send] external postMessage: (t, 'a) => Promise.t(bool) = "postMessage";
};

module WebviewPanel = {
  type options = {
    enableFindWidget: option(bool),
    retainContextWhenHidden: option(bool),
  };

  module IconPath = {
    [@unboxed]
    type t =
      | IconPath('a): t;

    type both = {
      dark: Uri.t,
      light: Uri.t,
    };

    type case =
      | Single(Uri.t)
      | Both(Uri.t, Uri.t);

    let single = (uri: Uri.t) => IconPath(uri);
    let both = (dark: Uri.t, light: Uri.t) => IconPath({dark, light});

    let classify = (IconPath(v): t): case =>
      if ([%raw {|v.dark === undefined|}]) {
        Single(Obj.magic(v): Uri.t);
      } else {
        Both(Obj.magic(v): Uri.t, Obj.magic(v): Uri.t);
      };
  };

  module Options = {
    type t = {
      enableFindWidget: option(bool),
      retainContextWhenHidden: option(bool),
    };
  };

  type t = {
    active: bool,
    iconPath: option(IconPath.t),
    options: Options.t,
    title: string,
    viewColumn: option(ViewColumn.t),
    viewType: string,
    visible: bool,
    webview: Webview.t,
  };

  [@bs.send] external dispose: t => unit = "dispose";
  [@bs.send]
  external reveal:
    (t, ~viewColumn: ViewColumn.t=?, ~preserveFocus: bool=?, unit) => unit =
    "reveal";

  type onDidChangeViewStateEvent = {webviewPanel: t};

  [@bs.send]
  external onDidChangeViewState: (t, onDidChangeViewStateEvent) => Disposable.t =
    "onDidChangeViewState";

  [@bs.send]
  external onDidDispose: (t, unit => unit) => Disposable.t = "onDidDispose";
};

module TextDocument = {
  type t = {
    eol: int,
    fileName: string,
    isClosed: bool,
    isDirty: bool,
    isUntitled: bool,
    languageId: string,
    lineCount: int,
    uri: Uri.t,
    version: int,
  };
  // [@bs.send]
  // external onDidDispose: (t, unit => unit) => Disposable.t = "onDidDispose";
};

module TextEditor = {
  type t = {document: TextDocument.t};
};

module Window = {
  [@bs.module "vscode"] [@bs.scope "window"]
  external activeTextEditor: option(TextEditor.t) = "activeTextEditor";

  [@bs.module "vscode"] [@bs.scope "window"]
  external showInformationMessage: string => unit = "showInformationMessage";

  module InputBoxOptions = {
    type t = {
      ignoreFocusOut: bool,
      password: bool,
      placeHolder: string,
      value: string,
      valueSelection: (int, int),
    };
  };
  module CancellationToken = {
    type t = {isCancellationRequested: bool};
  };
  [@bs.module "vscode"] [@bs.scope "window"]
  external showInputBox:
    (~option: InputBoxOptions.t=?, ~token: CancellationToken.t=?, unit) =>
    Promise.t(option(string)) =
    "showInputBox";

  type viewColumn = int;
  type showOptions = {
    preserveFocus: bool,
    viewColumn,
  };

  // WebviewPanelOptions & WebviewOptions
  module WebviewAndWebviewPanelOptions = {
    type t = {
      enableCommandUris: option(bool),
      enableScripts: option(bool),
      localResourceRoots: option(array(Uri.t)),
      portMapping: option(array(WebviewOptions.portMapping)),
      enableFindWidget: option(bool),
      retainContextWhenHidden: option(bool),
    };

    let make =
        (
          ~enableCommandUris=?,
          ~enableScripts=?,
          ~localResourceRoots=?,
          ~portMapping=?,
          ~enableFindWidget=?,
          ~retainContextWhenHidden=?,
          (),
        )
        : t => {
      enableCommandUris,
      enableScripts,
      localResourceRoots,
      portMapping,
      enableFindWidget,
      retainContextWhenHidden,
    };
  };

  [@bs.module "vscode"] [@bs.scope "window"]
  external createWebviewPanel:
    (string, string, viewColumn, option(WebviewAndWebviewPanelOptions.t)) =>
    WebviewPanel.t =
    "createWebviewPanel";

  [@bs.module "vscode"] [@bs.scope "window"]
  external createWebviewPanel':
    (string, string, showOptions, option(WebviewAndWebviewPanelOptions.t)) =>
    WebviewPanel.t =
    "createWebviewPanel";

  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeActiveTextEditor:
    (option(TextEditor.t) => unit) => Disposable.t =
    "onDidChangeActiveTextEditor";
};

module Workspace = {
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external textDocuments: array(TextDocument.t) = "textDocuments";

  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidOpenTextDocument: (TextDocument.t => unit) => Disposable.t =
    "onDidOpenTextDocument";

  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidCloseTextDocument: (TextDocument.t => unit) => Disposable.t =
    "onDidCloseTextDocument";
};