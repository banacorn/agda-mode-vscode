module Disposable = {
  type t;
};

module Event = {
  type t('a) = ('a => unit) => Disposable.t;
};

module ExtensionContext = {
  type t = {subscriptions: array(Disposable.t)};
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
};

module ViewColumn = {
  type t = int;
};

module WebviewPanel = {
  module Webview = {
    type t = {mutable html: string};
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

  module WebviewOption = {
    type t = {
      preserveFocus: bool,
      viewColumn: int,
    };
  };

  [@bs.module "vscode"] [@bs.scope "window"]
  external createWebviewPanel:
    (string, string, WebviewOption.t) => WebviewPanel.t =
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