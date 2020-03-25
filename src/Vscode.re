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

// https://code.visualstudio.com/api/references/vscode-api#Memento
module Memento = {
  type t;
  // methods
  [@bs.send] external get: (t, string) => option('a) = "get";
  [@bs.send] external getWithDefault: (t, string, 'a) => 'a = "get";
  [@bs.send] external update: (t, string, 'a) => Promise.t(unit) = "update";
};

module ExtensionContext = {
  type t;
  // properties
  [@bs.get] external extensionPath: t => string = "extensionPath";
  [@bs.get] external globalState: t => Memento.t = "globalState";
  [@bs.get] external globalStoragePath: t => string = "globalStoragePath";
  [@bs.get] external logPath: t => string = "logPath";
  [@bs.get] external storagePath: t => option(string) = "storagePath";
  [@bs.get]
  external subscriptions: t => array(Disposable.t) = "subscriptions";
  [@bs.get] external workspaceState: t => Memento.t = "workspaceState";
  // methods
  [@bs.send] external asAbsolutePath: (t, string) => string = "asAbsolutePath";
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

type event('a) = ('a => unit) => Disposable.t;

module Terminal = {
  type t;
};

module WindowState = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEditorOptionsChangeEvent
module TextEditorOptionsChangeEvent = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeEvent
module TextEditorSelectionChangeEvent = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorSelectionChangeKind
module TextEditorSelectionChangeKind = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorViewColumnChangeEvent
module TextEditorViewColumnChangeEvent = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorVisibleRangesChangeEvent
module TextEditorVisibleRangesChangeEvent = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#InputBox
module InputBox = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#OutputChannel
module OutputChannel = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#QuickPickItem
module QuickPickItem = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#QuickPick
module QuickPick = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#StatusBarItem
module StatusBarItem = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TerminalOptions
module TerminalOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ExtensionTerminalOptions
module ExtensionTerminalOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#DecorationRenderOptions
module DecorationRenderOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextEditorDecorationType
module TextEditorDecorationType = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TreeViewOptions
module TreeViewOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TreeView
module TreeView = {
  type t;
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

module ViewColumnAndPreserveFocus = {
  type t = {
    preserveFocus: bool,
    viewColumn: int,
  };
};

// https://code.visualstudio.com/api/references/vscode-api#TreeDataProvider
module TreeDataProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#UriHandler
module UriHandler = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#WebviewPanelSerializer
module WebviewPanelSerializer = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#MessageOptions
module MessageOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#MessageItem
module MessageItem = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#InputBoxOptions
module InputBoxOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#CancellationToken
module CancellationToken = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#OpenDialogOptions
module OpenDialogOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#QuickPickOptions
module QuickPickOptions = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#SaveDialogOptions
module SaveDialogOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolderPickOptions
module WorkspaceFolderPickOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFolder
module WorkspaceFolder = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ProgressOptions
module ProgressOptions = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#Progress
module Progress = {
  type t('a);
};

// https://code.visualstudio.com/api/references/vscode-api#window
module Window = {
  // variables
  [@bs.module "vscode"] [@bs.scope "window"]
  external activeTerminal: option(Terminal.t) = "activeTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external activeTextEditor: option(TextEditor.t) = "activeTextEditor";
  [@bs.module "vscode"] [@bs.scope "window"]
  external state: WindowState.t = "state";
  [@bs.module "vscode"] [@bs.scope "window"]
  external terminals: array(Terminal.t) = "terminals";
  [@bs.module "vscode"] [@bs.scope "window"]
  external visibleTextEditors: array(TextEditor.t) = "visibleTextEditors";
  // events
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeActiveTerminal: event(option(Terminal.t)) =
    "onDidChangeActiveTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeActiveTextEditor: event(option(TextEditor.t)) =
    "onDidChangeActiveTextEditor";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorOptions: event(TextEditorOptionsChangeEvent.t) =
    "onDidChangeTextEditorOptions";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorSelection:
    event(TextEditorSelectionChangeEvent.t) =
    "onDidChangeTextEditorSelection";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorViewColumn:
    event(TextEditorViewColumnChangeEvent.t) =
    "onDidChangeTextEditorViewColumn";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorVisibleRanges:
    event(TextEditorVisibleRangesChangeEvent.t) =
    "onDidChangeTextEditorVisibleRanges";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeVisibleTextEditors: event(TextEditor.t) =
    "onDidChangeVisibleTextEditors";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeWindowState: event(WindowState.t) =
    "onDidChangeWindowState";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidCloseTerminal: event(Terminal.t) = "onDidCloseTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidOpenTerminal: event(Terminal.t) = "onDidOpenTerminal";
  // functions
  [@bs.module "vscode"] [@bs.scope "window"]
  external createInputBox: unit => InputBox.t = "createInputBox";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createOutputChannel: string => OutputChannel.t =
    "createOutputChannel";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createQuickPick: QuickPickItem.t => QuickPick.t = "createQuickPick";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createStatusBarItem:
    (~alignment: [@bs.int] [ | `Left | `Right]=?, ~priority: int=?, unit) =>
    StatusBarItem.t =
    "createStatusBarItem";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTerminal:
    (
      ~name: string=?,
      ~shellPath: string=?,
      ~shellArgs: array(string)=?,
      unit
    ) =>
    Terminal.t =
    "createTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTerminalWithTerminalOptions: TerminalOptions.t => Terminal.t =
    "createTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTerminalWithExtensionTerminalOptions:
    ExtensionTerminalOptions.t => Terminal.t =
    "createTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTextEditorDecorationType:
    DecorationRenderOptions.t => TextEditorDecorationType.t =
    "createTextEditorDecorationType";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createTreeView: (string, TreeViewOptions.t) => TreeView.t =
    "createTreeView";
  [@bs.module "vscode"] [@bs.scope "window"]
  external createWebviewPanel:
    (
      string,
      string,
      ViewColumnAndPreserveFocus.t,
      option(WebviewAndWebviewPanelOptions.t)
    ) =>
    WebviewPanel.t =
    "createWebviewPanel";
  [@bs.module "vscode"] [@bs.scope "window"]
  external registerTreeDataProvider:
    (string, TreeDataProvider.t) => Disposable.t =
    "registerTreeDataProvider";
  [@bs.module "vscode"] [@bs.scope "window"]
  external registerUriHandler: UriHandler.t => Disposable.t =
    "registerUriHandler";
  [@bs.module "vscode"] [@bs.scope "window"]
  external registerWebviewPanelSerializer:
    (string, WebviewPanelSerializer.t) => Disposable.t =
    "registerWebviewPanelSerializer";
  [@bs.module "vscode"] [@bs.scope "window"]
  external setStatusBarMessageAndHideAfterTimeout:
    (string, int) => Disposable.t =
    "setStatusBarMessage";
  [@bs.module "vscode"] [@bs.scope "window"]
  external setStatusBarMessageAndHideWhenDone:
    (string, Promise.t('a)) => Disposable.t =
    "setStatusBarMessage";
  [@bs.module "vscode"] [@bs.scope "window"]
  external setStatusBarMessage: string => Disposable.t = "setStatusBarMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showErrorMessage:
    (string, array(string)) => Promise.t(option(string)) =
    "showErrorMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showErrorMessageWithOptions:
    (string, MessageOptions.t, array(string)) => Promise.t(option(string)) =
    "showErrorMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showInformationMessage:
    (string, array(string)) => Promise.t(option(string)) =
    "showInformationMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showInformationMessageWithOptions:
    (string, MessageOptions.t, array(string)) => Promise.t(option(string)) =
    "showInformationMessage";

  // module InputBoxOptions = {
  //   type t = {
  //     ignoreFocusOut: bool,
  //     password: bool,
  //     placeHolder: string,
  //     value: string,
  //     valueSelection: (int, int),
  //   };
  // };
  // module CancellationToken = {
  //   type t = {isCancellationRequested: bool};
  // };
  [@bs.module "vscode"] [@bs.scope "window"]
  external showInputBox:
    (~option: InputBoxOptions.t=?, ~token: CancellationToken.t=?, unit) =>
    Promise.t(option(string)) =
    "showInputBox";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showOpenDialog: OpenDialogOptions.t => Promise.t(option(Uri.t)) =
    "shoeOpenDialog";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showQuickPick:
    (
      Promise.t(array(string)),
      QuickPickOptions.t,
      option(CancellationToken.t)
    ) =>
    Promise.t(option(array(string))) =
    "showQuickPick";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showSaveDialog: SaveDialogOptions.t => Promise.t(option(Uri.t)) =
    "showSaveDialog";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showTextDocument:
    (TextDocument.t, ~column: ViewColumn.t=?, ~preserveFocus: bool=?, unit) =>
    Promise.t(option(Uri.t)) =
    "showTextDocument";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showWarningMessage:
    (string, array(string)) => Promise.t(option(string)) =
    "showWarningMessage";
  [@bs.module "vscode"] [@bs.scope "window"] [@bs.variadic]
  external showWarningMessageWithOptions:
    (string, MessageOptions.t, array(string)) => Promise.t(option(string)) =
    "showWarningMessage";
  [@bs.module "vscode"] [@bs.scope "window"]
  external showWorkspaceFolderPick:
    option(WorkspaceFolderPickOptions.t) =>
    Promise.t(option(WorkspaceFolder.t)) =
    "showWorkspaceFolderPick";
  [@bs.module "vscode"] [@bs.scope "window"]
  external withProgress:
    (
      ProgressOptions.t,
      (
        Progress.t({
          .
          "increment": int,
          "message": string,
        }),
        CancellationToken.t
      ) =>
      Promise.t('a)
    ) =>
    Promise.t('a) =
    "withProgress";
  [@bs.module "vscode"] [@bs.scope "window"]
  external withScmProgress:
    ((Progress.t(int), CancellationToken.t) => Promise.t('a)) =>
    Promise.t('a) =
    "withScmProgress";
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