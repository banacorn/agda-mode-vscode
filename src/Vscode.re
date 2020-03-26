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

// https://code.visualstudio.com/api/references/vscode-api#Position
module Position = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#Range
module Range = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#RegExp
module RegExp = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TextLine
module TextLine = {
  type t;
  // properties
  [@bs.get]
  external firstNonWhitespaceCharacterIndex: t => int =
    "firstNonWhitespaceCharacterIndex";
  [@bs.get] external isEmptyOrWhitespace: t => bool = "isEmptyOrWhitespace";
  [@bs.get] external lineNumber: t => int = "lineNumber";
  [@bs.get] external range: t => Range.t = "range";
  [@bs.get]
  external rangeIncludingLineBreak: t => Range.t = "rangeIncludingLineBreak";
  [@bs.get] external text: t => string = "text";
};

module TextDocument = {
  type t;
  // properties
  [@bs.get] external eol: t => int = "eol";
  [@bs.get] external fileName: t => string = "fileName";
  [@bs.get] external isClosed: t => bool = "isClosed";
  [@bs.get] external isDirty: t => bool = "isDirty";
  [@bs.get] external isUntitled: t => bool = "isUntitled";
  [@bs.get] external languageId: t => string = "languageId";
  [@bs.get] external lineCount: t => int = "lineCount";
  [@bs.get] external uri: t => Uri.t = "uri";
  [@bs.get] external version: t => int = "version";
  // methods
  [@bs.send] external getText: (t, option(Range.t)) => string = "getText";
  [@bs.send]
  external getWordRangeAtPosition:
    (t, Position.t, option(RegExp.t)) => option(Range.t) =
    "getWordRangeAtPosition";
  [@bs.send] external lineAt: (t, int) => TextLine.t = "lineAt";
  [@bs.send] external lineAtPosition: (t, Position.t) => TextLine.t = "lineAt";
  [@bs.send] external offsetAt: (t, Position.t) => int = "offsetAt";
  [@bs.send] external positionAt: (t, int) => Position.t = "positionAt";
  [@bs.send] external save: t => Promise.t(bool) = "save";
  [@bs.send]
  external validatePosition: (t, Position.t) => Position.t =
    "validatePosition";
  [@bs.send] external validateRange: (t, Range.t) => Range.t = "validateRange";
};

module TextEditor = {
  type t = {document: TextDocument.t};
};

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
  external onDidChangeActiveTerminal:
    (option(Terminal.t) => unit) => Disposable.t =
    "onDidChangeActiveTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeActiveTextEditor:
    (option(TextEditor.t) => unit) => Disposable.t =
    "onDidChangeActiveTextEditor";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorOptions:
    (option(TextEditorOptionsChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorOptions";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorSelection:
    (option(TextEditorSelectionChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorSelection";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorViewColumn:
    (option(TextEditorViewColumnChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorViewColumn";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeTextEditorVisibleRanges:
    (option(TextEditorVisibleRangesChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextEditorVisibleRanges";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeVisibleTextEditors:
    (option(TextEditor.t) => unit) => Disposable.t =
    "onDidChangeVisibleTextEditors";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidChangeWindowState:
    (option(WindowState.t) => unit) => Disposable.t =
    "onDidChangeWindowState";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidCloseTerminal: (option(Terminal.t) => unit) => Disposable.t =
    "onDidCloseTerminal";
  [@bs.module "vscode"] [@bs.scope "window"]
  external onDidOpenTerminal: (option(Terminal.t) => unit) => Disposable.t =
    "onDidOpenTerminal";
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

// https://code.visualstudio.com/api/references/vscode-api#FileSystem
module FileSystem = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ConfigurationChangeEvent
module ConfigurationChangeEvent = {
  type t;

  [@bs.send]
  external affectsConfiguration:
    (
      t,
      string,
      [@bs.unwrap] [
        | `Uri(Uri.t)
        | `TextDocument(TextDocument.t)
        | `WorkspaceFolder(WorkspaceFolder.t)
        | `Others(
            option({
              .
              "languageId": string,
              "uri": Uri.t,
            }),
          )
      ]
    ) =>
    bool =
    "affectsConfiguration";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentChangeEvent
module TextDocumentContentChangeEvent = {
  type t;
  // properties
  [@bs.get] external range: t => Range.t = "range";
  [@bs.get] external rangeLength: t => int = "rangeLength";
  [@bs.get] external rangeOffset: t => int = "rangeOffset";
  [@bs.get] external text: t => string = "text";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentChangeEvent
module TextDocumentChangeEvent = {
  type t;
  // properties
  [@bs.get]
  external contentChanges: t => array(TextDocumentContentChangeEvent.t) =
    "contentChanges";
  [@bs.get] external document: t => TextDocument.t = "document";
};

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceFoldersChangeEvent
module WorkspaceFoldersChangeEvent = {
  type t;
  // properties
  [@bs.get] external added: t => array(WorkspaceFolder.t) = "added";
  [@bs.get] external removed: t => array(WorkspaceFolder.t) = "removed";
};

// https://code.visualstudio.com/api/references/vscode-api#FileCreateEvent
module FileCreateEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
};

// https://code.visualstudio.com/api/references/vscode-api#FileDeleteEvent
module FileDeleteEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
};

// https://code.visualstudio.com/api/references/vscode-api#FileRenameEvent
module FileRenameEvent = {
  type t;
  // properties
  [@bs.get]
  external files:
    t =>
    array({
      .
      "newUri": Uri.t,
      "oldUri": Uri.t,
    }) =
    "files";
};

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceEdit
module WorkspaceEdit = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FileWillCreateEvent
module FileWillCreateEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
  // methods
  [@bs.send]
  external waitUntilWithWorkspaceEdit: (t, Promise.t(WorkspaceEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};
// https://code.visualstudio.com/api/references/vscode-api#FileWillDeleteEvent
module FileWillDeleteEvent = {
  type t;
  // properties
  [@bs.get] external files: t => array(Uri.t) = "files";
  // methods
  [@bs.send]
  external waitUntilWithWorkspaceEdit: (t, Promise.t(WorkspaceEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};
// https://code.visualstudio.com/api/references/vscode-api#FileWillRenameEvent
module FileWillRenameEvent = {
  type t;
  // properties
  [@bs.get]
  external files:
    t =>
    array({
      .
      "newUri": Uri.t,
      "oldUri": Uri.t,
    }) =
    "files";
  // methods
  [@bs.send]
  external waitUntilWithWorkspaceEdit: (t, Promise.t(WorkspaceEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentSaveReason
module TextDocumentSaveReason = {
  type t =
    | AfterDelay
    | FocusOut
    | Manual;

  let toEnum =
    fun
    | AfterDelay => 2
    | FocusOut => 3
    | Manual => 1;
  let fromEnum =
    fun
    | 2 => AfterDelay
    | 3 => FocusOut
    | _ => Manual;
};

// https://code.visualstudio.com/api/references/vscode-api#TextEdit
module TextEdit = {
  type t;
};
// https://code.visualstudio.com/api/references/vscode-api#TextDocumentWillSaveEvent
module TextDocumentWillSaveEvent = {
  type t;
  // properties
  [@bs.get] external document: t => TextDocument.t = "document";
  [@bs.get] external reason_raw: t => int = "reason";
  let reason = self => TextDocumentSaveReason.fromEnum(self->reason_raw);
  // methods
  [@bs.send]
  external waitUntilWithTextEdit: (t, Promise.t(TextEdit.t)) => unit =
    "waitUntil";
  [@bs.send] external waitUntil: (t, Promise.t('a)) => unit = "waitUntil";
};

// https://code.visualstudio.com/api/references/vscode-api#GlobPattern
module GlobPattern = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FileSystemWatcher
module FileSystemWatcher = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#ConfigurationScope
module ConfigurationScope = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#WorkspaceConfiguration
module WorkspaceConfiguration = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TextDocumentContentProvider
module TextDocumentContentProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#TaskProvider
module TaskProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#FileSystemProvider
module FileSystemProvider = {
  type t;
};

// https://code.visualstudio.com/api/references/vscode-api#workspace
module Workspace = {
  // variables
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external fs: FileSystem.t = "fs";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external name: option(string) = "name";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external rootPath: option(string) = "rootPath";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external textDocuments: array(TextDocument.t) = "textDocuments";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external workspaceFile: option(Uri.t) = "workspaceFile";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external workspaceFolders: option(array(WorkspaceFolder.t)) =
    "workspaceFolders";

  // events
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidChangeConfiguration:
    (option(ConfigurationChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeConfiguration";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidChangeTextDocument:
    (option(TextDocumentChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidChangeWorkspaceFolders:
    (option(WorkspaceFoldersChangeEvent.t) => unit) => Disposable.t =
    "onDidChangeWorkspaceFolders";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidCloseTextDocument:
    (option(TextDocument.t) => unit) => Disposable.t =
    "onDidCloseTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidCreateFiles:
    (option(FileCreateEvent.t) => unit) => Disposable.t =
    "onDidCreateFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidDeleteFiles:
    (option(FileDeleteEvent.t) => unit) => Disposable.t =
    "onDidDeleteFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidOpenTextDocument:
    (option(TextDocument.t) => unit) => Disposable.t =
    "onDidOpenTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidRenameFiles:
    (option(FileRenameEvent.t) => unit) => Disposable.t =
    "onDidRenameFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onDidSaveTextDocument:
    (option(TextDocument.t) => unit) => Disposable.t =
    "onDidSaveTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillCreateFiles:
    (option(FileWillCreateEvent.t) => unit) => Disposable.t =
    "onWillCreateFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillDeleteFiles:
    (option(FileWillDeleteEvent.t) => unit) => Disposable.t =
    "onWillDeleteFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillRenameFiles:
    (option(FileWillRenameEvent.t) => unit) => Disposable.t =
    "onWillRenameFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external onWillSaveTextDocument:
    (option(TextDocumentWillSaveEvent.t) => unit) => Disposable.t =
    "onWillSaveTextDocument";
  // functions
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external applyEdit: WorkspaceEdit.t => Promise.t(bool) = "applyEdit";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external asRelativePath: (string, option(bool)) => string =
    "asRelativePath";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external asRelativePathWithUri: (Uri.t, option(bool)) => string =
    "asRelativePath";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external createFileSystemWatcher:
    (
      GlobPattern.t,
      ~ignoreCreateEvents: bool=?,
      ~ignoreChangeEvents: bool=?,
      ~ignoreDeleteEvents: bool=?
    ) =>
    FileSystemWatcher.t =
    "createFileSystemWatcher";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external findFiles:
    (
      GlobPattern.t,
      ~exclude: Js.nullable(GlobPattern.t)=?,
      ~token: CancellationToken.t=?
    ) =>
    Promise.t(array(Uri.t)) =
    "findFiles";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external getConfiguration:
    (
      ~section: option(ConfigurationScope.t)=?,
      ~scope: Js.nullable(ConfigurationScope.t)=?
    ) =>
    WorkspaceConfiguration.t =
    "getConfiguration";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external getWorkspaceFolder: Uri.t => option(WorkspaceFolder.t) =
    "getWorkspaceFolder";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external openTextDocument: Uri.t => Promise.t(TextDocument.t) =
    "openTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external openTextDocumentWithFileName: string => Promise.t(TextDocument.t) =
    "openTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external openTextDocumentWithOptions:
    option({
      .
      "content": string,
      "language": string,
    }) =>
    Promise.t(TextDocument.t) =
    "openTextDocument";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external registerFileSystemProvider:
    (
      string,
      FileSystemProvider.t,
      option({
        .
        "isCaseSensitive": bool,
        "isReadonly": bool,
      })
    ) =>
    Disposable.t =
    "registerFileSystemProvider";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external registerTaskProvider: (string, TaskProvider.t) => Disposable.t =
    "registerTaskProvider";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external registerTextDocumentContentProvider:
    (string, TextDocumentContentProvider.t) => Disposable.t =
    "registerTextDocumentContentProvider";
  [@bs.module "vscode"] [@bs.scope "workspace"]
  external saveAll: option(bool) => Promise.t(bool) = "saveAll";
  [@bs.module "vscode"] [@bs.scope "workspace"] [@bs.variadic]
  external updateWorkspaceFolders:
    (
      int,
      option(int),
      array({
        .
        "name": string,
        "uri": Uri.t,
      })
    ) =>
    Promise.t(bool) =
    "updateWorkspaceFolders";
};