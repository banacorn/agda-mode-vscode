open VSCode;

type editor = TextEditor.t;
type context = ExtensionContext.t;
module Disposable = {
  type t = Disposable.t;
  let make = Disposable.make;
  let dispose = Disposable.dispose;
};
type view = View__Controller.t;

type ordering =
  | GT
  | EQ
  | LT;

module Point = {
  type t = Position.t;
  let make = Position.make;
  let line = Position.line;
  let column = Position.character;
  let translate = Position.translate;
  let compare = (x, y) =>
    switch (Position.compareTo(x, y)) {
    | (-1) => LT
    | 1 => GT
    | _ => EQ
    };
};

let pointAtOffset = (editor, offset) =>
  editor->TextEditor.document->TextDocument.positionAt(offset);

let offsetAtPoint = (editor, point) =>
  editor->TextEditor.document->TextDocument.offsetAt(point);

module Range = {
  type t = Range.t;
  let make = Range.make;
  let start = Range.start;
  let end_ = Range.end_;

  let contains = Range.contains;
  let containsRange = Range.containsRange;
  let fromOffset = (editor, (start, end_)) =>
    make(pointAtOffset(editor, start), pointAtOffset(editor, end_));
};

type fileName = string;

open! Belt;

let editorType = Sig.VsCode;

let getExtensionPath = context => context->ExtensionContext.extensionPath;

let getFileName = editor =>
  Some(editor->TextEditor.document->TextDocument.fileName->Parser.filepath);

let getEditor = fileName =>
  Window.showTextDocumentWithUri(Uri.file(fileName), None);

let save = editor => editor->TextEditor.document->TextDocument.save;

let addToSubscriptions = (disposable, context) =>
  disposable->Js.Array.push(context->ExtensionContext.subscriptions)->ignore;
// let onOpenEditor = callback => Workspace.onDidRenameFiles(event => ());
// when the editor got closed
let onDidCloseEditor = callback =>
  Workspace.onDidCloseTextDocument(textDoc =>
    textDoc->Option.forEach(textDoc =>
      callback(textDoc->TextDocument.fileName)
    )
  );

let onDidChangeFileName = callback =>
  Workspace.onDidRenameFiles(event =>
    event
    ->Option.map(FileRenameEvent.files)
    ->Option.forEach(files => {
        files->Array.forEach(file =>
          callback(
            Some(file##oldUri->Uri.path),
            Some(file##newUri->Uri.path),
          )
        )
      })
  );
let onDidChangeActivation = callback => {
  let previous = ref(Window.activeTextEditor);

  Window.onDidChangeActiveTextEditor(next =>
    if (next->Option.flatMap(getFileName)
        != (previous^)->Option.flatMap(getFileName)) {
      callback(previous^, next);
      previous := next;
    }
  );
};

let registerCommand = (name, callback) =>
  Commands.registerCommand("agda-mode." ++ name, () =>
    Window.activeTextEditor->Option.flatMap(editor =>
      editor->getFileName->Option.map(fileName => callback(editor, fileName))
    )
  );

let setContext = Commands.setContext;

//
// Configuration
//
module Config = {
  // Agda path
  let setAgdaPath = path =>
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.updateGlobalSettings("agdaPath", path, None);
  let getAgdaPath = () =>
    Workspace.getConfiguration(Some("agdaMode"), None)
    ->WorkspaceConfiguration.get("agdaPath");

  // Library path
  let getLibraryPath = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)
      ->WorkspaceConfiguration.get("libraryPath")
      ->Option.getWithDefault("");
    // split by comma, and clean them up
    Js.String.split(",", raw)
    ->Array.keep(x => x !== "")
    ->Array.map(Parser.filepath);
  };
  // Highlighting method
  let getHighlightingMethod = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)
      ->WorkspaceConfiguration.get("highlightingMethod");
    switch (raw) {
    | Some("Direct") => true
    | _ => false
    };
  };
  // Backend
  let getBackend = () => {
    let raw =
      Workspace.getConfiguration(Some("agdaMode"), None)
      ->WorkspaceConfiguration.get("backend");
    switch (raw) {
    | Some("GHC") => "GHCNoMain"
    | Some("LaTeX") => "LaTeX"
    | Some("QuickLaTeX") => "QuickLaTeX"
    | _ => "GHCNoMain"
    };
  };
};

//
// View
//

module View = {
  include View__Controller;
  // override View.make to inject editor-dependent arguments
  let make = View__Controller.make;
};
//
// Decoration
//

module Decoration = {
  type t = TextEditorDecorationType.t;
  type backgroundStyle = string;
  type foregroundStyle = string;
  type color = string;

  let decorate =
      (editor: editor, decoration: t, ranges: array(VSCode.Range.t)) => {
    editor->TextEditor.setDecorations(decoration, ranges);
  };
  //
  let highlightBackgroundPrim =
      (
        editor: editor,
        backgroundColor: ThemeColor.stringOrThemeColor,
        range: VSCode.Range.t,
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options =
      DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };
  let highlightBackground =
      (editor: editor, style: backgroundStyle, range: VSCode.Range.t) =>
    highlightBackgroundPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      range,
    );

  let highlightBackgroundWithColor =
      (editor: editor, color: color, range: VSCode.Range.t) =>
    highlightBackgroundPrim(editor, ThemeColor.string(color), range);

  let decorateTextPrim =
      (
        editor: editor,
        color: ThemeColor.stringOrThemeColor,
        range: VSCode.Range.t,
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options = DecorationRenderOptions.t(~color, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };
  let decorateText =
      (editor: editor, style: backgroundStyle, range: VSCode.Range.t) =>
    decorateTextPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      range,
    );

  let decorateTextWithColor =
      (editor: editor, color: color, range: VSCode.Range.t) =>
    decorateTextPrim(editor, ThemeColor.string(color), range);

  let overlayTextPrim =
      (
        editor: editor,
        color: ThemeColor.stringOrThemeColor,
        text: string,
        range: VSCode.Range.t,
      ) => {
    let after =
      ThemableDecorationAttachmentRenderOptions.t(
        ~contentText=text,
        ~color,
        (),
      );

    let options = DecorationRenderOptions.t(~after, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };

  let overlayText =
      (
        editor: editor,
        style: foregroundStyle,
        text: string,
        range: VSCode.Range.t,
      ) =>
    overlayTextPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      text,
      range,
    );

  let overlayTextWithColor =
      (editor: editor, color: color, text: string, range: VSCode.Range.t) =>
    overlayTextPrim(editor, ThemeColor.string(color), text, range);

  let underlineText = (editor: editor, range: VSCode.Range.t) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.OpenOpen);
    let textDecoration = "underline dotted";
    let options =
      DecorationRenderOptions.t(~rangeBehavior, ~textDecoration, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, [|range|]);
    decoration;
  };

  // ThemeColor.themeColor(
  //   switch (kind) {
  //   | Error => ThemeColor.make("errorForeground")
  //   | Highlight => ThemeColor.make("descriptionForeground")
  //   | Spec => ThemeColor.make("descriptionForeground")
  //   },
  // ),
  let destroy = TextEditorDecorationType.dispose;
};

let focus = editor => {
  Window.showTextDocument(
    editor->TextEditor.document,
    ~column=ViewColumn.Beside,
    (),
  )
  ->ignore;
};

let getSelectionRange = editor => {
  let selection = editor->TextEditor.selection;
  VSCode.Range.make(Selection.start(selection), Selection.end_(selection));
};

let getSelectionRanges = editor => {
  editor
  ->TextEditor.selections
  ->Array.map(selection => {
      VSCode.Range.make(
        Selection.start(selection),
        Selection.end_(selection),
      )
    });
};

let getCursorPosition = editor => editor->TextEditor.selection->Selection.end_;
let getCursorPositions = editor =>
  editor->TextEditor.selections->Array.map(Selection.end_);
let setCursorPosition = (editor, point) =>
  editor->TextEditor.setSelection(Selection.make(point, point));
let onChangeCursorPosition = callback =>
  Window.onDidChangeTextEditorSelection(event =>
    callback(
      event
      ->TextEditorSelectionChangeEvent.selections
      ->Array.map(Selection.anchor),
    )
  );

let rangeForLine = (editor, line) =>
  editor->TextEditor.document->TextDocument.lineAt(line)->TextLine.range;

let characterWidth: string => int = [%raw
  "function (string) {return [...string].length}"
];

// Code unit: a bit sequence used to encode each character of a repertoire within a given encoding form.
// returns `offset + 1` if `offset` cuts into the middle of a character of 2 code units wide
let codeUnitEndingOffset = (editor: editor, offset: int): (int, int) => {
  let range =
    VSCode.Range.make(
      VSCode.Position.make(0, 0), // start
      editor->TextEditor.document->TextDocument.positionAt(offset + 1) // end
    );
  // from `0` to `offset + 1`
  let textWithLookahead =
    editor->TextEditor.document->TextDocument.getText(Some(range));
  // from `0` to `offset`
  let textWithoutLookahead =
    Js.String.substring(~from=0, ~to_=offset, textWithLookahead);

  let charOffsetWithLookahead = characterWidth(textWithLookahead);
  let charOffsetWithoutLookahead = characterWidth(textWithoutLookahead);

  // if there's a character ranges from `offset - 1` to `offset + 1`
  // the character offset of `textWithLookahead` should be the same as `textWithoutLookahead`
  if (charOffsetWithLookahead == charOffsetWithoutLookahead) {
    (offset + 1, charOffsetWithoutLookahead);
  } else {
    (offset, charOffsetWithoutLookahead);
  };
};

// for converting offsets sent from Agda (UTF-8) to offsets in the editor (UTF-16)
let fromAgdaOffset = (editor, offset) => {
  // the native VS Code API uses UTF-16 internally and is bad at calculating widths of charactors
  // for example the width of grapheme cluster "𝕁" is 1 for Agda, but 2 for VS Code
  // we need to offset that difference here

  let rec approximate = (target, offset) => {
    // update `offset` in case that it cuts a grapheme in half, also calculates the current code unit offset
    let (offset, current) = codeUnitEndingOffset(editor, offset);
    if (target == current) {
      // return the current position if the target is met
      offset;
    } else {
      // else, offset by `target - current` to see if we can approximate the target
      let (nextOffset, nextCurrent) =
        codeUnitEndingOffset(editor, offset + target - current);

      if (current == nextCurrent) {
        // return it's not progressing anymore
        nextOffset;
      } else {
        approximate(target, nextOffset);
      };
    };
  };
  approximate(offset, offset);
};

let toAgdaOffset = (editor, offset) => {
  let range =
    VSCode.Range.make(
      VSCode.Position.make(0, 0), // start
      editor->TextEditor.document->TextDocument.positionAt(offset) // end
    );
  let text = editor->TextEditor.document->TextDocument.getText(Some(range));
  characterWidth(text);
};

let getTextInRange = (editor, range) =>
  editor->TextEditor.document->TextDocument.getText(Some(range));
let getText = editor =>
  editor->TextEditor.document->TextDocument.getText(None);
let selectText = (editor, range) => {
  let start = VSCode.Range.start(range);
  let end_ = VSCode.Range.end_(range);
  let selection = Selection.make(start, end_);
  editor->TextEditor.setSelection(selection);
};
let replaceText = (editor, range, text) => {
  let workspaceEdit = WorkspaceEdit.make();
  workspaceEdit->WorkspaceEdit.replace(
    editor->TextEditor.document->TextDocument.uri,
    range,
    text,
    None,
  );
  Workspace.applyEdit(workspaceEdit);
};
let insertText = (editor, point, text) => {
  let workspaceEdit = WorkspaceEdit.make();
  workspaceEdit->WorkspaceEdit.insert(
    editor->TextEditor.document->TextDocument.uri,
    point,
    text,
    None,
  );
  Workspace.applyEdit(workspaceEdit);
};
let insertTexts = (editor, points, text) => {
  let workspaceEdit = WorkspaceEdit.make();
  let textEdits = points->Array.map(point => TextEdit.insert(point, text));
  workspaceEdit->WorkspaceEdit.set(
    editor->TextEditor.document->TextDocument.uri,
    textEdits,
  );
  Workspace.applyEdit(workspaceEdit);
};
let deleteText = (editor, range) => {
  let workspaceEdit = WorkspaceEdit.make();
  workspaceEdit->WorkspaceEdit.delete(
    editor->TextEditor.document->TextDocument.uri,
    range,
    None,
  );
  Workspace.applyEdit(workspaceEdit);
};

type changeEvent = {
  offset: int,
  insertText: string,
  replaceLength: int,
};
// TextDocumentContentChangeEvent.t;

let onChange = callback => {
  Workspace.onDidChangeTextDocument(
    fun
    | None => ()
    | Some(event) => {
        event
        ->TextDocumentChangeEvent.contentChanges
        ->Array.map(change =>
            {
              offset: change->TextDocumentContentChangeEvent.rangeOffset,
              insertText: change->TextDocumentContentChangeEvent.text,
              replaceLength:
                change->TextDocumentContentChangeEvent.rangeLength,
            }
          )
        ->callback;
      },
  );
};

let copyToClipboard = text => Env.clipboard->Clipboard.writeText(text);

let colorThemeIsDark = () =>
  Window.activeColorTheme->ColorTheme.kind == ColorThemeKind.Dark;

let lineEndingIsCRLF = editor =>
  switch (editor->TextEditor.document->TextDocument.eol) {
  | EndOfLine.CRLF => true
  | _ => false
  };
