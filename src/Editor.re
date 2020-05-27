open Guacamole.VSCode;

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

module Range = {
  type t = Range.t;
  let make = Range.make;
  let start = Range.start;
  let end_ = Range.end_;

  let contains = Range.contains;
  let containsRange = Range.containsRange;
};

type fileName = string;

open! Belt;

let editorType = Sig.VsCode;

let getExtensionPath = context => context->ExtensionContext.extensionPath;

let getFileName = editor =>
  Some(editor->TextEditor.document->TextDocument.fileName);

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
  let previous = ref(Window.activeTextEditor->Option.flatMap(getFileName));

  Window.onDidChangeActiveTextEditor(next => {
    let next = next->Option.flatMap(getFileName);
    if (next != previous^) {
      callback(previous^, next);
      previous := next;
    };
  });
};

// if end with '.agda' or '.lagda'
let isAgda = (filepath): bool => {
  let filepath = filepath->Parser.filepath;
  Js.Re.test_([%re "/\\.agda$|\\.lagda$/i"], filepath);
};

let registerCommand = (name, callback) =>
  Commands.registerCommand("extension." ++ name, () => {
    Window.activeTextEditor->Option.forEach(editor => {
      editor
      ->getFileName
      ->Option.forEach(fileName =>
          if (isAgda(fileName)) {
            callback(editor);
          }
        )
    })
  });

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
};

//
// View
//

module View = {
  include View__Controller;
  // override View.make to inject editor-dependent arguments
  let make = View__Controller.make(getExtensionPath);
};
//
// Decoration
//

module Decoration = {
  type style = string;
  type t = TextEditorDecorationType.t;
  // | Error => ThemeColor.make("inputValidation.errorBackground")
  // | Highlight => ThemeColor.make("editor.symbolHighlightBackground")
  // | Spec => ThemeColor.make("editor.wordHighlightStrongBackground")

  let highlightBackground =
      (editor: editor, style: string, range: Guacamole.VSCode.Range.t) => {
    let backgroundColor = ThemeColor.themeColor(ThemeColor.make(style));
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options =
      DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ());
    let handle = Window.createTextEditorDecorationType(options);
    editor->TextEditor.setDecorations(handle, [|range|]);
    [|handle|];
  };

  // ThemeColor.themeColor(
  //   switch (kind) {
  //   | Error => ThemeColor.make("errorForeground")
  //   | Highlight => ThemeColor.make("descriptionForeground")
  //   | Spec => ThemeColor.make("descriptionForeground")
  //   },
  // ),
  let overlayText =
      (
        editor: editor,
        style: string,
        text: string,
        range: Guacamole.VSCode.Range.t,
      ) => {
    let after =
      ThemableDecorationAttachmentRenderOptions.t(
        ~contentText=text,
        ~color=ThemeColor.themeColor(ThemeColor.make(style)),
        (),
      );

    let options = DecorationRenderOptions.t(~after, ());
    let handle = Window.createTextEditorDecorationType(options);
    editor->TextEditor.setDecorations(handle, [|range|]);
    [|handle|];
  };

  let destroy = TextEditorDecorationType.dispose;
};

let getCursorPosition = editor => editor->TextEditor.selection->Selection.end_;
let setCursorPosition = (editor, point) =>
  editor->TextEditor.setSelection(Selection.make(point, point));

let rangeForLine = (editor, line) =>
  editor->TextEditor.document->TextDocument.lineAt(line)->TextLine.range;
let pointAtOffset = (editor, offset) =>
  editor->TextEditor.document->TextDocument.positionAt(offset);

let getTextInRange = (editor, range) =>
  editor->TextEditor.document->TextDocument.getText(Some(range));
let getText = editor =>
  editor->TextEditor.document->TextDocument.getText(None);
let selectText = (editor, range) => {
  let start = Guacamole.VSCode.Range.start(range);
  let end_ = Guacamole.VSCode.Range.end_(range);
  let selection = Selection.make(start, end_);
  editor->TextEditor.setSelection(selection);
};
let setText = (editor, range, text) => {
  let editCallback = edit => {
    edit->TextEditorEdit.replaceAtRange(range, text);
  };
  editor->TextEditor.edit(editCallback, None);
};
let insertText = (editor, point, text) => {
  let editCallback = edit => {
    edit->TextEditorEdit.insert(point, text);
  };
  editor->TextEditor.edit(editCallback, None);
};
let deleteText = (editor, range) => {
  let editCallback = edit => {
    edit->TextEditorEdit.delete(range);
  };
  editor->TextEditor.edit(editCallback, None);
};