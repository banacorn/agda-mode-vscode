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

let openEditor = fileName =>
  Window.showTextDocumentWithUri(Uri.file(fileName), None);

let openEditorWithContent = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );

let save = editor => editor->TextEditor.document->TextDocument.save;

let getDisposables = context => context->ExtensionContext.subscriptions;

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
        ranges: array(VSCode.Range.t),
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options =
      DecorationRenderOptions.t(~backgroundColor, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, ranges);
    decoration;
  };
  let highlightBackground =
      (editor: editor, style: backgroundStyle, ranges: array(VSCode.Range.t)) =>
    highlightBackgroundPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      ranges,
    );

  let highlightBackgroundWithColor =
      (editor: editor, color: color, ranges: array(VSCode.Range.t)) =>
    highlightBackgroundPrim(editor, ThemeColor.string(color), ranges);

  let decorateTextPrim =
      (
        editor: editor,
        color: ThemeColor.stringOrThemeColor,
        ranges: array(VSCode.Range.t),
      ) => {
    let rangeBehavior =
      DecorationRangeBehavior.toEnum(DecorationRangeBehavior.ClosedClosed);
    let options = DecorationRenderOptions.t(~color, ~rangeBehavior, ());
    let decoration = Window.createTextEditorDecorationType(options);
    editor->decorate(decoration, ranges);
    decoration;
  };
  let decorateText =
      (editor: editor, style: backgroundStyle, ranges: array(VSCode.Range.t)) =>
    decorateTextPrim(
      editor,
      ThemeColor.themeColor(ThemeColor.make(style)),
      ranges,
    );

  let decorateTextWithColor =
      (editor: editor, color: color, ranges: array(VSCode.Range.t)) =>
    decorateTextPrim(editor, ThemeColor.string(color), ranges);

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

let reveal = (editor, range) => {
  editor->TextEditor.revealRange(
    range,
    Some(TextEditorRevealType.InCenterIfOutsideViewport),
  );
};

let getSelection = editor => {
  let selection = editor->TextEditor.selection;
  VSCode.Range.make(Selection.start(selection), Selection.end_(selection));
};

let getSelections = editor => {
  editor
  ->TextEditor.selections
  ->Array.map(selection => {
      VSCode.Range.make(
        Selection.start(selection),
        Selection.end_(selection),
      )
    });
};

let setSelection = (editor, range) => {
  let selection =
    Selection.make(VSCode.Range.start(range), VSCode.Range.end_(range));
  editor->TextEditor.setSelection(selection);
};

let setSelections = (editor, ranges) => {
  let selections =
    ranges->Array.map(range =>
      Selection.make(VSCode.Range.start(range), VSCode.Range.end_(range))
    );
  editor->TextEditor.setSelections(selections);
};

let getCursorPosition = editor => editor->TextEditor.selection->Selection.end_;
let getCursorPositions = editor =>
  editor->TextEditor.selections->Array.map(Selection.end_);
let setCursorPosition = (editor, point) =>
  editor->TextEditor.setSelection(Selection.make(point, point));
let setCursorPositions = (editor, points) =>
  editor->TextEditor.setSelections(
    points->Array.map(point => Selection.make(point, point)),
  );
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

module OffsetIntervals = {
  //    Problem:  Symbols like "𝕁" should be treated like a single character as in UTF-8,
  //              however, it's treated like 2 characters in UTF-16 (which is what VS Code uses)
  type t = {
    intervals: array((int, int)),
    mutable cursor: int,
  };
  let computeUTF16SurrogatePairIndices = (text: string): array(int) => {
    let surrogatePairs = [||];

    // length in code units (16 bits), not the actual UTF-8 length
    let lengthInCodeUnits = String.length(text);

    // iterate through the text to find surrogate pairs
    let i = ref(0);
    while (i^ < lengthInCodeUnits) {
      let charCode = Js.String.charCodeAt(i^, text)->int_of_float;
      let notFinal = i^ + 1 < lengthInCodeUnits;
      // check if this is a part of a surrogate pair
      if (charCode >= 0xD800 && charCode <= 0xDBFF && notFinal) {
        // found the high surrogate, proceed to check the low surrogate
        let nextCharCode = Js.String.charCodeAt(i^ + 1, text)->int_of_float;
        if (nextCharCode >= 0xDC00 && charCode <= 0xDFFF) {
          // store the index of this surrogate pair
          Js.Array.push(i^, surrogatePairs)
          ->ignore;
        };
        // increment by 2 because we have checked the presumably low surrogate char
        i := i^ + 2;
      } else {
        i := i^ + 1;
      };
    };

    surrogatePairs;
  };

  // compile an array of UTF-8 based offset intervals
  // for faster UTF-8 => UTF-16 convertion
  let compile = (text: string): t => {
    //  Suppose that, there are surrogate pairs at [6000, 6001] and [6003, 6004]
    //
    //        UTF-8       UTF-16
    //        --------------------
    //        5999        5999
    //        6000        6000
    //                    6001
    //        6001        6002
    //        6002        6003
    //                    6004
    //        6003        6005
    //
    //  When converting from a UTF-8 based index, say, `6003`,
    //  we need to know how many surrogate pairs are there before `6003`
    //
    //  Here's what we have computed:
    //    * UTF-16 based indices of surrogate pairs: [6000, 6003]
    //
    //  Here's what we are going to compute next:
    //    * UTF-8 based indices of surrogate pairs: [6000, 6002]
    //    * intervals of UTF-8 based indices [(0, 6000), (6001, 6002), (6003, ...)]
    // [6000, 6003]
    let indicesUTF16 = computeUTF16SurrogatePairIndices(text);

    // [6000, 6002]
    let indicesUTF8 = indicesUTF16->Array.mapWithIndex((i, x) => x - i);

    // [(0, 6000), (6001, 6002)]
    let intervals =
      indicesUTF8->Array.mapWithIndex((i, rightEndpoint) => {
        let leftEndpoint =
          switch (indicesUTF8[i - 1]) {
          | Some(x) => x + 1
          // first interval
          | None => 0
          };
        (leftEndpoint, rightEndpoint);
      });

    // append the final interval
    let lastEndpoint = String.length(text) - Array.length(indicesUTF16);

    // [(0, 6000), (6001, 6002), (6003, ...)]
    let intervals =
      switch (intervals[Array.length(intervals) - 1]) {
      | None => [|(0, lastEndpoint)|]
      // otherwise
      | Some((_left, right)) =>
        Array.concat(intervals, [|(right + 1, lastEndpoint)|])
      };

    {intervals, cursor: 0};
  };
};

let rec fromUTF8Offset = (self, index) => {
  switch (self.OffsetIntervals.intervals[self.cursor]) {
  | None => index // shouldn't happen
  | Some((left, right)) =>
    if (index < left) {
      // reset the cursor to the beginning of the intervals
      self.cursor = 0;
      fromUTF8Offset(self, index);
    } else if (index > right) {
      // move the cursor a tad right
      self.cursor = self.cursor + 1;
      fromUTF8Offset(self, index);
    } else {
      index + self.cursor;
    }
  };
};

let toUTF8Offset = (editor, offset) => {
  let range =
    VSCode.Range.make(
      VSCode.Position.make(0, 0), // start
      editor->TextEditor.document->TextDocument.positionAt(offset) // end
    );
  let text = editor->TextEditor.document->TextDocument.getText(Some(range));
  Sig.characterWidth(text);
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
  editor->TextEditor.edit(
    editBuilder => {editBuilder->TextEditorEdit.replaceAtRange(range, text)},
    None,
  );
};
let replaceTextBatch = (editor, replacements) => {
  editor->TextEditor.edit(
    editBuilder => {
      replacements->Array.forEach(((range, text)) =>
        editBuilder->TextEditorEdit.replaceAtRange(range, text)
      )
    },
    None,
  );
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
  insertedText: string,
  replacedTextLength: int,
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
              insertedText: change->TextDocumentContentChangeEvent.text,
              replacedTextLength:
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

let registerProvider = defnProvider => {
  let documentSelector =
    MaybeArray.singular(DocumentFilterOrString.string("agda"));
  let definitionProvider =
    DefinitionProvider.makeWithLocations((textDocument, point, _) => {
      defnProvider(textDocument->TextDocument.fileName, point)
      ->Option.map(((fileName, position)) =>
          Promise.resolved([|
            Location.makeWithPosition(Uri.file(fileName), position),
          |])
        )
    });

  Languages.registerDefinitionProvider(documentSelector, definitionProvider);
};
