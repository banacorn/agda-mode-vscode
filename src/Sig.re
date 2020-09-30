type editorType =
  | Atom
  | VsCode;

let characterWidth: string => int = [%raw
  "function (string) {return [...string].length}"
];

module type Editor = {
  module Disposable: {
    type t;
    let make: (unit => unit) => t;
    let dispose: t => unit;
  };

  type editor;
  type context;
  type view;
  type fileName = string;

  type ordering =
    | GT
    | EQ
    | LT;

  module Point: {
    type t;
    let line: t => int;
    let column: t => int;
    let make: (int, int) => t;
    let translate: (t, int, int) => t;
    let compare: (t, t) => ordering;
  };

  let pointAtOffset: (editor, int) => Point.t;
  let offsetAtPoint: (editor, Point.t) => int;

  module Range: {
    type t;
    let make: (Point.t, Point.t) => t;
    let start: t => Point.t;
    let end_: t => Point.t;

    let contains: (t, Point.t) => bool;
    let containsRange: (t, t) => bool;

    let fromOffset: (editor, (int, int)) => t;
  };

  let editorType: editorType;

  // Helpers
  let getExtensionPath: context => fileName;
  let getFileName: editor => option(fileName);
  let openEditor: fileName => Promise.t(editor);
  let openEditorWithContent: string => Promise.t(editor);
  let save: editor => Promise.t(bool);

  // Events
  let onDidChangeFileName:
    ((option(fileName), option(fileName)) => unit) => Disposable.t;
  let onDidChangeActivation:
    ((option(editor), option(editor)) => unit) => Disposable.t;
  let onDidCloseEditor: (fileName => unit) => Disposable.t;
  let registerCommand: (string, (editor, fileName) => 'a) => Disposable.t;
  let setContext: (string, bool) => Promise.t(unit);

  // Subscriptions
  let getDisposables: context => array(Disposable.t);

  module Config: {
    // Agda path
    let getAgdaPath: unit => option(fileName);
    let setAgdaPath: fileName => Promise.t(unit);
    // Library path
    let getLibraryPath: unit => array(fileName);
    // Highlighting method
    let getHighlightingMethod: unit => bool;
    // Backend
    let getBackend: unit => string;
  };

  module View: {
    // construction/destruction
    let make: (fileName, editor) => view;
    let destroy: view => unit;
    // show/hide
    let show: view => unit;
    let hide: view => unit;
    let focus: view => unit;
    // messaging
    let send:
      (view, View.RequestOrEventToView.t) =>
      Promise.t(option(View.Response.t));
    let onEvent: (view, View.EventFromView.t => unit) => Disposable.t;
    // converting between types
    let fromPosition: View.Position.t => Point.t;
    let fromInterval: View.Interval.t => Range.t;
  };

  module Decoration: {
    type t;
    type backgroundStyle = string;
    type foregroundStyle = string;
    type color = string;

    let decorate: (editor, t, array(Range.t)) => unit;

    let highlightBackground: (editor, backgroundStyle, array(Range.t)) => t;
    let highlightBackgroundWithColor: (editor, color, array(Range.t)) => t;
    let decorateText: (editor, foregroundStyle, array(Range.t)) => t;
    let decorateTextWithColor: (editor, color, array(Range.t)) => t;
    // for hole indices
    let overlayText: (editor, foregroundStyle, string, Range.t) => t;
    let overlayTextWithColor: (editor, color, string, Range.t) => t;
    let underlineText: (editor, Range.t) => t;
    let destroy: t => unit;
  };

  let focus: editor => unit;
  let reveal: (editor, Range.t) => unit;

  let getSelection: editor => Range.t;
  let getSelections: editor => array(Range.t);
  let setSelection: (editor, Range.t) => unit;
  let setSelections: (editor, array(Range.t)) => unit;

  let getCursorPosition: editor => Point.t;
  let getCursorPositions: editor => array(Point.t);
  let setCursorPosition: (editor, Point.t) => unit;
  let setCursorPositions: (editor, array(Point.t)) => unit;
  let onChangeCursorPosition: (array(Point.t) => unit) => Disposable.t;

  let rangeForLine: (editor, int) => Range.t;

  // for converting between UTF-8/UTF-16 based offsets
  module OffsetIntervals: {
    type t = {
      intervals: array((int, int)),
      mutable cursor: int,
    };
    let compile: string => t;
  };

  let fromUTF8Offset: (OffsetIntervals.t, int) => int;
  let toUTF8Offset: (editor, int) => int;

  let getText: editor => string;
  let getTextInRange: (editor, Range.t) => string;
  let selectText: (editor, Range.t) => unit;
  let replaceText: (editor, Range.t, string) => Promise.t(bool);
  let replaceTextBatch:
    (editor, array((Range.t, string))) => Promise.t(bool);
  let insertText: (editor, Point.t, string) => Promise.t(bool);
  let insertTexts: (editor, array(Point.t), string) => Promise.t(bool);
  let deleteText: (editor, Range.t) => Promise.t(bool);

  type changeEvent = {
    offset: int,
    insertedText: string,
    replacedTextLength: int,
  };
  let onChange: (array(changeEvent) => unit) => Disposable.t;

  let copyToClipboard: string => Promise.t(unit);
  let colorThemeIsDark: unit => bool;
  let lineEndingIsCRLF: editor => bool;

  // for VSCode https://code.visualstudio.com/api/language-extensions/programmatic-language-features
  let registerProvider:
    ((fileName, Point.t) => option((fileName, Point.t))) => Disposable.t;
};
