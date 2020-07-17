type editorType =
  | Atom
  | VsCode;

module type Disposable = {
  type t;
  let make: (unit => unit) => t;
  let dispose: t => unit;
};

module type Editor = {
  type editor;
  type context;
  module Disposable: Disposable;
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
  let save: editor => Promise.t(bool);
  let isAgda: fileName => bool;

  // Events
  let onDidChangeFileName:
    ((option(fileName), option(fileName)) => unit) => Disposable.t;
  let onDidChangeActivation:
    ((option(editor), option(editor)) => unit) => Disposable.t;
  let onDidCloseEditor: (fileName => unit) => Disposable.t;
  let registerCommand: (string, editor => unit) => Disposable.t;
  let setContext: (string, bool) => Promise.t(unit);

  // Subscriptions
  let addToSubscriptions: (Disposable.t, context) => unit;

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
    let on: (view, View.ResponseOrEventFromView.t => unit) => Disposable.t;
  };

  module Decoration: {
    type t;
    type backgroundStyle = string;
    type foregroundStyle = string;
    type color = string;

    let decorate: (editor, t, array(Range.t)) => unit;

    let highlightBackground: (editor, backgroundStyle, Range.t) => t;
    let highlightBackgroundWithColor: (editor, color, Range.t) => t;
    let decorateText: (editor, foregroundStyle, Range.t) => t;
    let decorateTextWithColor: (editor, color, Range.t) => t;
    // for hole indices
    let overlayText: (editor, foregroundStyle, string, Range.t) => t;
    let overlayTextWithColor: (editor, color, string, Range.t) => t;
    let underlineText: (editor, Range.t) => t;
    let destroy: t => unit;
  };

  let focus: editor => unit;

  let getSelectionRange: editor => Range.t;
  let getSelectionRanges: editor => array(Range.t);

  let getCursorPosition: editor => Point.t;
  let getCursorPositions: editor => array(Point.t);
  let setCursorPosition: (editor, Point.t) => unit;
  let onChangeCursorPosition: (array(Point.t) => unit) => Disposable.t;

  let rangeForLine: (editor, int) => Range.t;
  let fromAgdaOffset: (editor, int) => int;
  let toAgdaOffset: (editor, int) => int;

  let getText: editor => string;
  let getTextInRange: (editor, Range.t) => string;
  let selectText: (editor, Range.t) => unit;
  let setText: (editor, Range.t, string) => Promise.t(bool);
  let insertText: (editor, Point.t, string) => Promise.t(bool);
  let deleteText: (editor, Range.t) => Promise.t(bool);

  type changeEvent = {
    offset: int,
    insertText: string,
    replaceLength: int,
  };
  let onChange: (array(changeEvent) => unit) => Disposable.t;

  let copyToClipboard: string => Promise.t(unit);
  let colorThemeIsDark: unit => bool;
  let lineEndingIsCRLF: editor => bool;
};
