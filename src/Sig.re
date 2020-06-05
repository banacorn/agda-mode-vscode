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

  module Range: {
    type t;
    let make: (Point.t, Point.t) => t;
    let start: t => Point.t;
    let end_: t => Point.t;

    let contains: (t, Point.t) => bool;
    let containsRange: (t, t) => bool;
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
    ((option(fileName), option(fileName)) => unit) => Disposable.t;
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
  };

  module View: {
    // construction/destruction
    let make: (context, editor) => view;
    let destroy: view => unit;
    // show/hide
    let show: view => unit;
    let hide: view => unit;
    let focus: view => unit;
    // messaging
    let send: (view, View.Request.t) => Promise.t(View.Response.t);
    let on: (view, View.Event.t => unit) => Disposable.t;
  };

  module Decoration: {
    type t;
    type backgroundStyle =
      | Hole;
    type foregroundStyle =
      | HoleIndex;
    // let digHole: (editor, Range.t) => unit;
    let highlightBackground: (editor, backgroundStyle, Range.t) => array(t);
    let overlayText: (editor, foregroundStyle, string, Range.t) => array(t);
    let destroy: t => unit;
  };

  let focus: editor => unit;

  let getCursorPosition: editor => Point.t;
  let setCursorPosition: (editor, Point.t) => unit;

  let rangeForLine: (editor, int) => Range.t;
  let pointAtOffset: (editor, int) => Point.t;
  let offsetAtPoint: (editor, Point.t) => int;

  let getText: editor => string;
  let getTextInRange: (editor, Range.t) => string;
  let selectText: (editor, Range.t) => unit;
  let setText: (editor, Range.t, string) => Promise.t(bool);
  let insertText: (editor, Point.t, string) => Promise.t(bool);
  let deleteText: (editor, Range.t) => Promise.t(bool);
};