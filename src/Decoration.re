open Belt;
module Impl = (Editor: Sig.Editor) => {
  let decorateHole = (editor: Editor.editor, range: Editor.Range.t) => {
    let start = Editor.Range.start(range);
    let end_ = Editor.Range.end_(range);

    let boundaryLeftRange =
      Editor.Range.make(start, Editor.Point.translate(start, 0, 2));
    let boundaryRightRange =
      Editor.Range.make(Editor.Point.translate(end_, 0, -2), end_);

    Array.concatMany([|
      Editor.Decoration.highlightBackground(
        editor,
        "editor.wordHighlightStrongBackground",
        boundaryLeftRange,
      ),
      Editor.Decoration.highlightBackground(
        editor,
        "editor.wordHighlightStrongBackground",
        boundaryRightRange,
      ),
    |]);
  };
};