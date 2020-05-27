open Belt;
module Impl = (Editor: Sig.Editor) => {
  let decorateHole =
      (editor: Editor.editor, range: Editor.Range.t, index: int) => {
    let background =
      Editor.Decoration.highlightBackground(
        editor,
        "editor.selectionHighlightBackground",
        range,
      );
    let indexText = string_of_int(index);
    let start = Editor.Range.start(range);
    let end_ = Editor.Range.end_(range);
    let indexRange =
      Editor.Range.make(
        start,
        Editor.pointAtOffset(editor, Editor.offsetAtPoint(editor, end_) - 2),
      );

    let index =
      Editor.Decoration.overlayText(
        editor,
        "editorLightBulb.foreground",
        indexText,
        indexRange,
      );

    Array.concatMany([|background, index|]);
  };
};