open Belt;
module Impl = (Editor: Sig.Editor) => {
  let decorateHole =
      (editor: Editor.editor, (start, end_): (int, int), index: int) => {
    let backgroundRange =
      Editor.Range.make(
        Editor.pointAtOffset(editor, start),
        Editor.pointAtOffset(editor, end_),
      );
    let background =
      Editor.Decoration.highlightBackground(editor, Hole, backgroundRange);
    let indexText = string_of_int(index);
    let indexRange =
      Editor.Range.make(
        Editor.pointAtOffset(editor, start),
        Editor.pointAtOffset(editor, end_ - 2),
      );

    let index =
      Editor.Decoration.overlayText(editor, HoleIndex, indexText, indexRange);

    Array.concatMany([|background, index|]);
  };
};