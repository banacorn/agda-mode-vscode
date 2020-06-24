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
      Editor.Decoration.highlightBackground(
        editor,
        "editor.selectionHighlightBackground",
        backgroundRange,
      );
    let indexText = string_of_int(index);
    let indexRange =
      Editor.Range.make(
        Editor.pointAtOffset(editor, start),
        Editor.pointAtOffset(editor, end_ - 2),
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

  let decorateAspect =
      (
        editor: Editor.editor,
        range: Editor.Range.t,
        aspect: Highlighting.Aspect.t,
      ) => {};

  let decorateHighlighting =
      (editor: Editor.editor, highlighting: Highlighting.t) => {
    let backgroundRange =
      Editor.Range.make(
        Editor.pointAtOffset(editor, highlighting.start),
        Editor.pointAtOffset(editor, highlighting.end_),
      );
    // let handle =
    //   Editor.Decoration.highlightBackgroundWithColor(
    //     editor,
    //     Response.Aspect.color(highlighting.aspects),
    //     backgroundRange,
    //   );
    // let indexText = string_of_int(index);
    // let indexRange =
    //   Editor.Range.make(
    //     Editor.pointAtOffset(editor, start),
    //     Editor.pointAtOffset(editor, end_ - 2),
    //   );

    // let index =
    //   Editor.Decoration.overlayText(
    //     editor,
    //     "editorLightBulb.foreground",
    //     indexText,
    //     indexRange,
    //   );

    // Array.concatMany([|background, index|]);
    [||];
  };
};