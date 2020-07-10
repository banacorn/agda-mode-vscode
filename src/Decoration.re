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
      ) => {
    let style = Highlighting.Aspect.toStyle(aspect);

    let decorate =
      fun
      | Highlighting.Background(color) =>
        Editor.Decoration.highlightBackgroundWithColor(editor, color, range)
      | Foreground(color) =>
        Editor.Decoration.decorateTextWithColor(editor, color, range);

    switch (style) {
    | Noop => [||]
    | Themed(light, dark) =>
      if (Editor.colorThemeIsDark()) {
        decorate(dark);
      } else {
        decorate(light);
      }
    };
  };

  let decorateHighlighting =
      (editor: Editor.editor, highlighting: Highlighting.t) => {
    let start = Editor.pointAtOffset(editor, highlighting.start);
    let end_ = Editor.pointAtOffset(editor, highlighting.end_);

    // Issue #3: https://github.com/banacorn/agda-mode-vscode/issues/3
    // Agda ignores `CRLF`s (line endings on Windows) and treat them like `LF`s
    // We need to count how many `CR`s are skipped and add them back to the offsets
    let normalize = point => {
      let useCRLF = Editor.lineEndingIsCRLF(editor);
      if (useCRLF) {
        let skippedCRLF = Editor.Point.line(point);
        Editor.Point.translate(point, 0, skippedCRLF);
      } else {
        point;
      };
    };

    let range = Editor.Range.make(normalize(start), normalize(end_));

    highlighting.aspects
    ->Array.map(decorateAspect(editor, range))
    ->Array.concatMany;
  };
};
