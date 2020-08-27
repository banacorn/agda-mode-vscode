open Belt;
module Impl = (Editor: Sig.Editor) => {
  type action =
    | AddDirectly(array(Highlighting.t))
    | AddIndirectly(string)
    | Apply
    | RemoveAll
    | Refresh;

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

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

    (background, index);
  };

  let decorateAspect =
      (
        editor: Editor.editor,
        range: Editor.Range.t,
        aspect: Highlighting.Aspect.t,
      )
      : array((Editor.Decoration.t, Editor.Range.t)) => {
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
        [|(decorate(dark), range)|];
      } else {
        [|(decorate(light), range)|];
      }
    };
  };

  let decorateHighlighting =
      (editor: Editor.editor, highlighting: Highlighting.t) => {
    // converts offsets from Agda to offsets for editor first

    let start = Editor.fromUTF8Offset(editor, None, highlighting.start);
    let end_ = Editor.fromUTF8Offset(editor, None, highlighting.end_);
    let start = Editor.pointAtOffset(editor, start);
    let end_ = Editor.pointAtOffset(editor, end_);

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

  let decorateHighlightings =
      (editor: Editor.editor, highlightings: array(Highlighting.t)) => {
    let initOffset = {Editor.utf8: 0, utf16: 0};
    highlightings
    ->Array.map(highlighting => {
        let start =
          Editor.fromUTF8Offset(
            editor,
            Some(initOffset),
            highlighting.start,
          );
        let end_ =
          Editor.fromUTF8Offset(editor, Some(initOffset), highlighting.end_);
        let start = Editor.pointAtOffset(editor, start);
        let end_ = Editor.pointAtOffset(editor, end_);

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
      })
    ->Array.concatMany;
  };

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  type t = {
    // from AddIndirectly
    mutable tempFilePaths: array(string),
    // from AddDirectly
    mutable highlightings: array(Highlighting.t),
    // after Apply
    mutable decorations: array((Editor.Decoration.t, Editor.Range.t)),
  };

  let make = () => {
    highlightings: [||],
    tempFilePaths: [||],
    decorations: [||],
  };

  let destroy = self => {
    self.highlightings = [||];
    self.tempFilePaths
    ->Array.forEach(filepath => N.Fs.unlink(filepath, _ => ()));
    self.tempFilePaths = [||];
    self.decorations
    ->Array.forEach(((decoration, _)) =>
        Editor.Decoration.destroy(decoration)
      );
  };

  let refresh = (editor, self) => {
    self.decorations
    ->Array.forEach(((decoration, range)) => {
        Editor.Decoration.decorate(editor, decoration, [|range|])
      });
  };

  let addDirectly = (self, highlightings) => {
    self.highlightings = Array.concat(self.highlightings, highlightings);
  };

  let addIndirectly = (self, filepath) => {
    Js.Array.push(filepath, self.tempFilePaths)->ignore;
  };

  let readFile = N.Util.promisify(N.Fs.readFile);

  let readAndParse = (filepath): Promise.t(array(Highlighting.t)) => {
    readFile(. filepath)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(
        fun
        | Ok(content) => {
            open! Parser.SExpression;
            let expressions =
              content->Node.Buffer.toString->Parser.SExpression.parse;
            // TODO: we should do something about these parse errors
            let _parseErrors: array((int, string)) =
              expressions->Array.keepMap(
                fun
                | Error(error) => Some(error)
                | Ok(_) => None,
              );
            expressions
            ->Array.keepMap(
                fun
                | Error(_) => None // filter errors out
                | Ok(L(xs)) =>
                  Some(Highlighting.parseIndirectHighlightings(xs))
                | Ok(_) => Some([||]),
              )
            ->Array.concatMany;
          }
        // TODO: we should do something about these parse errors
        | Error(_err) => [||],
      );
  };

  // .tempFilePaths ====> .highlightings
  let readTempFiles = self => {
    self.tempFilePaths
    ->Array.map(readAndParse)
    ->Promise.allArray
    ->Promise.map(Array.concatMany)
    ->Promise.map(highlightings => {
        self.highlightings = Array.concat(self.highlightings, highlightings);
        self.tempFilePaths = [||];
      });
  };

  // .highlightings ====> .decorations
  let applyHighlightings = (self, editor) => {
    let decorations = decorateHighlightings(editor, self.highlightings);
    // ->Array.map(decorateHighlighting(editor))
    // ->Array.concatMany;
    self.highlightings = [||];
    self.decorations = Array.concat(self.decorations, decorations);
  };
};
