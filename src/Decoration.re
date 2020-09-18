open Belt;
module Impl = (Editor: Sig.Editor) => {
  type action =
    | AddDirectly(array(Highlighting.t))
    | AddIndirectly(string)
    | Clear
    | Apply
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
        [|backgroundRange|],
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

  // Issue #3: https://github.com/banacorn/agda-mode-vscode/issues/3
  // Agda ignores `CRLF`s (line endings on Windows) and treat them like `LF`s
  // We need to count how many `CR`s are skipped and add them back to the offsets
  let normalize = (editor, point) => {
    let useCRLF = Editor.lineEndingIsCRLF(editor);
    if (useCRLF) {
      let skippedCRLF = Editor.Point.line(point);
      Editor.Point.translate(point, 0, skippedCRLF);
    } else {
      point;
    };
  };
  let decorateHighlightings =
      (editor: Editor.editor, highlightings: array(Highlighting.t))
      : array((Editor.Decoration.t, array(Editor.Range.t))) => {
    let text = Editor.getText(editor);

    // for fast UTF8 => UTF16 index conversion
    let intervals = Editor.OffsetIntervals.compile(text);

    // array of pairs of Aspect & Range
    let aspects: array((Highlighting.Aspect.t, Editor.Range.t)) =
      highlightings
      ->Array.map(highlighting => {
          // calculate the range of each highlighting
          let range = {
            let start = Editor.fromUTF8Offset(intervals, highlighting.start);
            let end_ = Editor.fromUTF8Offset(intervals, highlighting.end_);
            let start = Editor.pointAtOffset(editor, start);
            let end_ = Editor.pointAtOffset(editor, end_);
            Editor.Range.make(
              normalize(editor, start),
              normalize(editor, end_),
            );
          };
          // pair the aspect with the range
          highlighting.aspects->Array.map(aspect => (aspect, range));
        })
      ->Array.concatMany;

    // dictionaries of color-ranges mapping
    // speed things up by aggregating decorations of the same kind
    let backgroundColorDict: Js.Dict.t(array(Editor.Range.t)) =
      Js.Dict.empty();
    let foregroundColorDict: Js.Dict.t(array(Editor.Range.t)) =
      Js.Dict.empty();

    let addFaceToDict = (face: Highlighting.face, range) => {
      switch (face) {
      | Background(color) =>
        switch (Js.Dict.get(backgroundColorDict, color)) {
        | None => Js.Dict.set(backgroundColorDict, color, [|range|])
        | Some(ranges) => Js.Array.push(range, ranges)->ignore
        }
      | Foreground(color) =>
        switch (Js.Dict.get(foregroundColorDict, color)) {
        | None => Js.Dict.set(foregroundColorDict, color, [|range|])
        | Some(ranges) => Js.Array.push(range, ranges)->ignore
        }
      };
    };

    // convert Aspects to colors and collect them in the dict
    aspects->Array.forEach(((aspect, range)) => {
      let style = Highlighting.Aspect.toStyle(aspect);
      switch (style) {
      | Noop => ()
      | Themed(light, dark) =>
        if (Editor.colorThemeIsDark()) {
          addFaceToDict(dark, range);
        } else {
          addFaceToDict(light, range);
        }
      };
    });

    // decorate with colors stored in the dicts
    let backgroundDecorations =
      Js.Dict.entries(backgroundColorDict)
      ->Array.map(((color, ranges)) =>
          (
            Editor.Decoration.highlightBackgroundWithColor(
              editor,
              color,
              ranges,
            ),
            ranges,
          )
        );
    let foregroundDecorations =
      Js.Dict.entries(foregroundColorDict)
      ->Array.map(((color, ranges)) =>
          (
            Editor.Decoration.decorateTextWithColor(editor, color, ranges),
            ranges,
          )
        );

    // return decorations
    Js.Array.concat(backgroundDecorations, foregroundDecorations);
  };

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  type t = {
    // from AddIndirectly
    mutable tempFilePaths: array(string),
    // from AddDirectly
    mutable highlightings: array(Highlighting.t),
    // after Apply
    mutable decorations:
      array((Editor.Decoration.t, array(Editor.Range.t))),
  };

  let make = () => {
    highlightings: [||],
    tempFilePaths: [||],
    decorations: [||],
  };

  let removeAppliedDecorations = self => {
    self.decorations
    ->Array.forEach(((decoration, _)) =>
        Editor.Decoration.destroy(decoration)
      );
    self.decorations = [||];
  };

  let destroy = self => {
    self.tempFilePaths
    ->Array.forEach(filepath => N.Fs.unlink(filepath, _ => ()));
    self.tempFilePaths = [||];
    self.highlightings = [||];
    self.decorations
    ->Array.forEach(((decoration, _)) =>
        Editor.Decoration.destroy(decoration)
      );
    self.decorations = [||];
  };

  let refresh = (editor, self) => {
    self.decorations
    ->Array.forEach(((decoration, ranges)) => {
        Editor.Decoration.decorate(editor, decoration, ranges)
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
