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

  type srcLoc = {
    // range of the reference
    range: Editor.Range.t,
    // file/offset of the source
    filepath: Highlighting.filepath,
    offset: int,
  };

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  let decorateHole =
      (editor: Editor.editor, (start, end_): (int, int), index: int) => {
    let document = Editor.getDocument(editor);
    let backgroundRange =
      Editor.Range.make(
        Editor.pointAtOffset(document, start),
        Editor.pointAtOffset(document, end_),
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
        Editor.pointAtOffset(document, start),
        Editor.pointAtOffset(document, end_ - 2),
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
  let normalize = (document, point) => {
    let useCRLF = Editor.lineEndingIsCRLF(document);
    if (useCRLF) {
      let skippedCRLF = Editor.Point.line(point);
      Editor.Point.translate(point, 0, skippedCRLF);
    } else {
      point;
    };
  };

  // with compiled intervals for speeding the convertion
  let offsetToPoint = (document, intervals, offset) => {
    // UTF8 -> UTF16
    let offset = Editor.fromUTF8Offset(intervals, offset);
    // offset -> point
    let point = Editor.pointAtOffset(document, offset);
    // unnormalized -> normalized
    normalize(document, point);
  };

  // one off slow conversion for srclocs
  let offsetToPointSlow = (document, offset) => {
    let text = Editor.getText(document);
    let intervals = Editor.OffsetIntervals.compile(text);
    // UTF8 -> UTF16
    let offset = Editor.fromUTF8Offset(intervals, offset);
    // offset -> point
    let point = Editor.pointAtOffset(document, offset);
    // unnormalized -> normalized
    normalize(document, point);
  };

  let decorateHighlightings =
      (editor: Editor.editor, highlightings: array(Highlighting.t))
      : (
          array((Editor.Decoration.t, array(Editor.Range.t))),
          array(srcLoc),
        ) => {
    Js.Console.timeStart("$$$ Decoration / aspects");
    Js.Console.timeStart("$$$ Decoration / aspects / offset conversion");

    let document = Editor.getDocument(editor);
    let text = Editor.getText(document);

    // for fast UTF8 => UTF16 index conversion
    let intervals = Editor.OffsetIntervals.compile(text);

    // convert offsets in Highlighting.t to Ranges
    let highlightings:
      array(
        (
          Editor.Range.t,
          array(Highlighting.Aspect.t),
          option((Highlighting.filepath, int)),
        ),
      ) =
      highlightings->Array.map(highlighting => {
        // calculate the range of each highlighting
        let range = {
          let start = offsetToPoint(document, intervals, highlighting.start);
          let end_ = offsetToPoint(document, intervals, highlighting.end_);

          Editor.Range.make(start, end_);
        };
        (range, highlighting.aspects, highlighting.source);
      });
    // array of Aspect & Range
    let aspects: array((Highlighting.Aspect.t, Editor.Range.t)) =
      highlightings
      ->Array.map(((range, aspects, _)) => {
          // pair the aspect with the range
          aspects->Array.map(aspect => (aspect, range))
        })
      ->Array.concatMany;

    Js.Console.timeEnd("$$$ Decoration / aspects / offset conversion");
    Js.Console.timeStart("$$$ Decoration / aspects / scrlocs conversion");

    // array of Range & source location
    let srcLocs: array(srcLoc) =
      highlightings->Array.keepMap(((range, _, source)) =>
        source->Option.map(((filepath, offset)) =>
          {range, filepath, offset}
        )
      );
    Js.Console.timeEnd("$$$ Decoration / aspects / scrlocs conversion");
    Js.Console.timeStart("$$$ Decoration / aspects / dict bundling");
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
    Js.Console.timeEnd("$$$ Decoration / aspects / dict bundling");
    Js.Console.timeEnd("$$$ Decoration / aspects");
    Js.Console.timeStart("$$$ Decoration / dicts");

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
    Js.Console.timeEnd("$$$ Decoration / dicts");
    Js.Console.timeStart("$$$ Decoration / apply");
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
      ->Array.map(((color, ranges)) => {
          (
            // Js.log(ranges);
            Editor.Decoration.decorateTextWithColor(editor, color, ranges),
            ranges,
          )
        });
    Js.Console.timeEnd("$$$ Decoration / apply");
    // return decorations
    (Js.Array.concat(backgroundDecorations, foregroundDecorations), srcLocs);
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
    // source locations
    mutable srcLocs: array(srcLoc),
  };

  let make = () => {
    tempFilePaths: [||],
    highlightings: [||],
    decorations: [||],
    srcLocs: [||],
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
    let (decorations, srcLocs) =
      decorateHighlightings(editor, self.highlightings);

    self.highlightings = [||];
    self.srcLocs = srcLocs;
    self.decorations = Array.concat(self.decorations, decorations);
  };

  let lookupSrcLoc =
      (self, point)
      : option(Promise.t(array((Highlighting.filepath, Editor.Point.t)))) => {
    Js.Array.find(
      (srcLoc: srcLoc) => Editor.Range.contains(srcLoc.range, point),
      self.srcLocs,
    )
    ->Option.map(srcLoc => {
        Editor.openDocument(srcLoc.filepath)
        ->Promise.map(document => {
            let point = offsetToPointSlow(document, srcLoc.offset - 1);
            [|(srcLoc.filepath, point)|];
          })
      });
  };
};
