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

  let computeUTF16SurrogatePairIndices = (text: string): array(int) => {
    let surrogatePairs = [||];

    // length in code units (16 bits), not the actual UTF-8 length
    let lengthInCodeUnits = String.length(text);

    // iterate through the text to find surrogate pairs
    let i = ref(0);
    while (i^ < lengthInCodeUnits) {
      let charCode = Js.String.charCodeAt(i^, text)->int_of_float;
      let notFinal = i^ + 1 < lengthInCodeUnits;
      // check if this is a part of a surrogate pair
      if (charCode >= 0xD800 && charCode <= 0xDBFF && notFinal) {
        // found the high surrogate, proceed to check the low surrogate
        let nextCharCode = Js.String.charCodeAt(i^ + 1, text)->int_of_float;
        if (nextCharCode >= 0xDC00 && charCode <= 0xDFFF) {
          // store the index of this surrogate pair
          Js.Array.push(i^, surrogatePairs)
          ->ignore;
        };
        // increment by 2 because we have checked the presumably low surrogate char
        i := i^ + 2;
      } else {
        i := i^ + 1;
      };
    };

    surrogatePairs;
  };

  let decorateHighlightings =
      (editor: Editor.editor, highlightings: array(Highlighting.t)) => {
    let text = Editor.getText(editor);

    //  Suppose that, there are surrogate pairs at [6000, 6001] and [6003, 6004]
    //
    //        UTF-8       UTF-16
    //        --------------------
    //        5999        5999
    //        6000        6000
    //                    6001
    //        6001        6002
    //        6002        6003
    //                    6004
    //        6003        6005
    //
    //  When converting from a UTF-8 based index, say, `6003`,
    //  we need to know how many surrogate pairs are there before `6003`
    //
    //  Here's what we have computed:
    //    * UTF-16 based indices of surrogate pairs: [6000, 6003]
    //
    //  Here's what we are going to compute next:
    //    * UTF-8 based indices of surrogate pairs: [6000, 6002]
    //    * intervals of UTF-8 based indices [(0, 6000), (6001, 6002), (6003, ...)]

    // [6000, 6003]
    let indicesUTF16 = computeUTF16SurrogatePairIndices(text);
    // [6000, 6002]
    let indicesUTF8 = indicesUTF16->Array.mapWithIndex((i, x) => x - i);

    // [(0, 6000), (6001, 6002), (6003, ...)]
    let intervalsUTF8 = {
      // [(0, 6000), (6001, 6002)]
      let intervals =
        indicesUTF8->Array.mapWithIndex((i, rightEndpoint) => {
          let leftEndpoint =
            switch (indicesUTF8[i - 1]) {
            | Some(x) => x + 1
            // first interval
            | None => 0
            };
          (leftEndpoint, rightEndpoint);
        });

      // append the final interval
      let lastEndpoint = String.length(text) - Array.length(indicesUTF16);
      switch (intervals[Array.length(intervals) - 1]) {
      | None => [|(0, lastEndpoint)|]
      // otherwise
      | Some((_left, right)) =>
        Array.concat(intervals, [|(right + 1, lastEndpoint)|])
      };
    };

    let cursor = ref(0);
    let rec fromUTF8Offset = index => {
      switch (intervalsUTF8[cursor^]) {
      | None => index // shouldn't happen
      | Some((left, right)) =>
        if (index < left) {
          // reset the cursor to the beginning of the intervals
          cursor := 0;
          fromUTF8Offset(index);
        } else if (index > right) {
          // move the cursor a tad right
          cursor := cursor^ + 1;
          fromUTF8Offset(index);
        } else {
          index + cursor^;
        }
      };
    };

    //
    //
    //

    highlightings
    ->Array.map(highlighting => {
        let start = fromUTF8Offset(highlighting.start);
        let end_ = fromUTF8Offset(highlighting.end_);
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
