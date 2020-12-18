module type Module = {
  type srcLoc = {
    // range of the reference
    range: VSCode.Range.t,
    // file/offset of the source
    filepath: Highlighting.filepath,
    offset: int,
  }

  type t

  let make: unit => t

  let decorateHole: (
    VSCode.TextEditor.t,
    (int, int),
    int,
  ) => (VSCode.TextEditorDecorationType.t, VSCode.TextEditorDecorationType.t)

  let addDirectly: (t, array<Highlighting.t>) => unit
  let addIndirectly: (t, string) => unit
  let removeAppliedDecorations: t => unit
  let readTempFiles: t => Promise.t<unit>
  let applyHighlightings: (t, VSCode.TextEditor.t) => unit
  let refresh: (t, VSCode.TextEditor.t) => unit
  let destroy: t => unit
  let lookupSrcLoc: (
    t,
    VSCode.Position.t,
  ) => option<Promise.t<array<(VSCode.Range.t, Highlighting.filepath, VSCode.Position.t)>>>
}

module Module: Module = {
  open Belt

  type srcLoc = {
    // range of the reference
    range: VSCode.Range.t,
    // file/offset of the source
    filepath: Highlighting.filepath,
    offset: int,
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  let decorateHole = (editor: VSCode.TextEditor.t, (start, end_): (int, int), index: int) => {
    let document = VSCode.TextEditor.document(editor)
    let backgroundRange = VSCode.Range.make(
      document->VSCode.TextDocument.positionAt(start),
      document->VSCode.TextDocument.positionAt(end_),
    )

    let background = Editor.Decoration.highlightBackground(
      editor,
      "editor.selectionHighlightBackground",
      [backgroundRange],
    )
    let indexText = string_of_int(index)
    let indexRange = VSCode.Range.make(
      document->VSCode.TextDocument.positionAt(start),
      document->VSCode.TextDocument.positionAt(end_ - 2),
    )

    let index = Editor.Decoration.overlayText(
      editor,
      "editorLightBulb.foreground",
      indexText,
      indexRange,
    )

    (background, index)
  }

  // Issue #3: https://github.com/banacorn/agda-mode-vscode/issues/3
  // Agda ignores `CRLF`s (line endings on Windows) and treat them like `LF`s
  // For example, in "ab\r\ncd", Agda would ignore the "\r" and think that "c" is the 4th character
  // we pass in compiled indices for speeding up the convertion
  let offsetToPoint = (document, utf16indices, eolIndices, offset) => {
    // UTF8 -> UTF16
    let offset = Editor.Indices.convert(utf16indices, offset)
    // LF -> CRLF
    let offset = Editor.Indices.convert(eolIndices, offset)
    // offset -> point
    document->VSCode.TextDocument.positionAt(offset)
  }

  // one off slow conversion for srclocs
  let offsetToPointSlow = (document, offset) => {
    let text = Editor.Text.getAll(document)
    let utf16indices = Editor.Indices.make(Editor.computeUTF16SurrogatePairIndices(text))
    // UTF8 -> UTF16
    let offset = Editor.Indices.convert(utf16indices, offset)
    // offset -> point
    document->VSCode.TextDocument.positionAt(offset)
  }

  let decorateHighlightings = (editor: VSCode.TextEditor.t, highlightings: array<Highlighting.t>): (
    array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    array<srcLoc>,
  ) => {
    Js.Console.timeStart("$$$ Decoration / aspects")
    Js.Console.timeStart("$$$ Decoration / aspects / offset conversion")

    let document = VSCode.TextEditor.document(editor)
    let text = Editor.Text.getAll(document)

    // for fast UTF8 => UTF16 offset conversion
    let utf16indices = Editor.Indices.make(Editor.computeUTF16SurrogatePairIndices(text))
    // for LF => CRLF => LF offset conversion
    let eolIndices = Editor.Indices.make(Editor.computeCRLFIndices(text))

    // convert offsets in Highlighting.t to Ranges
    let highlightings: array<(
      VSCode.Range.t,
      array<Highlighting.Aspect.t>,
      option<(Highlighting.filepath, int)>,
    )> =
      highlightings->Array.map(highlighting => {
        // calculate the range of each highlighting
        let range = {
          let start = offsetToPoint(document, utf16indices, eolIndices, highlighting.start)
          let end_ = offsetToPoint(document, utf16indices, eolIndices, highlighting.end_)

          VSCode.Range.make(start, end_)
        }
        (range, highlighting.aspects, highlighting.source)
      })
    // array of Aspect & Range
    let aspects: array<(Highlighting.Aspect.t, VSCode.Range.t)> = highlightings->Array.map(((
      range,
      aspects,
      _,
    )) =>
      // pair the aspect with the range
      aspects->Array.map(aspect => (aspect, range))
    )->Array.concatMany

    Js.Console.timeEnd("$$$ Decoration / aspects / offset conversion")
    Js.Console.timeStart("$$$ Decoration / aspects / scrlocs conversion")

    // array of Range & source location
    let srcLocs: array<srcLoc> =
      highlightings->Array.keepMap(((range, _, source)) => source->Option.map(((
          filepath,
          offset,
        )) => {
          range: range,
          filepath: filepath,
          offset: offset,
        }))
    Js.Console.timeEnd("$$$ Decoration / aspects / scrlocs conversion")
    Js.Console.timeStart("$$$ Decoration / aspects / dict bundling")
    // dictionaries of color-ranges mapping
    // speed things up by aggregating decorations of the same kind
    let backgroundColorDict: Js.Dict.t<array<VSCode.Range.t>> = Js.Dict.empty()
    let foregroundColorDict: Js.Dict.t<array<VSCode.Range.t>> = Js.Dict.empty()

    let addFaceToDict = (face: Highlighting.face, range) =>
      switch face {
      | Background(color) =>
        switch Js.Dict.get(backgroundColorDict, color) {
        | None => Js.Dict.set(backgroundColorDict, color, [range])
        | Some(ranges) => Js.Array.push(range, ranges)->ignore
        }
      | Foreground(color) =>
        switch Js.Dict.get(foregroundColorDict, color) {
        | None => Js.Dict.set(foregroundColorDict, color, [range])
        | Some(ranges) => Js.Array.push(range, ranges)->ignore
        }
      }
    Js.Console.timeEnd("$$$ Decoration / aspects / dict bundling")
    Js.Console.timeEnd("$$$ Decoration / aspects")
    Js.Console.timeStart("$$$ Decoration / dicts")

    // convert Aspects to colors and collect them in the dict
    aspects->Array.forEach(((aspect, range)) => {
      let style = Highlighting.Aspect.toStyle(aspect)
      switch style {
      | Noop => ()
      | Themed(light, dark) =>
        let theme = VSCode.Window.activeColorTheme->VSCode.ColorTheme.kind
        if theme == VSCode.ColorThemeKind.Dark {
          addFaceToDict(dark, range)
        } else {
          addFaceToDict(light, range)
        }
      }
    })
    Js.Console.timeEnd("$$$ Decoration / dicts")
    Js.Console.timeStart("$$$ Decoration / apply")
    // decorate with colors stored in the dicts
    let backgroundDecorations =
      Js.Dict.entries(backgroundColorDict)->Array.map(((color, ranges)) => (
        Editor.Decoration.highlightBackgroundWithColor(editor, color, ranges),
        ranges,
      ))
    let foregroundDecorations =
      Js.Dict.entries(foregroundColorDict)->Array.map(((color, ranges)) => (
        Editor.Decoration.decorateTextWithColor(editor, color, ranges),
        ranges,
      ))
    Js.Console.timeEnd("$$$ Decoration / apply")
    // return decorations
    (Js.Array.concat(backgroundDecorations, foregroundDecorations), srcLocs)
  }

  // split a single range into multiple ranges that only occupies single lines
  let lines = (doc: VSCode.TextDocument.t, range: VSCode.Range.t): array<VSCode.Range.t> => {
    open VSCode.Range
    open VSCode.Position
    let startingLine = line(start(range))
    let endingLine = line(end_(range))

    let ranges = []
    for i in startingLine to endingLine {
      let startingPoint = if i == startingLine {
        start(range)
      } else {
        VSCode.Position.make(i, 0)
      }
      let endingPoint = if i == endingLine {
        end_(range)
      } else {
        let offset = doc->VSCode.TextDocument.offsetAt(VSCode.Position.make(i + 1, 0)) - 1
        doc->VSCode.TextDocument.positionAt(offset)
      }
      Js.Array.push(VSCode.Range.make(startingPoint, endingPoint), ranges)->ignore
    }
    ranges
  }

  let _generateSemanticTokens = (
    editor: VSCode.TextEditor.t,
    highlightings: array<Highlighting.t>,
    push: (VSCode.Range.t, string, option<array<string>>) => unit,
  ): Promise.t<unit> => {
    Js.log("GENERATE")
    let document = VSCode.TextEditor.document(editor)
    let text = Editor.Text.getAll(document)

    // for fast UTF8 => UTF16 offset conversion
    let utf16indices = Editor.Indices.make(Editor.computeUTF16SurrogatePairIndices(text))
    // for LF => CRLF => LF offset conversion
    let eolIndices = Editor.Indices.make(Editor.computeCRLFIndices(text))

    // convert offsets in Highlighting.t to Ranges
    let highlightings: array<(
      VSCode.Range.t,
      array<Highlighting.Aspect.t>,
      option<(Highlighting.filepath, int)>,
    )> =
      highlightings->Array.map(highlighting => {
        // calculate the range of each highlighting
        let range = {
          let start = offsetToPoint(document, utf16indices, eolIndices, highlighting.start)
          let end_ = offsetToPoint(document, utf16indices, eolIndices, highlighting.end_)

          VSCode.Range.make(start, end_)
        }
        (range, highlighting.aspects, highlighting.source)
      })
    // array of Aspect & Range
    let aspects: array<(Highlighting.Aspect.t, VSCode.Range.t)> = highlightings->Array.map(((
      range,
      aspects,
      _,
    )) =>
      // pair the aspect with the range
      aspects->Array.map(aspect => (aspect, range))
    )->Array.concatMany

    // convert Aspects to colors and collect them in the dict
    aspects->Array.forEach(((aspect, range)) => {
      // split the range so in case that it spans multiple lines
      let ranges = lines(VSCode.TextEditor.document(editor), range)
      let (tokenType, tokenModifier) = Highlighting.Aspect.toTokenTypeAndModifiers(aspect)
      let tokenType = Highlighting.Aspect.TokenType.toString(tokenType)
      let tokenModifier =
        tokenModifier->Option.map(x => x->Array.map(Highlighting.Aspect.TokenModifier.toString))
      ranges->Array.forEach(range => push(range, tokenType, tokenModifier))
    })

    Promise.resolved()
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  type t = {
    // from AddViaFile
    mutable tempFilePaths: array<string>,
    // from AddViaPipe
    mutable highlightings: array<Highlighting.t>,
    // after Apply
    mutable decorations: array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    // source locations
    mutable srcLocs: array<srcLoc>,
  }

  let make = () => {
    tempFilePaths: [],
    highlightings: [],
    decorations: [],
    srcLocs: [],
  }

  let removeAppliedDecorations = self => {
    self.decorations->Array.forEach(((decoration, _)) => Editor.Decoration.destroy(decoration))
    self.decorations = []
  }

  let destroy = self => {
    self.tempFilePaths->Array.forEach(filepath => N.Fs.unlink(filepath, _ => ()))
    self.tempFilePaths = []
    self.highlightings = []
    self.decorations->Array.forEach(((decoration, _)) => Editor.Decoration.destroy(decoration))
    self.decorations = []
  }

  let refresh = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )

  let addDirectly = (self, highlightings) =>
    self.highlightings = Array.concat(self.highlightings, highlightings)

  let addIndirectly = (self, filepath) => Js.Array.push(filepath, self.tempFilePaths)->ignore

  let readFile = N.Util.promisify(N.Fs.readFile)

  let readAndParse = (filepath): Promise.t<array<Highlighting.t>> =>
    readFile(. filepath)->Promise.Js.fromBsPromise->Promise.Js.toResult->Promise.map(x =>
      switch x {
      | Ok(content) =>
        open! Parser.SExpression
        let expressions = content->Node.Buffer.toString->Parser.SExpression.parse
        // TODO: we should do something about these parse errors
        let _parseErrors: array<(int, string)> = expressions->Array.keepMap(x =>
          switch x {
          | Error(error) => Some(error)
          | Ok(_) => None
          }
        )
        expressions->Array.keepMap(x =>
          switch x {
          | Error(_) => None // filter errors out
          | Ok(L(xs)) => Some(Highlighting.parseIndirectHighlightings(xs))
          | Ok(_) => Some([])
          }
        )->Array.concatMany
      // TODO: we should do something about these parse errors
      | Error(_err) => []
      }
    )

  // .tempFilePaths ====> .highlightings
  let readTempFiles = self =>
    self.tempFilePaths
    ->Array.map(readAndParse)
    ->Promise.allArray
    ->Promise.map(Array.concatMany)
    ->Promise.map(highlightings => {
      self.highlightings = Array.concat(self.highlightings, highlightings)
      self.tempFilePaths = []
    })

  // .highlightings ====> .decorations
  let applyHighlightings = (self, editor) => {
    let (decorations, srcLocs) = decorateHighlightings(editor, self.highlightings)

    self.highlightings = []
    self.srcLocs = srcLocs
    self.decorations = Array.concat(self.decorations, decorations)
  }

  let lookupSrcLoc = (self, point): option<
    Promise.t<array<(VSCode.Range.t, Highlighting.filepath, VSCode.Position.t)>>,
  > =>
    Js.Array.find(
      (srcLoc: srcLoc) => VSCode.Range.contains(srcLoc.range, point),
      self.srcLocs,
    )->Option.map(srcLoc =>
      VSCode.Workspace.openTextDocumentWithFileName(srcLoc.filepath)->Promise.map(document => {
        let point = offsetToPointSlow(document, srcLoc.offset - 1)
        [(srcLoc.range, srcLoc.filepath, point)]
      })
    )
}

include Module
