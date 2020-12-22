open Common
module type Module = {
  type t

  let make: unit => t
  let destroy: t => unit

  // for decorating Goals
  let decorateHole: (
    VSCode.TextEditor.t,
    Interval.t,
    int,
  ) => (VSCode.TextEditorDecorationType.t, VSCode.TextEditorDecorationType.t)

  // adding decorations
  let addViaPipe: (t, array<Highlighting.t>) => unit
  let addViaFile: (t, string) => unit
  // applying decorations
  let apply: (t, VSCode.TextEditor.t) => Promise.t<unit>
  // removing decorations
  let clear: t => unit
  // redecorate everything after the TextEditor has been replaced
  let redecorate: (t, VSCode.TextEditor.t) => unit

  // LSP
  let lookupSrcLoc: (
    t,
    VSCode.Position.t,
  ) => option<Promise.t<array<(VSCode.Range.t, Highlighting.filepath, VSCode.Position.t)>>>

  let generateSemanticTokens: (
    t,
    VSCode.TextEditor.t,
    (
      VSCode.Range.t,
      Highlighting.Aspect.TokenType.t,
      option<array<AgdaModeVscode.Highlighting.Aspect.TokenModifier.t>>,
    ) => unit,
  ) => Promise.t<unit>
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

  let decorateHole = (editor: VSCode.TextEditor.t, interval: Interval.t, index: int) => {
    let document = VSCode.TextEditor.document(editor)
    let backgroundRange = Editor.Range.fromInterval(document, interval)

    let background = Editor.Decoration.highlightBackground(
      editor,
      "editor.selectionHighlightBackground",
      [backgroundRange],
    )
    let indexText = string_of_int(index)
    let innerInterval = (fst(interval), snd(interval) - 2)
    let indexRange = Editor.Range.fromInterval(document, innerInterval)

    let index = Editor.Decoration.overlayText(
      editor,
      "editorLightBulb.foreground",
      indexText,
      indexRange,
    )

    (background, index)
  }

  let decorateHighlightings = (editor: VSCode.TextEditor.t, highlightings: array<Highlighting.t>): (
    array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    array<srcLoc>,
  ) => {
    // Js.Console.timeStart("$$$ Decoration / aspects")
    // Js.Console.timeStart("$$$ Decoration / aspects / offset conversion")

    let document = VSCode.TextEditor.document(editor)
    let text = Editor.Text.getAll(document)

    let offsetConverter = Agda.OffsetConverter.make(text)

    // convert offsets in Highlighting.t to Ranges
    let highlightings: array<(
      VSCode.Range.t,
      array<Highlighting.Aspect.t>,
      option<(Highlighting.filepath, int)>,
    )> = highlightings->Array.map(highlighting => {
      // calculate the range of each highlighting
      let start = Agda.OffsetConverter.convert(offsetConverter, highlighting.start)
      let end_ = Agda.OffsetConverter.convert(offsetConverter, highlighting.end_)
      let range = Editor.Range.fromInterval(document, (start, end_))
      (range, highlighting.aspects, highlighting.source)
    })
    // array of Aspect & Range
    let aspects: array<(Highlighting.Aspect.t, VSCode.Range.t)> =
      highlightings
      ->Array.map(((range, aspects, _)) =>
        // pair the aspect with the range
        aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.concatMany

    // Js.Console.timeEnd("$$$ Decoration / aspects / offset conversion")
    // Js.Console.timeStart("$$$ Decoration / aspects / scrlocs conversion")

    // array of Range & source location
    let srcLocs: array<srcLoc> = highlightings->Array.keepMap(((range, _, source)) =>
      source->Option.map(((filepath, offset)) => {
        range: range,
        filepath: filepath,
        offset: offset,
      })
    )
    // Js.Console.timeEnd("$$$ Decoration / aspects / scrlocs conversion")
    // Js.Console.timeStart("$$$ Decoration / aspects / dict bundling")

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
    // Js.Console.timeEnd("$$$ Decoration / aspects / dict bundling")
    // Js.Console.timeEnd("$$$ Decoration / aspects")
    // Js.Console.timeStart("$$$ Decoration / dicts")

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
    // Js.Console.timeEnd("$$$ Decoration / dicts")
    // Js.Console.timeStart("$$$ Decoration / apply")
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
    // Js.Console.timeEnd("$$$ Decoration / apply")
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

  let clear = self => {
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

  let redecorate = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )

  let addViaPipe = (self, highlightings) =>
    self.highlightings = Array.concat(self.highlightings, highlightings)

  let addViaFile = (self, filepath) => Js.Array.push(filepath, self.tempFilePaths)->ignore

  let readFile = N.Util.promisify(N.Fs.readFile)

  let readAndParse = (filepath): Promise.t<array<Highlighting.t>> =>
    readFile(. filepath)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(x =>
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
        expressions
        ->Array.keepMap(x =>
          switch x {
          | Error(_) => None // filter errors out
          | Ok(L(xs)) => Some(Highlighting.parseIndirectHighlightings(xs))
          | Ok(_) => Some([])
          }
        )
        ->Array.concatMany
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

  let apply = (self, editor) =>
    readTempFiles(self)->Promise.map(() => {
      // only apply decorations when Semantic Highlighting is off
      if Config.getSemanticHighlighting() {
        ()
      } else {
        applyHighlightings(self, editor)
      }
    })

  let lookupSrcLoc = (self, point): option<
    Promise.t<array<(VSCode.Range.t, Highlighting.filepath, VSCode.Position.t)>>,
  > =>
    Js.Array.find(
      (srcLoc: srcLoc) => VSCode.Range.contains(srcLoc.range, point),
      self.srcLocs,
    )->Option.map(srcLoc =>
      VSCode.Workspace.openTextDocumentWithFileName(srcLoc.filepath)->Promise.map(document => {
        let text = Editor.Text.getAll(document)
        let offsetConverter = Agda.OffsetConverter.make(text)
        let offset = Agda.OffsetConverter.convert(offsetConverter, srcLoc.offset - 1)
        let position = Editor.Position.fromOffset(document, offset)
        [(srcLoc.range, srcLoc.filepath, position)]
      })
    )

  let generateSemanticTokens = (
    self: t,
    editor: VSCode.TextEditor.t,
    pushToken: (
      VSCode.Range.t,
      Highlighting.Aspect.TokenType.t,
      option<array<Highlighting.Aspect.TokenModifier.t>>,
    ) => unit,
  ): Promise.t<unit> => {
    let document = VSCode.TextEditor.document(editor)
    let text = Editor.Text.getAll(document)

    let offsetConverter = Agda.OffsetConverter.make(text)

    // convert offsets in Highlighting.t to Ranges
    let highlightings: array<(
      VSCode.Range.t,
      array<Highlighting.Aspect.t>,
      option<(Highlighting.filepath, int)>,
    )> = self.highlightings->Array.map(highlighting => {
      // calculate the range of each highlighting
      let start = Agda.OffsetConverter.convert(offsetConverter, highlighting.start)
      let end_ = Agda.OffsetConverter.convert(offsetConverter, highlighting.end_)
      let range = Editor.Range.fromInterval(document, (start, end_))
      (range, highlighting.aspects, highlighting.source)
    })
    // array of Aspect & Range
    let aspects: array<(Highlighting.Aspect.t, VSCode.Range.t)> =
      highlightings
      ->Array.map(((range, aspects, _)) =>
        // pair the aspect with the range
        aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.concatMany

    // convert Aspects to colors and collect them in the dict
    aspects->Array.forEach(((aspect, range)) => {
      // split the range so in case that it spans multiple lines
      let ranges = lines(VSCode.TextEditor.document(editor), range)
      let (tokenType, tokenModifiers) = Highlighting.Aspect.toTokenTypeAndModifiers(aspect)
      ranges->Array.forEach(range => pushToken(range, tokenType, tokenModifiers))
    })

    Promise.resolved()
  }
}

include Module
