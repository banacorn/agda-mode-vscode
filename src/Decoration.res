open Common
module type Module = {
  type t

  module SemanticToken: {
    type t = {
      range: VSCode.Range.t,
      type_: Highlighting.Aspect.TokenType.t,
      modifiers: option<array<AgdaModeVscode.Highlighting.Aspect.TokenModifier.t>>,
    }
    let toString: t => string
  }

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
  let addViaJSONFile: (t, string) => unit
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

  // accumulated TextDocumentChangeEvents
  module SemanticHighlighting: {
    let update: (t, VSCode.TextDocumentChangeEvent.t) => unit
    let get: t => array<SemanticToken.t>
    let reset: (t, VSCode.TextEditor.t) => array<SemanticToken.t>
  }
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

  module Format = {
    type t =
      | Emacs(string)
      | JSON(string)
    let toFilepath = format =>
      switch format {
      | Emacs(filepath) => filepath
      | JSON(filepath) => filepath
      }
  }

  module SemanticToken = {
    type t = {
      range: VSCode.Range.t,
      type_: Highlighting.Aspect.TokenType.t,
      modifiers: option<array<AgdaModeVscode.Highlighting.Aspect.TokenModifier.t>>,
    }

    let toString = token => {
      let range = token.range
      let tokenType = token.type_->Highlighting.Aspect.TokenType.toString
      let modifiers =
        token.modifiers->Option.mapWithDefault([], xs =>
          xs->Array.map(AgdaModeVscode.Highlighting.Aspect.TokenModifier.toString)
        )->Util.Pretty.array

      "(" ++
      string_of_int(VSCode.Position.line(VSCode.Range.start(range)))
      ++ ", " ++ string_of_int(VSCode.Position.character(VSCode.Range.start(range)))
      ++ ") (" ++ 
      string_of_int(VSCode.Position.line(VSCode.Range.end_(range)))
      ++ ", " ++ string_of_int(VSCode.Position.character(VSCode.Range.end_(range)))
      ++ ") " ++ tokenType ++ " " ++ modifiers
    }
  }

  type t = {
    // from AddViaFile
    mutable tempFilePaths: array<Format.t>,
    // from AddViaPipe
    mutable highlightings: array<Highlighting.t>,
    // after Apply
    mutable decorations: array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    // source locations
    mutable srcLocs: array<srcLoc>,
    // accumulated TextDocumentChangeEvents
    mutable semanticTokens: array<SemanticToken.t>,
  }

  let make = () => {
    tempFilePaths: [],
    highlightings: [],
    decorations: [],
    srcLocs: [],
    semanticTokens: [],
  }

  let clear = self => {
    self.decorations->Array.forEach(((decoration, _)) => Editor.Decoration.destroy(decoration))
    self.decorations = []
  }

  let destroy = self => {
    self.tempFilePaths->Array.forEach(format => {
      N.Fs.unlink(Format.toFilepath(format), _ => ())
    })
    self.tempFilePaths = []
    self.highlightings = []
    clear(self)
  }

  let redecorate = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )

  let addViaPipe = (self, highlightings) =>
    self.highlightings = Array.concat(self.highlightings, highlightings)

  let addViaFile = (self, filepath) =>
    Js.Array.push(Format.Emacs(filepath), self.tempFilePaths)->ignore
  let addViaJSONFile = (self, filepath) =>
    Js.Array.push(Format.JSON(filepath), self.tempFilePaths)->ignore

  let readFile = N.Util.promisify(N.Fs.readFile)

  let readAndParseEmacs = (filepath): Promise.t<Highlighting.Infos.t> =>
    readFile(. filepath)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(x =>
      switch x {
      | Ok(content) =>
        open! Parser.SExpression
        let tokens = switch Parser.SExpression.parse(Node.Buffer.toString(content))[0] {
        | Some(Ok(L(xs))) => xs
        | _ => []
        }
        // keepHighlighting
        let keepHighlighting = switch tokens[0] {
        | Some(A("remove")) => false
        | _ => true
        }
        let infos = Js.Array.sliceFrom(1, tokens)->Array.keepMap(Highlighting.parse)
        Highlighting.Infos.Infos(keepHighlighting, infos)
      // TODO: we should do something about these parse errors
      | Error(_err) => Highlighting.Infos.Infos(true, [])
      }
    )

  let readAndParseJSON = (filepath): Promise.t<Highlighting.Infos.t> =>
    readFile(. filepath)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(x =>
      switch x {
      | Ok(buffer) =>
        let raw = Node.Buffer.toString(buffer)
        switch Js.Json.parseExn(raw) {
        | exception _e => Highlighting.Infos.Infos(true, [])
        | json => Highlighting.Infos.decode(json)
        }
      | Error(_err) => Highlighting.Infos.Infos(true, [])
      }
    )

  let readAndParse = format =>
    switch format {
    | Format.Emacs(filepath) => readAndParseEmacs(filepath)
    | JSON(filepath) => readAndParseJSON(filepath)
    }

  // .tempFilePaths ====> .highlightings
  let readTempFiles = self =>
    self.tempFilePaths
    ->Array.map(readAndParse)
    ->Promise.allArray
    ->Promise.map(xs => xs->Array.map(Highlighting.Infos.toInfos)->Array.concatMany)
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
      if Config.Highlighting.getSemanticHighlighting() {
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

  module SemanticHighlighting = {
    let update = (self, event) => {
      let changes = VSCode.TextDocumentChangeEvent.contentChanges(event)

      let applyChange = (tokens, change: VSCode.TextDocumentContentChangeEvent.t) => {
        Js.log(tokens->Array.map(SemanticToken.toString))
        Js.log(change)
        ()
      }
      changes->Array.forEach(applyChange(self.semanticTokens))
    }
    let get = self => self.semanticTokens

    let reset = (self: t, editor: VSCode.TextEditor.t) => {
      let document = VSCode.TextEditor.document(editor)
      let text = Editor.Text.getAll(document)

      let offsetConverter = Agda.OffsetConverter.make(text)

      let intervalTree = IntervalTree.make()
      // Js.log2("Number of highlightings: ", Array.length(self.highlightings))

      self.highlightings->Array.forEach(highlighting => {
        // calculate the range of each highlighting
        let start = Agda.OffsetConverter.convert(offsetConverter, highlighting.start)
        let end_ = Agda.OffsetConverter.convert(offsetConverter, highlighting.end_)
        let range = Editor.Range.fromInterval(document, (start, end_))
        // insert [start, end_) to the interval tree
        let alreadyExists = intervalTree->IntervalTree.intersectAny((start, end_ - 1))
        if !alreadyExists {
          intervalTree
          ->IntervalTree.insert((start, end_ - 1), (range, highlighting.aspects))
          ->ignore
        }
      })

      let tokens =
        intervalTree
        ->IntervalTree.items
        ->Array.map(item => {
          let (range, aspects) = item["value"]
          // split the range in case that it spans multiple lines
          let ranges = lines(VSCode.TextEditor.document(editor), range)
          ranges->Array.map(range => (range, aspects))
        })
        ->Array.concatMany
        ->Array.keepMap(((range, aspects)) => {
          let tokenTypeAccum = []
          let tokenModifiersAccum = []
          // convert Aspects to TokenType and TokenModifiers
          aspects
          ->Array.keepMap(Highlighting.Aspect.toTokenTypeAndModifiers)
          ->Array.forEach(((tokenType, tokenModifiers)) => {
            Js.Array2.push(tokenTypeAccum, tokenType)->ignore
            Js.Array2.pushMany(tokenModifiersAccum, tokenModifiers)->ignore
          })
          tokenTypeAccum[0]->Option.map(type_ => {
            SemanticToken.range: range,
            type_: type_,
            modifiers: Some(tokenModifiersAccum),
          })
        })

      // update cached semantic tokens
      self.semanticTokens = tokens

      tokens
    }
  }
}

include Module
