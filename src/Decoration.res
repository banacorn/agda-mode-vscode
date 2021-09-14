open Common
open Belt

// Tokens for Semantic Highlighting 
module type SemanticToken = {
  // a Range that does not span multiple lines
  module SingleLineRange: {
    type t = {
      line: int,
      column: (int, int),
    }
    let toString: t => string
    let toVsCodeRange: t => VSCode.Range.t
    let splitRange: (VSCode.TextDocument.t, VSCode.Range.t) => array<t>
  }

  type t = {
    range: SingleLineRange.t,
    type_: Highlighting.Agda.Aspect.TokenType.t,
    modifiers: option<array<Highlighting.Agda.Aspect.TokenModifier.t>>,
  }
  let toString: t => string
}

module SemanticToken = {
  // a Range that does not span multiple lines
  module SingleLineRange = {
    type t = {
      line: int,
      column: (int, int),
    }

    let toString = ({line, column}) =>
      string_of_int(line) ++ ":" ++ string_of_int(fst(column)) ++ "-" ++ string_of_int(snd(column))

    let toVsCodeRange = ({line, column}) =>
      VSCode.Range.make(
        VSCode.Position.make(line, fst(column)),
        VSCode.Position.make(line, snd(column)),
      )

    // split a single range into multiple ranges that only occupies single lines
    let splitRange = (doc: VSCode.TextDocument.t, range: VSCode.Range.t): array<t> => {
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
        Js.Array.push(
          {
            line: VSCode.Position.line(startingPoint),
            column: (
              VSCode.Position.character(startingPoint),
              VSCode.Position.character(endingPoint),
            ),
          },
          ranges,
        )->ignore
      }
      ranges
    }
  }

  type t = {
    range: SingleLineRange.t,
    type_: Highlighting.Agda.Aspect.TokenType.t,
    modifiers: option<array<Highlighting.Agda.Aspect.TokenModifier.t>>,
  }

  let toString = token => {
    // let range = token.range
    let tokenType = token.type_->Highlighting.Agda.Aspect.TokenType.toString
    let modifiers =
      token.modifiers
      ->Option.mapWithDefault([], xs =>
        xs->Array.map(Highlighting.Agda.Aspect.TokenModifier.toString)
      )
      ->Util.Pretty.array

    "(" ++ SingleLineRange.toString(token.range) ++ ") " ++ tokenType ++ " " ++ modifiers
  }
}

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
  let addViaPipe: (t, array<Highlighting.Agda.Info.t>) => unit
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
  ) => option<Promise.t<array<(VSCode.Range.t, Highlighting.Agda.Info.filepath, VSCode.Position.t)>>>

  module SemanticHighlighting: {
    let update: (t, VSCode.TextDocumentChangeEvent.t) => unit
    let get: t => Promise.t<ref<array<SemanticToken.t>>>
    let apply: (t, VSCode.TextEditor.t) => array<SemanticToken.t>
  }
}

module Module: Module = {

  type srcLoc = {
    // range of the reference
    range: VSCode.Range.t,
    // file/offset of the source
    filepath: Highlighting.Agda.Info.filepath,
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

  let decorateHighlightings = (
    editor: VSCode.TextEditor.t,
    infos: IntervalTree.t<Highlighting.Agda.Info.t>,
  ): (array<(Editor.Decoration.t, array<VSCode.Range.t>)>, array<srcLoc>) => {
    // Js.Console.timeStart("$$$ Decoration / aspects")
    // Js.Console.timeStart("$$$ Decoration / aspects / offset conversion")

    let document = VSCode.TextEditor.document(editor)
    let text = Editor.Text.getAll(document)

    let offsetConverter = Agda.OffsetConverter.make(text)

    // convert offsets in Info.t to Ranges
    let infos: array<(
      VSCode.Range.t,
      array<Highlighting.Agda.Aspect.t>,
      option<(Highlighting.Agda.Info.filepath, int)>,
    )> = IntervalTree.items(infos)->Array.map(item => {
      let highlighting = item["value"]
      // calculate the range of each highlighting
      let start = Agda.OffsetConverter.convert(offsetConverter, highlighting.start)
      let end_ = Agda.OffsetConverter.convert(offsetConverter, highlighting.end_)
      let range = Editor.Range.fromInterval(document, (start, end_))
      (range, highlighting.aspects, highlighting.source)
    })
    // array of Aspect & Range
    let aspects: array<(Highlighting.Agda.Aspect.t, VSCode.Range.t)> =
      infos
      ->Array.map(((range, aspects, _)) =>
        // pair the aspect with the range
        aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.concatMany

    // Js.Console.timeEnd("$$$ Decoration / aspects / offset conversion")
    // Js.Console.timeStart("$$$ Decoration / aspects / scrlocs conversion")

    // array of Range & source location
    let srcLocs: array<srcLoc> = infos->Array.keepMap(((range, _, source)) =>
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

    let addFaceToDict = (face: Highlighting.Agda.Aspect.face, range) =>
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
      let style = Highlighting.Agda.Aspect.toStyle(aspect)
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

  type t = {
    // from AddViaFile
    mutable tempFilePaths: array<Format.t>,
    // from AddViaPipe
    mutable infos: IntervalTree.t<Highlighting.Agda.Info.t>,
    // after Apply
    mutable decorations: array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    // source locations
    mutable srcLocs: array<srcLoc>,
    // Semantic Tokens
    mutable semanticTokens: Promise.t<ref<array<SemanticToken.t>>>,
    mutable resolveSemanticTokens: ref<array<SemanticToken.t>> => unit,
  }

  let make = () => {
    let (promise, resolve) = Promise.pending()
    {
      tempFilePaths: [],
      infos: IntervalTree.make(),
      decorations: [],
      srcLocs: [],
      semanticTokens: promise,
      resolveSemanticTokens: resolve,
    }
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
    self.infos = IntervalTree.make()
    clear(self)
  }

  let redecorate = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )

  // insert to an IntervalTree
  let addViaPipe = (self, infos: array<Highlighting.Agda.Info.t>) => {
    infos->Array.forEach(info => {
      let alreadyExists =
        self.infos->IntervalTree.intersectAny((info.start, info.end_ - 1))
      if !alreadyExists {
        self.infos
        ->IntervalTree.insert((info.start, info.end_ - 1), info)
        ->ignore
      }
    })
  }

  let addViaFile = (self, filepath) =>
    Js.Array.push(Format.Emacs(filepath), self.tempFilePaths)->ignore
  let addViaJSONFile = (self, filepath) =>
    Js.Array.push(Format.JSON(filepath), self.tempFilePaths)->ignore

  let readFile = N.Util.promisify(N.Fs.readFile)

  let readAndParseEmacs = (filepath): Promise.t<Highlighting.Agda.Infos.t> =>
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
        let infos = Js.Array.sliceFrom(1, tokens)->Array.keepMap(Highlighting.Agda.Info.parse)
        Highlighting.Agda.Infos.Infos(keepHighlighting, infos)
      // TODO: we should do something about these parse errors
      | Error(_err) => Highlighting.Agda.Infos.Infos(true, [])
      }
    )

  let readAndParseJSON = (filepath): Promise.t<Highlighting.Agda.Infos.t> =>
    readFile(. filepath)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(x =>
      switch x {
      | Ok(buffer) =>
        let raw = Node.Buffer.toString(buffer)
        switch Js.Json.parseExn(raw) {
        | exception _e => Highlighting.Agda.Infos.Infos(true, [])
        | json => Highlighting.Agda.Infos.decode(json)
        }
      | Error(_err) => Highlighting.Agda.Infos.Infos(true, [])
      }
    )

  let readAndParse = format =>
    switch format {
    | Format.Emacs(filepath) => readAndParseEmacs(filepath)
    | JSON(filepath) => readAndParseJSON(filepath)
    }

  // .tempFilePaths ====> .infos
  let readTempFiles = self =>
    self.tempFilePaths
    ->Array.map(readAndParse)
    ->Promise.allArray
    ->Promise.map(xs => xs->Array.map(Highlighting.Agda.Infos.toInfos)->Array.concatMany)
    ->Promise.map(infos => {
      addViaPipe(self, infos)
      self.tempFilePaths = []
    })

  // .infos ====> .decorations
  let applyHighlightings = (self, editor) => {
    let (decorations, srcLocs) = decorateHighlightings(editor, self.infos)

    self.infos = IntervalTree.make()
    self.srcLocs = srcLocs
    self.decorations = Array.concat(self.decorations, decorations)
  }

  let lookupSrcLoc = (self, point): option<
    Promise.t<array<(VSCode.Range.t, Highlighting.Agda.Info.filepath, VSCode.Position.t)>>,
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
    module Change = {
      type action =
        // tokens BEFORE where the change happens
        | NoOp
        // tokens WITHIN where the change removes or destroys
        | Remove
        // tokens AFTER where the change happens, but on the same line
        | Move(int, int) // delta of LINE, delta of COLUMN
        // tokens AFTER where the change happens, but not on the same line
        | MoveLinesOnly(int) // delta of LINE

      // what should we do to this token?
      let classify = (change, token: SemanticToken.t) => {
        // tokens WITHIN this range should be removed
        let removedRange = change->VSCode.TextDocumentContentChangeEvent.range

        let (lineDelta, columnDelta) = {
          // +1 line for each linebreak ('\n', '\r', and '\r\n')
          // -1 line for each line in `removedRange`
          // +1 column for each charactor after the last linebreak
          // -1 column for each charactor in `removedRange`

          let regex = %re("/\\r\\n|\\r|\\n/")
          let lines = Js.String.splitByRe(regex, change->VSCode.TextDocumentContentChangeEvent.text)

          let lineDetalOfRemovedRange =
            VSCode.Position.line(VSCode.Range.end_(removedRange)) -
            VSCode.Position.line(VSCode.Range.start(removedRange))
          let lineDelta = Array.length(lines) - 1 - lineDetalOfRemovedRange

          if lineDelta > 0 {
            // to the next line
            (lineDelta, -VSCode.Position.character(VSCode.Range.end_(removedRange)))
          } else if lineDelta < 0 {
            // to the previous line
            let columnDelta =
              VSCode.Position.character(VSCode.Range.end_(removedRange)) -
              VSCode.Position.character(VSCode.Range.start(removedRange))
            (lineDelta, -columnDelta)
          } else {
            // stays on the same line
            let columnDeltaOfRemovedRange =
              VSCode.Position.character(VSCode.Range.end_(removedRange)) -
              VSCode.Position.character(VSCode.Range.start(removedRange))

            let columnDelta = switch lines[lineDelta] {
            | Some(Some(line)) =>
              // number of characters after the last linebreak
              String.length(line) - columnDeltaOfRemovedRange
            | _ => 0
            }
            (0, columnDelta)
          }
        }

        let tokenRange = token.range->SemanticToken.SingleLineRange.toVsCodeRange

        if (
          VSCode.Position.isBeforeOrEqual(
            VSCode.Range.end_(tokenRange),
            VSCode.Range.start(removedRange),
          )
        ) {
          NoOp
        } else if (
          VSCode.Range.containsRange(removedRange, tokenRange) ||
          (VSCode.Position.isBefore(
            VSCode.Range.start(tokenRange),
            VSCode.Range.start(removedRange),
          ) &&
          VSCode.Position.isAfter(VSCode.Range.end_(tokenRange), VSCode.Range.end_(removedRange)))
        ) {
          Remove
        } else if token.range.line == VSCode.Position.line(VSCode.Range.end_(removedRange)) {
          Move(lineDelta, columnDelta)
        } else {
          MoveLinesOnly(lineDelta)
        }
      }

      let apply = (token: SemanticToken.t, action) =>
        switch action {
        | NoOp => [token]
        | Remove => []
        | Move(lineDelta, columnDelta) => [
            {
              ...token,
              range: {
                line: token.range.line + lineDelta,
                column: (
                  fst(token.range.column) + columnDelta,
                  snd(token.range.column) + columnDelta,
                ),
              },
            },
          ]
        | MoveLinesOnly(lineDelta) => [
            {
              ...token,
              range: {
                line: token.range.line + lineDelta,
                column: token.range.column,
              },
            },
          ]
        }
    }

    let update = (self, event) => {
      let changes = VSCode.TextDocumentChangeEvent.contentChanges(event)

      let applyChange = (
        tokens: array<SemanticToken.t>,
        change: VSCode.TextDocumentContentChangeEvent.t,
      ) =>
        tokens
        ->Array.map(token => {
          let action = Change.classify(change, token)
          Change.apply(token, action)
        })
        ->Array.concatMany

      // read from the cached
      self.semanticTokens->Promise.get(semanticTokensRef => {
        changes->Array.forEach(change => {
          semanticTokensRef := applyChange(semanticTokensRef.contents, change)
        })
        // update the cache
        self.resolveSemanticTokens(semanticTokensRef)
      })
    }

    let get = (self: t) => self.semanticTokens

    let apply = (self: t, editor: VSCode.TextEditor.t) => {
      let document = VSCode.TextEditor.document(editor)
      let text = Editor.Text.getAll(document)

      let offsetConverter = Agda.OffsetConverter.make(text)

      let tokens =
        self.infos
        ->IntervalTree.items
        ->Array.map(item => {
          let highlighting = item["value"]

          // calculate the range of each highlighting
          let start = Agda.OffsetConverter.convert(offsetConverter, highlighting.start)
          let end_ = Agda.OffsetConverter.convert(offsetConverter, highlighting.end_)
          let range = Editor.Range.fromInterval(document, (start, end_))

          // split the range in case that it spans multiple lines
          let ranges = SemanticToken.SingleLineRange.splitRange(
            VSCode.TextEditor.document(editor),
            range,
          )
          ranges->Array.map(range => (range, highlighting.aspects))
        })
        ->Array.concatMany
        ->Array.keepMap(((range, aspects)) => {
          let tokenTypeAccum = []
          let tokenModifiersAccum = []
          // convert Aspects to TokenType and TokenModifiers
          aspects
          ->Array.keepMap(Highlighting.Agda.Aspect.toTokenTypeAndModifiers)
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

      if Array.length(tokens) == 0 {
        let (promise, resolve) = Promise.pending()
        self.semanticTokens = promise
        self.resolveSemanticTokens = resolve
      } else {
        // trigger and resolve
        self.resolveSemanticTokens(ref(tokens))
        // update cached semantic tokens
        self.semanticTokens->Promise.get(semanticTokensRef => {
          semanticTokensRef := tokens
        })
      }

      tokens
    }
  }

  let apply = (self, editor) =>
    readTempFiles(self)->Promise.map(() => {
      // only apply decorations when Semantic Highlighting is off
      if Config.Highlighting.getSemanticHighlighting() {
        SemanticHighlighting.apply(self, editor)->ignore
        self.infos = IntervalTree.make()
      } else {
        applyHighlightings(self, editor)
      }
    })
}

include Module
