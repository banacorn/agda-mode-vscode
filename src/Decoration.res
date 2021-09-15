open Common
open Belt

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

  // add Agda highlighting infos
  let addViaPipe: (t, array<Highlighting.Agda.Info.t>) => unit
  let addViaFile: (t, string) => unit
  let addViaJSONFile: (t, string) => unit
  // apply Agda highlighting infos
  let applyAndClear: (t, VSCode.TextEditor.t) => Promise.t<unit>
  // remove Agda highlighting infos
  let clear: t => unit
  // redecorate everything after the TextEditor has been replaced
  let redecorate: (t, VSCode.TextEditor.t) => unit

  // LSP
  let lookupSrcLoc: (
    t,
    int,
  ) => option<
    Promise.t<array<(VSCode.Range.t, Highlighting.Agda.Info.filepath, VSCode.Position.t)>>,
  >

  module SemanticHighlighting: {
    let update: (t, VSCode.TextDocumentChangeEvent.t) => unit
    let get: t => Promise.t<ref<array<Highlighting.SemanticToken.t>>>
    let convert: (t, VSCode.TextEditor.t) => array<Highlighting.SemanticToken.t>
  }
}

module Module: Module = {
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

  // helper function for tagging IntervalTree.t<Info.t> with Ranges calculated from offsets
  let tagWithRange = (
    editor: VSCode.TextEditor.t,
    infos: IntervalTree.t<Highlighting.Agda.Info.t>,
  ): IntervalTree.t<(Highlighting.Agda.Info.t, VSCode.Range.t)> => {
    let document = VSCode.TextEditor.document(editor)
    let text = Editor.Text.getAll(document)
    // table for speeding up the offset-Range conversion
    let offsetConverter = Agda.OffsetConverter.make(text)

    infos->IntervalTree.map((info, _) => {
      // calculate the range of each info
      let start = Agda.OffsetConverter.convert(offsetConverter, info.start)
      let end_ = Agda.OffsetConverter.convert(offsetConverter, info.end_)
      let range = Editor.Range.fromInterval(document, (start, end_))
      (info, range)
    })
  }

  let toDecorations = (
    infosWithRanges: IntervalTree.t<(Highlighting.Agda.Info.t, VSCode.Range.t)>,
    editor: VSCode.TextEditor.t,
  ): array<(Editor.Decoration.t, array<VSCode.Range.t>)> => {
    let aspects: array<(Highlighting.Agda.Aspect.t, VSCode.Range.t)> =
      infosWithRanges
      ->IntervalTree.elems
      ->Array.map(((info, range)) =>
        // pair the aspect with the range
        info.aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.concatMany

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
    // return decorations
    Js.Array.concat(backgroundDecorations, foregroundDecorations)
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
    // from addViaFile/addViaJSONFile
    mutable tempFilePaths: array<Format.t>,
    // from addViaPipe
    mutable infos: IntervalTree.t<Highlighting.Agda.Info.t>,
    mutable infosWithRanges: IntervalTree.t<(Highlighting.Agda.Info.t, VSCode.Range.t)>,
    // Decorations
    mutable decorations: array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    // Semantic Tokens
    mutable semanticTokens: Promise.t<ref<array<Highlighting.SemanticToken.t>>>,
    mutable resolveSemanticTokens: ref<array<Highlighting.SemanticToken.t>> => unit,
  }

  let make = () => {
    let (promise, resolve) = Promise.pending()
    {
      tempFilePaths: [],
      infos: IntervalTree.make(),
      infosWithRanges: IntervalTree.make(),
      decorations: [],
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
      let alreadyExists = self.infos->IntervalTree.intersectAny((info.start, info.end_ - 1))
      if !alreadyExists {
        self.infos->IntervalTree.insert((info.start, info.end_ - 1), info)->ignore
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
    let decorations = toDecorations(self.infosWithRanges, editor)
    self.decorations = Array.concat(self.decorations, decorations)
  }

  let lookupSrcLoc = (self, offset): option<
    Promise.t<array<(VSCode.Range.t, Highlighting.Agda.Info.filepath, VSCode.Position.t)>>,
  > => {
    let matched = self.infosWithRanges->IntervalTree.search((offset - 1, offset))
    // returns the first matching srcloc
    matched[0]
    ->Option.flatMap(((info, range)) =>
      info.source->Option.map(((filepath, offset)) => (range, filepath, offset))
    )
    ->Option.map(((range, filepath, offset)) => {
      VSCode.Workspace.openTextDocumentWithFileName(filepath)->Promise.map(document => {
        let text = Editor.Text.getAll(document)
        let offsetConverter = Agda.OffsetConverter.make(text)
        let offset = Agda.OffsetConverter.convert(offsetConverter, offset - 1)
        let position = Editor.Position.fromOffset(document, offset)
        [(range, filepath, position)]
      })
    })
  }

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
      let classify = (change, token: Highlighting.SemanticToken.t) => {
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

        let tokenRange = token.range->Highlighting.SemanticToken.SingleLineRange.toVsCodeRange

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

      let apply = (token: Highlighting.SemanticToken.t, action) =>
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
        tokens: array<Highlighting.SemanticToken.t>,
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

    let convert = (self: t, editor: VSCode.TextEditor.t) => {
      let tokens =
        self.infosWithRanges
        ->IntervalTree.elems
        ->Array.map(((info: Highlighting.Agda.Info.t, range)) => {
          // split the range in case that it spans multiple lines
          let ranges = Highlighting.SemanticToken.SingleLineRange.splitRange(
            VSCode.TextEditor.document(editor),
            range,
          )
          ranges->Array.map(range => (info.aspects, range))
        })
        ->Array.concatMany
        ->Array.keepMap(((aspects, range)) => {
          let tokenTypeAccum = []
          let tokenModifiersAccum = []
          // convert Aspects to TokenType and TokenModifiers
          aspects
          ->Array.keepMap(Highlighting.SemanticToken.fromAspect)
          ->Array.forEach(((tokenType, tokenModifiers)) => {
            Js.Array2.push(tokenTypeAccum, tokenType)->ignore
            Js.Array2.pushMany(tokenModifiersAccum, tokenModifiers)->ignore
          })
          tokenTypeAccum[0]->Option.map(type_ => {
            Highlighting.SemanticToken.range: range,
            type_: type_,
            modifiers: Some(tokenModifiersAccum),
          })
        })

      tokens
    }
  }

  let applyAndClear = (self, editor) =>
    readTempFiles(self)->Promise.map(() => {
      self.infosWithRanges = tagWithRange(editor, self.infos)

      if Config.Highlighting.getSemanticHighlighting() {
        let tokens = SemanticHighlighting.convert(self, editor)
        // renew promise when there are no tokens
        if Array.length(tokens) == 0 {
          let (promise, resolve) = Promise.pending()
          self.semanticTokens = promise
          self.resolveSemanticTokens = resolve
        } else {
          // resolve to complete the request for semantic tokens
          self.resolveSemanticTokens(ref(tokens))
          // update cached semantic tokens
          self.semanticTokens->Promise.get(semanticTokensRef => {
            semanticTokensRef := tokens
          })
        }
      } else {
        applyHighlightings(self, editor)
      }

      // remove old infos
      self.infos = IntervalTree.make()
    })
}

include Module
