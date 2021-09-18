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
  let addViaPipe: (t, VSCode.TextEditor.t, array<Highlighting.Agda.Info.t>) => unit
  let addViaFile: (t, string) => unit
  let addViaJSONFile: (t, string) => unit
  // apply Agda highlighting infos
  let apply: (t, VSCode.TextEditor.t) => Promise.t<unit>
  // remove Agda highlighting infos
  let clear: t => unit
  // redecorate everything after the TextEditor has been replaced
  let redecorate: (t, VSCode.TextEditor.t) => unit

  let lookupSrcLoc: (
    t,
    int,
  ) => option<
    Promise.t<array<(VSCode.Range.t, Highlighting.Agda.Info.filepath, VSCode.Position.t)>>,
  >

  module SemanticHighlighting: {
    let update: (t, VSCode.TextDocumentChangeEvent.t) => unit
    let get: t => Promise.t<ref<array<Highlighting.SemanticToken.t>>>
    let toSemanticTokensAndDecorations: (
      t,
      VSCode.TextEditor.t,
    ) => (array<Highlighting.SemanticToken.t>, array<(Editor.Decoration.t, array<VSCode.Range.t>)>)
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

  let fromInfostoDecorations = (
    infosWithRanges: AVLTree.t<(Highlighting.Agda.Info.t, VSCode.Range.t)>,
    editor: VSCode.TextEditor.t,
  ): array<(Editor.Decoration.t, array<VSCode.Range.t>)> => {
    let aspects: array<(Highlighting.Agda.Aspect.t, VSCode.Range.t)> =
      infosWithRanges
      ->AVLTree.toArray
      ->Array.map(((info, range)) =>
        // pair the aspect with the range
        info.aspects->Array.map(aspect => (aspect, range))
      )
      ->Array.concatMany

    aspects
    ->Array.keepMap(((aspect, range)) =>
      Highlighting.Decoration.fromAspect(aspect)->Option.map(x => (x, range))
    )
    ->Highlighting.Decoration.toVSCodeDecorations(editor)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  open Highlighting.Agda

  type t = {
    // from addViaPipe
    mutable infos: Infos.t,
    // Decorations
    mutable decorations: array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    // Semantic Tokens
    mutable semanticTokens: Promise.t<ref<array<Highlighting.SemanticToken.t>>>,
    mutable resolveSemanticTokens: ref<array<Highlighting.SemanticToken.t>> => unit,
  }

  let make = () => {
    let (promise, resolve) = Promise.pending()
    {
      infos: Infos.make(),
      decorations: [],
      semanticTokens: promise,
      resolveSemanticTokens: resolve,
    }
  }

  let clear = self => {
    // reset Infos
    self.infos = Infos.make()
    // remove Decorations 
    self.decorations->Array.forEach(((decoration, _)) => Editor.Decoration.destroy(decoration))
    self.decorations = []
  }

  let destroy = self => {
    Infos.destroy(self.infos)
    clear(self)
  }

  let redecorate = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )

  // insert to an AVLTree
  let addViaPipe = (self, editor, infos) => Infos.insert(self.infos, editor, infos)

  let addViaFile = (self, filepath) => Infos.addEmacsFilePath(self.infos, filepath)
  let addViaJSONFile = (self, filepath) => Infos.addJSONFilePath(self.infos, filepath)

  let lookupSrcLoc = (self, offset): option<
    Promise.t<array<(VSCode.Range.t, Highlighting.Agda.Info.filepath, VSCode.Position.t)>>,
  > => {
    Js.log(self.infos)
    let matched = self.infos->Infos.get->AVLTree.find(offset)
    Js.log3("lookupSrcLoc", matched, self.infos->Infos.get->AVLTree.count)
    // returns the first matching srcloc
    matched
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

    let toSemanticTokensAndDecorations = (self: t, editor: VSCode.TextEditor.t) => {
      let (tokens, backgrounds) =
        self.infos
        ->Infos.get
        ->AVLTree.toArray
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
          // convert Aspects to TokenType / TokenModifiers / Backgrounds
          let (tokenTypeAndModifiers, backgrounds) =
            aspects->Array.map(Highlighting.SemanticToken.fromAspect)->Array.unzip
          let (tokenTypes, tokenModifiers) = tokenTypeAndModifiers->Array.unzip
          // merge TokenType / TokenModifiers / Backgrounds
          let tokenTypes = tokenTypes->Array.keepMap(x => x)
          let tokenModifiers = tokenModifiers->Array.concatMany
          let backgrounds =
            backgrounds->Array.keepMap(x =>
              x->Option.map(x => (
                x,
                Highlighting.SemanticToken.SingleLineRange.toVsCodeRange(range),
              ))
            )

          // only 1 TokenType is allowed, so we take the first one
          let token = tokenTypes[0]->Option.map(tokenType => {
            Highlighting.SemanticToken.range: range,
            type_: tokenType,
            modifiers: Some(tokenModifiers),
          })
          Some(token, backgrounds)
        })
        ->Array.unzip
      let tokens = tokens->Array.keepMap(x => x)
      let backgrounds =
        backgrounds->Array.concatMany->Highlighting.Decoration.toVSCodeDecorations(editor)

      (tokens, backgrounds)
    }
  }

  let apply = (self, editor) =>
    Infos.readTempFiles(self.infos, editor)->Promise.map(() => {
      Js.log("APPLY")
      if Config.Highlighting.getSemanticHighlighting() {
        let (tokens, decorations) = SemanticHighlighting.toSemanticTokensAndDecorations(
          self,
          editor,
        )
        self.decorations = Array.concat(self.decorations, decorations)

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
        let decorations = fromInfostoDecorations(self.infos->Infos.get, editor)
        self.decorations = Array.concat(self.decorations, decorations)
      }
    })
}

include Module
