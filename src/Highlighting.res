open Common

module Decoration = Highlighting__Decoration
module SemanticToken = Highlighting__SemanticToken

module type Module = {
  type t

  let make: option<Resource.t<array<SemanticToken.t>>> => t
  let destroy: t => unit

  // for decorating Goals
  let decorateHole: (
    VSCode.TextEditor.t,
    Interval.t,
    int,
  ) => (VSCode.TextEditorDecorationType.t, VSCode.TextEditorDecorationType.t)

  // let clear: t => unit
  // redecorate everything after the TextEditor has been replaced
  let redecorate: (t, VSCode.TextEditor.t) => unit

  // let updateSemanticHighlighting: (t, VSCode.TextDocumentChangeEvent.t) => promise<unit>
  // let getSemanticTokens: t => Resource.t<array<SemanticToken.t>>
  // let apply: (t, Tokens.t, VSCode.TextEditor.t) => promise<unit>
}

module Module: Module = {
  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  let decorateHole = (editor: VSCode.TextEditor.t, interval: Interval.t, index: int) => {
    let document = VSCode.TextEditor.document(editor)
    let backgroundRange = Interval.toVSCodeRange(document, interval)

    let background = Editor.Decoration.highlightBackground(
      editor,
      "editor.selectionHighlightBackground",
      [backgroundRange],
    )
    let indexText = string_of_int(index)
    let innerInterval = (fst(interval), snd(interval) - 2)
    let indexRange = Interval.toVSCodeRange(document, innerInterval)

    let index = Editor.Decoration.overlayText(
      editor,
      "editorLightBulb.foreground",
      indexText,
      indexRange,
    )

    (background, index)
  }

  ////////////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////////////

  type t = {
    // Decorations
    mutable decorations: array<(Editor.Decoration.t, array<VSCode.Range.t>)>,
    // Semantic Tokens
    mutable semanticTokens: Resource.t<array<SemanticToken.t>>,
  }

  let make = semanticTokens => {
    decorations: [],
    semanticTokens: switch semanticTokens {
    | None => Resource.make()
    | Some(semanticTokens) => semanticTokens
    },
  }

  let clear = self => {
    // remove Decorations
    self.decorations->Array.forEach(((decoration, _)) => Editor.Decoration.destroy(decoration))
    self.decorations = []
  }

  let destroy = self => {
    // Tokens.destroy(self.infos)
    clear(self)
  }

  let redecorate = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )

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
        // +1 line for each linebreak ('\n' or '\r\n')
        // -1 line for each line in `removedRange`
        // +1 column for each character after the last linebreak
        // -1 column for each character in `removedRange`
        let lines = Parser.splitToLines(change->VSCode.TextDocumentContentChangeEvent.text)
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
          | Some(line) =>
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
      } else if lineDelta == 0 {
        NoOp
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
                abs(fst(token.range.column) + columnDelta),
                abs(snd(token.range.column) + columnDelta),
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

  let updateSemanticHighlighting = async (self, event) => {
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
      ->Array.flat

    // apply changes to the cached tokens
    let tokens = await self.semanticTokens->Resource.get
    self.semanticTokens->Resource.set(changes->Array.reduce(tokens, applyChange))
  }

  let getSemanticTokens = (self: t) => self.semanticTokens

  let apply = async (self, tokens, editor) => {
    if Config.Highlighting.getHighlightWithThemeColors() {
      let (decorations, semanticTokens) = Tokens.toDecorationsAndSemanticTokens(tokens, editor)
      self.semanticTokens->Resource.set(semanticTokens)
      self.decorations = Array.concat(self.decorations, decorations)
    } else {
      let decorations = Tokens.toDecorations(tokens, editor)
      self.decorations = Array.concat(self.decorations, decorations)
      ()
    }
  }
}

include Module
