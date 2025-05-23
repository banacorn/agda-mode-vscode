open Common

module Decoration = Highlighting__Decoration
module SemanticToken = Highlighting__SemanticToken

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

  // redecorate everything after the TextEditor has been replaced
  let redecorate: (t, VSCode.TextEditor.t) => unit
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
  }

  let make = () => {
    decorations: [],
  }

  let clear = self => {
    // remove Decorations
    self.decorations->Array.forEach(((decoration, _)) => Editor.Decoration.destroy(decoration))
    self.decorations = []
  }

  let destroy = self => {
    clear(self)
  }

  let redecorate = (self, editor) =>
    self.decorations->Array.forEach(((decoration, ranges)) =>
      Editor.Decoration.decorate(editor, decoration, ranges)
    )
}

include Module
