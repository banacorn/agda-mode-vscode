open Common
module type Module = {
  type t = {
    index: int,
    mutable interval: Interval.t,
    decorationBackground: Editor.Decoration.t,
    decorationIndex: Editor.Decoration.t,
  }

  // helper function for building strings for Agda
  let buildHaskellRange: (t, VSCode.TextDocument.t, string, string) => string
  //
  let generateDiffs: (VSCode.TextDocument.t, array<int>) => array<SourceFile.Diff.t>

  // let makeMany: (
  //   VSCode.TextEditor.t,
  //   array<(int, ((int, int), VSCode.Range.t))>,
  // ) => promise<array<t>>
  // get the content inside the hole
  let getContent: (t, VSCode.TextDocument.t) => string
  // set the content inside the hole
  let setContent: (t, VSCode.TextDocument.t, string) => promise<bool>
  // set cursor inside the hole {! cursor here !}
  //                               ^
  let setCursor: (t, VSCode.TextEditor.t) => unit
  // let getInnerRange: (t, VSCode.TextDocument.t) => VSCode.Range.t
  let refreshDecoration: (t, VSCode.TextEditor.t) => unit
  let destroyDecoration: t => unit
}

module Module: Module = {
  type t = {
    index: int,
    mutable interval: Interval.t,
    decorationBackground: Editor.Decoration.t,
    decorationIndex: Editor.Decoration.t,
  }

  let generateDiffs = (document: VSCode.TextDocument.t, indices: array<int>): array<
    SourceFile.Diff.t,
  > => {
    let fileName = document->VSCode.TextDocument.fileName->Parser.filepath
    let source = Editor.Text.getAll(document)
    SourceFile.parse(indices, fileName, source)
  }

  // make an array of Goal.t with given goal indices
  // modifies the text buffer along the way
  // let makeMany = async (
  //   editor: VSCode.TextEditor.t,
  //   holes: array<(int, ((int, int), VSCode.Range.t))>,
  // ): array<t> => {
  //   let document = VSCode.TextEditor.document(editor)
  //   let indices = holes->Array.map(fst)
  //   // we need to convert question marks "?" to holes "{!   !}"
  //   let replacements = holes->Array.filterMap(((index, (offset, range))) => {
  //     let content = VSCode.TextDocument.getText(document, Some(range))
  //     if content == "?" {
  //       Js.log("found a hole at " ++ string_of_int(index) ++ " " ++ Editor.Range.toString(range))
  //       Some((range, "{!   !}"))
  //     } else {
  //       Js.log("found a non-hole at " ++ string_of_int(index) ++ " " ++ Editor.Range.toString(range))
  //       None
  //     }
  //   })

  //   let diffs = generateDiffs(document, indices)
  //   // scan through the diffs to modify the text buffer one by one

  //   let delta = ref(0)
  //   // let replacements =
  //   //   diffs
  //   //   ->Array.filter(diff => diff.changed)
  //   //   ->Array.mapWithIndex((diff, i) => {
  //   //     let range = VSCode.Range.make(
  //   //       document->VSCode.TextDocument.positionAt(fst(diff.originalInterval) - delta.contents),
  //   //       document->VSCode.TextDocument.positionAt(snd(diff.originalInterval) - delta.contents),
  //   //     )

  //   //     // update the delta
  //   //     delta :=
  //   //       delta.contents +
  //   //       (snd(diff.modifiedInterval) - fst(diff.modifiedInterval)) -
  //   //       (snd(diff.originalInterval) -
  //   //       fst(diff.originalInterval))

  //   //     Js.log("goal range: " ++ Editor.Range.toString(range))
  //   //     Js.log("expected content: " ++ diff.content)
  //   //     Js.log("actual content: " ++ contents[i]->Option.getOr(""))

  //   //     let text = diff.content
  //   //     (range, text)
  //   //   })

  //   let _ = await Editor.Text.batchReplace(document, replacements)
  //   diffs->Array.map(diff => {
  //     // decorate the hole
  //     let (decorationBackground, decorationIndex) = Highlighting.decorateHole(
  //       editor,
  //       diff.modifiedInterval,
  //       diff.index,
  //     )
  //     {
  //       index: diff.index,
  //       interval: diff.modifiedInterval,
  //       decorationBackground,
  //       decorationIndex,
  //     }
  //   })
  // }

  let getInnerRange = (self, document) => {
    let interval = (fst(self.interval) + 2, snd(self.interval) - 2)
    Interval.toVSCodeRange(document, interval)
  }

  let getOuterRange = (self, document) => Interval.toVSCodeRange(document, self.interval)

  let getContent = (self, document) => {
    let innerRange = getInnerRange(self, document)
    Editor.Text.get(document, innerRange)->String.trim
  }

  let setContent = (self, document, text) => {
    let innerRange = getInnerRange(self, document)
    Editor.Text.replace(document, innerRange, " " ++ text ++ " ")
  }

  let setCursor = (self, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let (start, _) = self.interval
    let position = VSCode.TextDocument.positionAt(document, start + 3)
    Editor.Cursor.set(editor, position)
    // scroll to that part of the document
    let range = Interval.toVSCodeRange(document, self.interval)
    editor->VSCode.TextEditor.revealRange(range, None)
  }

  let buildHaskellRange = (self, document, version, filepath: string) => {
    let (start, end_) = self.interval
    let startPoint = VSCode.TextDocument.positionAt(document, start)
    let endPoint = VSCode.TextDocument.positionAt(document, end_)

    let startIndex = string_of_int(start + 3)
    let startRow = string_of_int(VSCode.Position.line(startPoint) + 1)
    let startColumn = string_of_int(VSCode.Position.character(startPoint) + 3)
    let startPart = `${startIndex} ${startRow} ${startColumn}`
    let endIndex' = string_of_int(end_ - 3)
    let endRow = string_of_int(VSCode.Position.line(endPoint) + 1)
    let endColumn = string_of_int(VSCode.Position.character(endPoint) - 1)
    let endPart = `${endIndex'} ${endRow} ${endColumn}`

    if Util.Version.gte(version, "2.8.0") {
      `(intervalsToRange (Just (mkAbsolute "${filepath}")) [Interval () (Pn () ${startPart}) (Pn () ${endPart})])` // after 2.8.0
    } else if Util.Version.gte(version, "2.5.1") {
      `(intervalsToRange (Just (mkAbsolute "${filepath}")) [Interval (Pn () ${startPart}) (Pn () ${endPart})])` // after 2.5.1, before (not including) 2.8.0
    } else {
      `(Range [Interval (Pn (Just (mkAbsolute "${filepath}")) ${startPart}) (Pn (Just (mkAbsolute "${filepath}")) ${endPart})])` // before (not including) 2.5.1
    }
  }

  let refreshDecoration = (self, editor: VSCode.TextEditor.t) => {
    // redecorate the background
    let range = getOuterRange(self, VSCode.TextEditor.document(editor))
    Editor.Decoration.decorate(editor, self.decorationBackground, [range])
    // redecorate the index
    let range = VSCode.Range.make(
      VSCode.Range.start(range),
      VSCode.Position.translate(VSCode.Range.end_(range), 0, -2),
    )
    Editor.Decoration.decorate(editor, self.decorationIndex, [range])
  }

  let destroyDecoration = self => {
    self.decorationBackground->Editor.Decoration.destroy
    self.decorationIndex->Editor.Decoration.destroy
  }
}
include Module
