open VSCode
module VSRange = Range
open Belt

type t = {
  index: int,
  mutable range: (int, int),
  decorationBackground: Editor.Decoration.t,
  decorationIndex: Editor.Decoration.t,
}

type action<'task> =
  | Instantiate(array<int>)
  | UpdateRange
  | Next
  | Previous
  | Modify(t, string => string)
  | SaveCursor
  | RestoreCursor
  | SetCursor(int)
  | JumpToOffset(int)
  | RemoveBoundaryAndDestroy(t)
  | ReplaceWithLines(t, array<string>)
  | ReplaceWithLambda(t, array<string>)
  // for commands that have both the local (goal-specific) and global (top-level) version
  | LocalOrGlobal(t => list<'task>, list<'task>)
  | LocalOrGlobal2((t, string) => list<'task>, t => list<'task>, list<'task>)

let generateDiffs = (document: TextDocument.t, indices: array<int>): array<SourceFile.Diff.t> => {
  let filePath = document->TextDocument.fileName->Parser.filepath
  let source = Editor.Text.getAll(document)
  SourceFile.parse(indices, filePath, source)
}

// make an array of Goal.t with given goal indices
// modifies the text buffer along the way
let makeMany = (editor: TextEditor.t, indices: array<int>): Promise.t<array<t>> => {
  let document = TextEditor.document(editor)
  let diffs = generateDiffs(document, indices)
  // scan through the diffs to modify the text buffer one by one

  let delta = ref(0)
  let replacements = diffs->Array.keep(diff => diff.changed)->Array.map(diff => {
    let range = VSRange.make(
      document->TextDocument.positionAt(fst(diff.originalRange) - delta.contents),
      document->TextDocument.positionAt(snd(diff.originalRange) - delta.contents),
    )

    // update the delta
    delta :=
      delta.contents +
      (snd(diff.modifiedRange) - fst(diff.modifiedRange)) -
      (snd(diff.originalRange) -
      fst(diff.originalRange))

    let text = diff.content
    (range, text)
  })

  Editor.Text.batchReplace(document, replacements)->Promise.map(_ => diffs->Array.map(diff => {
      let (decorationBackground, decorationIndex) = Decoration.decorateHole(
        editor,
        diff.modifiedRange,
        diff.index,
      )
      {
        index: diff.index,
        range: diff.modifiedRange,
        decorationBackground: decorationBackground,
        decorationIndex: decorationIndex,
      }
    }))
}

// parse the whole source file and update the ranges of an array of Goal.t
let updateRanges = (goals: array<t>, document: TextDocument.t) => {
  let indices = goals->Array.map(goal => goal.index)
  let diffs = generateDiffs(document, indices)
  diffs->Array.forEachWithIndex((i, diff) =>
    switch goals[i] {
    | None => () // do nothing :|
    | Some(goal) => goal.range = diff.modifiedRange
    }
  )
}

let getInnerRange = (self, document) =>
  VSRange.make(
    document->TextDocument.positionAt(fst(self.range) + 2),
    document->TextDocument.positionAt(snd(self.range) - 2),
  )

let getOuterRange = (self, document) =>
  VSRange.make(
    document->TextDocument.positionAt(fst(self.range)),
    document->TextDocument.positionAt(snd(self.range)),
  )

let getContent = (self, document) => {
  let innerRange = getInnerRange(self, document)
  Editor.Text.get(document, innerRange)->Parser.userInput
}

let setContent = (self, document, text) => {
  let innerRange = getInnerRange(self, document)
  Editor.Text.replace(document, innerRange, " " ++ (text ++ " "))
}

let setCursor = (self, editor) => {
  let (start, _) = self.range
  let point = editor->TextEditor.document->TextDocument.positionAt(start + 3)
  Editor.Cursor.set(editor, point)
}

let buildHaskellRange = (document, self, old, filepath: string) => {
  let (start, end_) = self.range
  let startPoint = TextDocument.positionAt(document, start)
  let endPoint = TextDocument.positionAt(document, end_)

  let startIndex = string_of_int(start + 3)
  let startRow = string_of_int(Position.line(startPoint) + 1)
  let startColumn = string_of_int(Position.character(startPoint) + 3)
  let startPart = j`$(startIndex) $(startRow) $(startColumn)`
  let endIndex' = string_of_int(end_ - 3)
  let endRow = string_of_int(Position.line(endPoint) + 1)
  let endColumn = string_of_int(Position.character(endPoint) - 1)
  let endPart = j`$(endIndex') $(endRow) $(endColumn)`

  if old {
    j`(Range [Interval (Pn (Just (mkAbsolute "$(filepath)")) $(startPart)) (Pn (Just (mkAbsolute "$(filepath)")) $(endPart))])` // before (not including) 2.5.1
  } else {
    j`(intervalsToRange (Just (mkAbsolute "$(filepath)")) [Interval (Pn () $(startPart)) (Pn () $(endPart))])` // after 2.5.1
  }
}

let refreshDecoration = (self, editor: TextEditor.t) => {
  // redecorate the background
  let range = getOuterRange(self, TextEditor.document(editor))
  Editor.Decoration.decorate(editor, self.decorationBackground, [range])
  // redecorate the index
  let range = VSRange.make(VSRange.start(range), Position.translate(VSRange.end_(range), 0, -2))
  Editor.Decoration.decorate(editor, self.decorationIndex, [range])
}

let destroy = self => {
  self.decorationBackground->Editor.Decoration.destroy
  self.decorationIndex->Editor.Decoration.destroy
}
