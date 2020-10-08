open Belt;
module Impl = (Editor: Sig.Editor) => {
  module Decoration = Decoration.Impl(Editor);
  type t = {
    index: int,
    mutable range: (int, int),
    decorationBackground: Editor.Decoration.t,
    decorationIndex: Editor.Decoration.t,
  };

  type action('task) =
    | Instantiate(array(int))
    | UpdateRange
    | Next
    | Previous
    | Modify(t, string => string)
    | SaveCursor
    | RestoreCursor
    | SetCursor(int)
    | JumpToOffset(int)
    | RemoveBoundaryAndDestroy(t)
    | ReplaceWithLines(t, array(string))
    | ReplaceWithLambda(t, array(string))
    // for commands that have both the local (goal-specific) and global (top-level) version
    | LocalOrGlobal(t => list('task), list('task))
    | LocalOrGlobal2(
        (t, string) => list('task),
        t => list('task),
        list('task),
      );

  let generateDiffs =
      (document: Editor.document, indices: array(int))
      : array(SourceFile.Diff.t) => {
    let filePath =
      Editor.getFileName(document)->Option.getWithDefault("unnamed.agda");
    let source = Editor.getText(document);
    SourceFile.parse(indices, filePath, source);
  };

  // make an array of Goal.t with given goal indices
  // modifies the text buffer along the way
  let makeMany =
      (editor: Editor.editor, indices: array(int)): Promise.t(array(t)) => {
    let document = Editor.getDocument(editor);
    let diffs = generateDiffs(document, indices);
    // scan through the diffs to modify the text buffer one by one

    let delta = ref(0);
    let replacements =
      diffs
      ->Array.keep(diff => diff.changed)
      ->Array.map(diff => {
          let range =
            Editor.Range.make(
              Editor.pointAtOffset(
                document,
                fst(diff.originalRange) - delta^,
              ),
              Editor.pointAtOffset(
                document,
                snd(diff.originalRange) - delta^,
              ),
            );

          // update the delta
          delta :=
            delta^
            + (snd(diff.modifiedRange) - fst(diff.modifiedRange))
            - (snd(diff.originalRange) - fst(diff.originalRange));

          let text = diff.content;
          (range, text);
        });

    Editor.replaceTextBatch(document, replacements)
    ->Promise.map(_ => {
        diffs->Array.map(diff => {
          let (decorationBackground, decorationIndex) =
            Decoration.decorateHole(editor, diff.modifiedRange, diff.index);
          {
            index: diff.index,
            range: diff.modifiedRange,
            decorationBackground,
            decorationIndex,
          };
        })
      });
  };

  // parse the whole source file and update the ranges of an array of Goal.t
  let updateRanges = (goals: array(t), document: Editor.document) => {
    let indices = goals->Array.map(goal => goal.index);
    let diffs = generateDiffs(document, indices);
    diffs->Array.forEachWithIndex((i, diff) => {
      switch (goals[i]) {
      | None => () // do nothing :|
      | Some(goal) => goal.range = diff.modifiedRange
      }
    });
  };

  let getInnerRange = (self, editor) =>
    Editor.Range.make(
      Editor.pointAtOffset(editor, fst(self.range) + 2),
      Editor.pointAtOffset(editor, snd(self.range) - 2),
    );

  let getOuterRange = (self, editor) =>
    Editor.Range.make(
      Editor.pointAtOffset(editor, fst(self.range)),
      Editor.pointAtOffset(editor, snd(self.range)),
    );

  let getContent = (self, editor) => {
    let innerRange = getInnerRange(self, editor);
    Editor.getTextInRange(editor, innerRange)->Parser.userInput;
  };

  let setContent = (self, editor, text) => {
    let innerRange = getInnerRange(self, editor);
    Editor.replaceText(editor, innerRange, " " ++ text ++ " ");
  };

  let setCursor = (self, editor) => {
    let (start, _) = self.range;
    let point = Editor.pointAtOffset(Editor.getDocument(editor), start + 3);
    Editor.setCursorPosition(editor, point);
  };

  let buildHaskellRange = (editor, self, old, filepath: string) => {
    let (start, end_) = self.range;
    let startPoint = Editor.pointAtOffset(editor, start);
    let endPoint = Editor.pointAtOffset(editor, end_);

    let startIndex = string_of_int(start + 3);
    let startRow = string_of_int(Editor.Point.line(startPoint) + 1);
    let startColumn = string_of_int(Editor.Point.column(startPoint) + 3);
    let startPart = {j|$(startIndex) $(startRow) $(startColumn)|j};
    let endIndex' = string_of_int(end_ - 3);
    let endRow = string_of_int(Editor.Point.line(endPoint) + 1);
    let endColumn = string_of_int(Editor.Point.column(endPoint) - 1);
    let endPart = {j|$(endIndex') $(endRow) $(endColumn)|j};

    if (old) {
      {j|(Range [Interval (Pn (Just (mkAbsolute "$(filepath)")) $(startPart)) (Pn (Just (mkAbsolute "$(filepath)")) $(endPart))])|j}
      // before (not including) 2.5.1
    } else {
      {j|(intervalsToRange (Just (mkAbsolute "$(filepath)")) [Interval (Pn () $(startPart)) (Pn () $(endPart))])|j}
      // after 2.5.1
    };
  };

  let refreshDecoration = (self, editor: Editor.editor) => {
    // redecorate the background
    let range = getOuterRange(self, Editor.getDocument(editor));
    Editor.Decoration.decorate(editor, self.decorationBackground, [|range|]);
    // redecorate the index
    let range =
      Editor.Range.make(
        Editor.Range.start(range),
        Editor.Point.translate(Editor.Range.end_(range), 0, -2),
      );
    Editor.Decoration.decorate(editor, self.decorationIndex, [|range|]);
  };

  let destroy = self => {
    self.decorationBackground->Editor.Decoration.destroy;
    self.decorationIndex->Editor.Decoration.destroy;
  };
};
