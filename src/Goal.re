open Belt;
module Impl = (Editor: Sig.Editor) => {
  // module State = State.Impl(Editor);
  type t = {
    index: int,
    mutable range: Editor.Range.t,
    // marker: Editor.Decoration.t,
  };

  // NOTE: helper function of `makeMany`
  let make =
      (editor: Editor.editor, diff: SourceFile.Diff.t)
      : (unit => Promise.t(t)) => {
    let originalRange =
      Editor.Range.make(
        Editor.pointAtOffset(editor, fst(diff.originalRange)),
        Editor.pointAtOffset(editor, snd(diff.originalRange)),
      );
    // modify the text buffer base on the Diff
    () =>
      Editor.setText(editor, originalRange, diff.content)
      ->Promise.map(_ => {
          let modifiedRange =
            Editor.Range.make(
              Editor.pointAtOffset(editor, fst(diff.modifiedRange)),
              Editor.pointAtOffset(editor, snd(diff.modifiedRange)),
            );
          {index: diff.index, range: modifiedRange};
        });
  };

  // make an array of Goal.t with given goal indices
  // modifies the text buffer along the way
  let makeMany =
      (editor: Editor.editor, indices: array(int)): Promise.t(array(t)) => {
    let filePath =
      Editor.getFileName(editor)->Option.getWithDefault("unnamed.agda");
    let source = Editor.getText(editor);
    let diffs = SourceFile.parse(indices, filePath, source);
    // scan through the diffs to modify the text buffer

    diffs->Array.map(make(editor))->Util.oneByOne;
  };

  let destroy = self => ();
};