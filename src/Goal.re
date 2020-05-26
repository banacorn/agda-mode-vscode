module Impl = (Editor: Sig.Editor) => {
  type t = {
    index: int,
    mutable range: Editor.Range.t,
    marker: Editor.Decoration.t,
    mutable content: string,
    mutable disposables: Editor.Disposable.t,
  };
};