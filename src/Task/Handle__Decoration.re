module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  open! Task;
  open Belt;
  // from Decoration to Tasks
  let handle =
    fun
    | Decoration.Add(highlightings) => [
        WithState(
          state => {
            let decorations =
              highlightings
              ->Array.map(highlighting => {
                  Decoration.decorateHighlighting(state.editor, highlighting)
                })
              ->Array.concatMany;

            state.decorations = Array.concat(state.decorations, decorations);
          },
        ),
      ];
};
