module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Task = Task.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  open! Task;
  open Belt;

  // from Decoration to Tasks
  let handle =
    fun
    | Decoration.AddDirectly(highlightings) => [
        WithState(
          state => {state.decorations->Decoration.addDirectly(highlightings)},
        ),
      ]
    | AddIndirectly(filepath) => [
        WithState(
          state => {state.decorations->Decoration.addIndirectly(filepath)},
        ),
      ]
    | Apply => [
        timeStart(">>> Apply decorations"),
        WithStateP(
          state => {
            let receivedNewHighlighting =
              Array.length(state.decorations.highlightings) > 0;
            let receivedNewTempFilePaths =
              Array.length(state.decorations.tempFilePaths) > 0;
            // only remove old decorations and apply new ones, when there are new yet-to-be-applied highlightings
            if (receivedNewHighlighting || receivedNewTempFilePaths) {
              Decoration.readTempFiles(state.decorations)
              ->Promise.map(() => {
                  Decoration.applyHighlightings(
                    state.decorations,
                    state.editor,
                  );
                  [];
                });
            } else {
              Promise.resolved([]);
            };
          },
        ),
        timeEnd(">>> Apply decorations"),
      ]
    | Refresh => [
        WithState(
          state => {
            // highlightings
            Decoration.refresh(state.editor, state.decorations);
            // goal decorations
            state.goals
            ->Array.forEach(goal =>
                goal->Goal.refreshDecoration(state.editor)
              );
          },
        ),
      ];
};
