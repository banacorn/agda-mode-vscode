open Command.InputMethodAction;
open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module InputMethod = InputMethod.Impl(Editor);
  open! Task;
  // from Editor Command to Tasks
  let handle =
    fun
    | Activate => [
        WithState(
          state =>
            if (state.inputMethod.activated) {
              InputMethod.insertBackslash(state.editor);
              Promise.resolved([]);
            } else {
              state.inputMethod.activated = true;
              // the places where the input method is activated
              let startingOffsets: array(int) =
                Editor.getCursorPositions(state.editor)
                ->Array.map(Editor.offsetAtPoint(state.editor));
              InputMethod.activate(
                state.inputMethod,
                state.editor,
                startingOffsets,
              );
              // Promise.resolved([Debug("InputMethod(Activated)")]);
              Promise.resolved([]);
            },
        ),
      ]
    | Deactivate => [
        WithState(
          state =>
            if (state.inputMethod.activated) {
              state.inputMethod.activated = false;
              // Promise.resolved([Debug("InputMethod(Deactivated)")]);
              Promise.resolved([]);
            } else {
              Promise.resolved([]);
            },
        ),
      ];
};