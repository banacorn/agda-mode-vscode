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
          state => {
            // the places where the input method is activated
            let startingOffsets: array(int) =
              Editor.getCursorPositions(state.editor)
              ->Array.map(Editor.offsetAtPoint(state.editor));

            InputMethod.activate(
              state.editor,
              startingOffsets,
              state.onInputMethodAction,
            );
            Promise.resolved([Debug("InputSymbol(Activated)")]);
          },
        ),
      ]
    | Deactivate => [Debug("InputSymbol(Deactivate)")];
};