open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module InputMethod = InputMethod.Impl(Editor);
  open! Task;
  // from Editor Command to Tasks
  let handle =
    fun
    | Command.InputMethod.Activate => [
        WithStateP(
          state =>
            if (state.inputMethod.activated) {
              InputMethod.insertBackslash(state.editor);
              InputMethod.deactivate(state.inputMethod);
              Promise.resolved([ViewEvent(InputMethod(Deactivate))]);
            } else {
              state.inputMethod.activated = true;
              // // collect of cursor positions and remove all selected texts
              // let offsets =
              //   Editor.getSelectionRanges(state.editor)
              //   ->Array.map(range => {
              //       let _ = Editor.replaceText(state.editor, range, "");
              //       ();
              //     });

              // activated the input method with positions of cursors
              let startingRanges: array((int, int)) =
                Editor.getSelectionRanges(state.editor)
                ->Array.map(range =>
                    (
                      Editor.offsetAtPoint(
                        state.editor,
                        Editor.Range.start(range),
                      ),
                      Editor.offsetAtPoint(
                        state.editor,
                        Editor.Range.end_(range),
                      ),
                    )
                  );
              InputMethod.activate(
                state.inputMethod,
                state.editor,
                startingRanges,
              );
              Promise.resolved([ViewEvent(InputMethod(Activate))]);
            },
        ),
      ]
    | Deactivate => [
        WithState(
          state => {
            state.inputMethod.activated = false;
            InputMethod.deactivate(state.inputMethod);
          },
        ),
        ViewEvent(InputMethod(Deactivate)),
      ]

    | Update(sequence, translation, index) => [
        ViewEvent(InputMethod(Update(sequence, translation, index))),
      ]
    | InsertChar(char) => [
        WithState(state => {InputMethod.insertChar(state.editor, char)}),
      ]
    | ChooseSymbol(symbol) => [
        WithState(
          state => {
            InputMethod.chooseSymbol(state.inputMethod, state.editor, symbol)
          },
        ),
      ]
    | MoveUp => [
        WithState(
          state => {InputMethod.moveUp(state.inputMethod, state.editor)},
        ),
      ]
    | MoveRight => [
        WithState(
          state => {InputMethod.moveRight(state.inputMethod, state.editor)},
        ),
      ]
    | MoveDown => [
        WithState(
          state => {InputMethod.moveDown(state.inputMethod, state.editor)},
        ),
      ]
    | MoveLeft => [
        WithState(
          state => {InputMethod.moveLeft(state.inputMethod, state.editor)},
        ),
      ];
};
