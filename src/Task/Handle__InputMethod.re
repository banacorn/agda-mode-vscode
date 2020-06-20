open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module InputMethod = InputMethod.Impl(Editor);
  open! Task;
  // from Editor Command to Tasks
  let handle =
    fun
    | Command.InputMethod.Activate => [
        WithState(
          state =>
            if (state.inputMethod.activated) {
              InputMethod.insertBackslash(state.editor);
              InputMethod.deactivate(state.inputMethod);
              Promise.resolved([ViewReq(InputMethod(Deactivate), _ => [])]);
            } else {
              // setContext
              Editor.setContext("agdaModeTyping", true)->ignore;

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
              Promise.resolved([ViewReq(InputMethod(Activate), _ => [])]);
            },
        ),
      ]
    | Deactivate => [
        WithState(
          state => {
            // setContext
            Editor.setContext("agdaModeTyping", false)->ignore;

            state.inputMethod.activated = false;
            InputMethod.deactivate(state.inputMethod);
            // Promise.resolved([Debug("InputMethod(Deactivated)")]);
            Promise.resolved([]);
          },
        ),
        ViewReq(InputMethod(Deactivate), _ => []),
      ]

    | Update(sequence, translation, index) => [
        ViewReq(InputMethod(Update(sequence, translation, index)), _ => []),
      ]
    | InsertChar(char) => [
        WithState(
          state => {
            InputMethod.insertChar(state.editor, char);
            Promise.resolved([]);
          },
        ),
      ]
    | ChooseSymbol(symbol) => [
        WithState(
          state => {
            InputMethod.chooseSymbol(state.inputMethod, state.editor, symbol);
            Promise.resolved([]);
          },
        ),
      ]
    | MoveUp => [
        WithState(
          state => {
            InputMethod.moveUp(state.inputMethod, state.editor);
            Promise.resolved([]);
          },
        ),
      ]
    | MoveRight => [
        WithState(
          state => {
            InputMethod.moveRight(state.inputMethod, state.editor);
            Promise.resolved([]);
          },
        ),
      ]
    | MoveDown => [
        WithState(
          state => {
            InputMethod.moveDown(state.inputMethod, state.editor);
            Promise.resolved([]);
          },
        ),
      ]
    | MoveLeft => [
        WithState(
          state => {
            InputMethod.moveLeft(state.inputMethod, state.editor);
            Promise.resolved([]);
          },
        ),
      ];
};