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
            switch (state.inputMethod.activated) {
            | InputMethod.ByEditor =>
              InputMethod.insertBackslash(state.editor);
              InputMethod.deactivate(state.inputMethod);
              Promise.resolved([ViewEvent(InputMethod(Deactivate))]);
            | ByQuery => Promise.resolved([Debug(":D")])
            | No =>
              state.inputMethod.activated = ByEditor;
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
    | QueryChange(input) => [
        Debug("QueryChange " ++ input),
        WithStateP(
          state =>
            {
              // activate when the user typed a backslash "/"
              let shouldActivate = Js.String.endsWith("\\", input);
              Promise.resolved([ViewEvent(QueryUpdate(input))]);
            },
            // switch (state.inputMethod.activated) {
            // | InputMethod.ByEditor =>
            //   if (shouldActivate) {
            //     // the InputMethod was activated by the editor, should deactivate it first
            //     InputMethod.deactivate(state.inputMethod);
            //     Promise.resolved([ViewEvent(InputMethod(Deactivate))]);
            //   } else {
            //     // do nothing
            //     Promise.resolved([]);
            //   }
            // | ByQuery =>
            //   if (shouldActivate) {
            //     Promise.resolved([
            //       Debug("ByQuery: activate"),
            //       ViewEvent(QueryUpdate(input)),
            //     ]);
            //   } else {
            //     Promise.resolved([Debug("ByQuery: nothing")]);
            //   }
            // | No =>
            //   if (shouldActivate) {
            //     state.inputMethod.activated = ByQuery;
            //     // remove the ending backslash "/"
            //     let input =
            //       Js.String.substring(
            //         ~from=0,
            //         ~to_=String.length(input) - 1,
            //         input,
            //       );
            //     Promise.resolved([
            //       ViewEvent(InputMethod(Activate)),
            //       ViewEvent(QueryUpdate(input)),
            //     ]);
            //   } else {
            //     Promise.resolved([]);
            //   }
            // };
        ),
      ]
    | Deactivate => [
        WithState(
          state => {
            state.inputMethod.activated = No;
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
