open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Buffer = Buffer.Impl(Editor);
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
          state => {
            // activate when the user typed a backslash "/"
            let shouldActivate = Js.String.endsWith("\\", input);

            let deactivateByEditor = () => {
              InputMethod.deactivate(state.inputMethod);
              [ViewEvent(InputMethod(Deactivate))];
            };
            let activateByQuery = () => {
              state.inputMethod.activated = ByQuery;
              // remove the ending backslash "\"
              let input =
                Js.String.substring(
                  ~from=0,
                  ~to_=String.length(input) - 1,
                  input,
                );
              [
                ViewEvent(InputMethod(Activate)),
                ViewEvent(QueryUpdate(input)),
              ];
            };

            switch (state.inputMethod.activated) {
            | ByEditor =>
              if (shouldActivate) {
                Promise.resolved(
                  List.concatMany([|
                    deactivateByEditor(),
                    activateByQuery(),
                  |]),
                );
              } else {
                // do nothing
                Promise.resolved([
                  Debug("ByEditor => ByEditor"),
                  ViewEvent(QueryUpdate(input)),
                ]);
              }
            | ByQuery =>
              let buffer = Buffer.make();
              // Buffer.reflectEditorChange(buffer, )
              // Buffer.toSequence(instance.buffer)
              Promise.resolved([Debug("ByQuery => ByQuery")]);
            // if (shouldActivate) {
            //   // already activated, insert backslash "\"
            //   InputMethod.deactivate(state.inputMethod);
            //   Promise.resolved([
            //     Debug("ByQuery => No"),
            //     ViewEvent(QueryUpdate(input)),
            //     ViewEvent(InputMethod(Deactivate)),
            //   ]);
            // } else {
            //   Promise.resolved([Debug("ByQuery => ByQuery")]);
            // }
            | No =>
              if (shouldActivate) {
                Promise.resolved(activateByQuery());
              } else {
                Promise.resolved([ViewEvent(QueryUpdate(input))]);
              }
            };
          },
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
