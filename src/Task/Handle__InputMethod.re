open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Buffer = Buffer.Impl(Editor);
  module Task = Task.Impl(Editor);
  module EditorIM = EditorIM.Impl(Editor);
  open! Task;
  // from Editor Command to Tasks
  let handle =
    fun
    | Command.InputMethod.Activate => [
        WithStateP(
          state =>
            if (state.editorIM.activated) {
              // already activated, insert backslash "\" instead
              EditorIM.insertBackslash(state.editor);
              EditorIM.deactivate(state.editorIM);
              Promise.resolved([ViewEvent(InputMethod(Deactivate))]);
            } else {
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
              EditorIM.activate(state.editorIM, state.editor, startingRanges);
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

            let deactivateEditorIM = () => {
              EditorIM.deactivate(state.editorIM);
              [ViewEvent(InputMethod(Deactivate))];
            };
            let activateQueryIM = () => {
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

            if (state.editorIM.activated) {
              if (shouldActivate) {
                Promise.resolved(
                  List.concatMany([|
                    deactivateEditorIM(),
                    activateQueryIM(),
                  |]),
                );
              } else {
                Promise.resolved([ViewEvent(QueryUpdate(input))]);
              };
            } else if (state.queryIM.activated) {
              // let buffer = Buffer.make();
              // Buffer.reflectEditorChange(buffer, )
              // Buffer.toSequence(instance.buffer)
              Promise.resolved([
                Debug("ByQuery => ByQuery"),
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
              ]);
            } else if (shouldActivate) {
              Promise.resolved(activateQueryIM());
            } else {
              Promise.resolved([ViewEvent(QueryUpdate(input))]);
            };
          },
        ),
      ]
    | Deactivate => [
        WithState(state => EditorIM.deactivate(state.editorIM)),
        ViewEvent(InputMethod(Deactivate)),
      ]

    | Update(sequence, translation, index) => [
        ViewEvent(InputMethod(Update(sequence, translation, index))),
      ]
    | InsertChar(char) => [
        WithState(state => {EditorIM.insertChar(state.editor, char)}),
      ]
    | ChooseSymbol(symbol) => [
        WithState(
          state => {
            EditorIM.chooseSymbol(state.editorIM, state.editor, symbol)
          },
        ),
      ]
    | MoveUp => [
        WithState(state => {EditorIM.moveUp(state.editorIM, state.editor)}),
      ]
    | MoveRight => [
        WithState(
          state => {EditorIM.moveRight(state.editorIM, state.editor)},
        ),
      ]
    | MoveDown => [
        WithState(
          state => {EditorIM.moveDown(state.editorIM, state.editor)},
        ),
      ]
    | MoveLeft => [
        WithState(
          state => {EditorIM.moveLeft(state.editorIM, state.editor)},
        ),
      ];
};
