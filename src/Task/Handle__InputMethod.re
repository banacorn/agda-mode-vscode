open Belt;

module Impl = (Editor: Sig.Editor) => {
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
                Editor.getSelections(state.editor)
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
        // Debug("QueryChange " ++ input),
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
              QueryIM.activate(state.queryIM, input);

              // update the view
              [
                ViewEvent(InputMethod(Activate)),
                ViewEvent(PromptUpdate(input)),
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
                Promise.resolved([ViewEvent(PromptUpdate(input))]);
              };
            } else if (state.queryIM.activated) {
              let result = QueryIM.update(state.queryIM, input);
              switch (result) {
              | None =>
                Promise.resolved([DispatchCommand(InputMethod(Deactivate))])
              | Some((text, command)) =>
                Promise.resolved([
                  ViewEvent(PromptUpdate(text)),
                  DispatchCommand(InputMethod(command)),
                ])
              };
            } else if (shouldActivate) {
              Promise.resolved(activateQueryIM());
            } else {
              Promise.resolved([ViewEvent(PromptUpdate(input))]);
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
        WithStateP(
          state =>
            if (state.editorIM.activated) {
              EditorIM.insertChar(state.editor, char);
              Promise.resolved([]);
            } else if (state.queryIM.activated) {
              let result = QueryIM.insertChar(state.queryIM, char);
              switch (result) {
              | None =>
                Promise.resolved([DispatchCommand(InputMethod(Deactivate))])
              | Some((text, command)) =>
                Promise.resolved([
                  ViewEvent(PromptUpdate(text)),
                  DispatchCommand(InputMethod(command)),
                ])
              };
            } else {
              Promise.resolved([]);
            },
        ),
      ]
    | ChooseSymbol(symbol) => [
        WithStateP(
          state =>
            if (state.editorIM.activated) {
              EditorIM.chooseSymbol(state.editorIM, state.editor, symbol);
              Promise.resolved([]);
            } else if (state.queryIM.activated) {
              switch (state.queryIM.buffer.symbol) {
              | None => Promise.resolved([])
              | Some((_, symbolSequence)) =>
                state.queryIM.buffer = {
                  ...state.queryIM.buffer,
                  symbol: Some((symbol, symbolSequence)),
                };
                Promise.resolved([ViewEvent(PromptUpdate(symbol))]);
              };
            } else {
              Promise.resolved([]);
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
