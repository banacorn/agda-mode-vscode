open Belt;

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
            let document = VSCode.TextEditor.document(state.editor);
            // activated the input method with positions of cursors
            let startingRanges: array((int, int)) =
              Editor.Selection.getMany(state.editor)
              ->Array.map(range =>
                  (
                    document->VSCode.TextDocument.offsetAt(
                      VSCode.Range.start(range),
                    ),
                    document->VSCode.TextDocument.offsetAt(
                      VSCode.Range.end_(range),
                    ),
                  )
                );
            EditorIM.activate(state.editorIM, state.editor, startingRanges);
            Promise.resolved([ViewEvent(InputMethod(Activate))]);
          },
      ),
    ]
  | PromptChange(input) => [
      // Debug("PromptChange " ++ input),
      WithStateP(
        state => {
          // activate when the user typed a backslash "/"
          let shouldActivate = Js.String.endsWith("\\", input);

          let deactivateEditorIM = () => {
            EditorIM.deactivate(state.editorIM);
            [ViewEvent(InputMethod(Deactivate))];
          };
          let activatePromptIM = () => {
            // remove the ending backslash "\"
            let input =
              Js.String.substring(
                ~from=0,
                ~to_=String.length(input) - 1,
                input,
              );
            PromptIM.activate(state.promptIM, input);

            // update the view
            [
              ViewEvent(InputMethod(Activate)),
              ViewEvent(PromptIMUpdate(input)),
            ];
          };

          if (state.editorIM.activated) {
            if (shouldActivate) {
              Promise.resolved(
                List.concatMany([|deactivateEditorIM(), activatePromptIM()|]),
              );
            } else {
              Promise.resolved([ViewEvent(PromptIMUpdate(input))]);
            };
          } else if (state.promptIM.activated) {
            let result = PromptIM.update(state.promptIM, input);
            switch (result) {
            | None =>
              Promise.resolved([DispatchCommand(InputMethod(Deactivate))])
            | Some((text, command)) =>
              Promise.resolved([
                ViewEvent(PromptIMUpdate(text)),
                DispatchCommand(InputMethod(command)),
              ])
            };
          } else if (shouldActivate) {
            Promise.resolved(activatePromptIM());
          } else {
            Promise.resolved([ViewEvent(PromptIMUpdate(input))]);
          };
        },
      ),
    ]
  | Deactivate => [
      WithState(
        state => {
          EditorIM.deactivate(state.editorIM);
          PromptIM.deactivate(state.promptIM);
        },
      ),
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
          } else if (state.promptIM.activated) {
            let result = PromptIM.insertChar(state.promptIM, char);
            switch (result) {
            | None =>
              Promise.resolved([DispatchCommand(InputMethod(Deactivate))])
            | Some((text, command)) =>
              Promise.resolved([
                ViewEvent(PromptIMUpdate(text)),
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
          } else if (state.promptIM.activated) {
            switch (state.promptIM.buffer.symbol) {
            | None => Promise.resolved([])
            | Some((_, symbolSequence)) =>
              state.promptIM.buffer = {
                ...state.promptIM.buffer,
                symbol: Some((symbol, symbolSequence)),
              };
              Promise.resolved([ViewEvent(PromptIMUpdate(symbol))]);
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
      WithState(state => {EditorIM.moveRight(state.editorIM, state.editor)}),
    ]
  | MoveDown => [
      WithState(state => {EditorIM.moveDown(state.editorIM, state.editor)}),
    ]
  | MoveLeft => [
      WithState(state => {EditorIM.moveLeft(state.editorIM, state.editor)}),
    ];
