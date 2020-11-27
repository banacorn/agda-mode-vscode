open Belt

open! Task
// from Editor Command to Tasks
let handle = x =>
  switch x {
  | Command.InputMethod.Activate => list{
      WithStateP(
        state =>
          if state.editorIM.activated {
            // already activated, insert backslash "\" instead
            EditorIM.insertBackslash(state.editor)
            EditorIM.deactivate(state.editorIM)
            Promise.resolved(list{ViewEvent(InputMethod(Deactivate))})
          } else {
            let document = VSCode.TextEditor.document(state.editor)
            // activated the input method with positions of cursors
            let startingRanges: array<(int, int)> =
              Editor.Selection.getMany(state.editor)->Array.map(range => (
                document->VSCode.TextDocument.offsetAt(VSCode.Range.start(range)),
                document->VSCode.TextDocument.offsetAt(VSCode.Range.end_(range)),
              ))
            EditorIM.activate(state.editorIM, state.editor, startingRanges)
            Promise.resolved(list{ViewEvent(InputMethod(Activate))})
          },
      ),
    }
  | PromptChange(input) => list{
      // Debug("PromptChange " ++ input),
      WithStateP(
        state => {
          // activate when the user typed a backslash "/"
          let shouldActivate = Js.String.endsWith("\\", input)

          let deactivateEditorIM = () => {
            EditorIM.deactivate(state.editorIM)
            list{ViewEvent(InputMethod(Deactivate))}
          }
          let activatePromptIM = () => {
            // remove the ending backslash "\"
            let input = Js.String.substring(~from=0, ~to_=String.length(input) - 1, input)
            PromptIM.activate(state.promptIM, input)

            // update the view
            list{ViewEvent(InputMethod(Activate)), ViewEvent(PromptIMUpdate(input))}
          }

          if state.editorIM.activated {
            if shouldActivate {
              Promise.resolved(List.concatMany([deactivateEditorIM(), activatePromptIM()]))
            } else {
              Promise.resolved(list{ViewEvent(PromptIMUpdate(input))})
            }
          } else if state.promptIM.activated {
            let result = PromptIM.update(state.promptIM, input)
            switch result {
            | None => Promise.resolved(list{DispatchCommand(InputMethod(Deactivate))})
            | Some((text, command)) =>
              Promise.resolved(list{
                ViewEvent(PromptIMUpdate(text)),
                DispatchCommand(InputMethod(command)),
              })
            }
          } else if shouldActivate {
            Promise.resolved(activatePromptIM())
          } else {
            Promise.resolved(list{ViewEvent(PromptIMUpdate(input))})
          }
        },
      ),
    }
  | Deactivate => list{
      WithState(
        state => {
          EditorIM.deactivate(state.editorIM)
          PromptIM.deactivate(state.promptIM)
        },
      ),
      ViewEvent(InputMethod(Deactivate)),
    }

  | Update(sequence, translation, index) => list{
      ViewEvent(InputMethod(Update(sequence, translation, index))),
    }
  | InsertChar(char) => list{
      WithStateP(
        state =>
          if state.editorIM.activated {
            EditorIM.insertChar(state.editor, char)
            Promise.resolved(list{})
          } else if state.promptIM.activated {
            let result = PromptIM.insertChar(state.promptIM, char)
            switch result {
            | None => Promise.resolved(list{DispatchCommand(InputMethod(Deactivate))})
            | Some((text, command)) =>
              Promise.resolved(list{
                ViewEvent(PromptIMUpdate(text)),
                DispatchCommand(InputMethod(command)),
              })
            }
          } else {
            Promise.resolved(list{})
          },
      ),
    }
  | ChooseSymbol(symbol) => list{
      WithStateP(
        state =>
          if state.editorIM.activated {
            EditorIM.chooseSymbol(state.editorIM, state.editor, symbol)
            Promise.resolved(list{})
          } else if state.promptIM.activated {
            switch state.promptIM.buffer.symbol {
            | None => Promise.resolved(list{})
            | Some((_, symbolSequence)) =>
              state.promptIM.buffer = {
                ...state.promptIM.buffer,
                symbol: Some((symbol, symbolSequence)),
              }
              Promise.resolved(list{ViewEvent(PromptIMUpdate(symbol))})
            }
          } else {
            Promise.resolved(list{})
          },
      ),
    }
  | MoveUp => list{WithState(state => EditorIM.moveUp(state.editorIM, state.editor))}
  | MoveRight => list{WithState(state => EditorIM.moveRight(state.editorIM, state.editor))}
  | MoveDown => list{WithState(state => EditorIM.moveDown(state.editorIM, state.editor))}
  | MoveLeft => list{WithState(state => EditorIM.moveLeft(state.editorIM, state.editor))}
  }
