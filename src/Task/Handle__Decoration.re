module Impl = (Editor: Sig.Editor) => {
  module State = State.Impl(Editor);
  module Task = Task.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  open! Task;
  open Belt;

  let readFile = N.Util.promisify(N.Fs.readFile);

  let readAndParse = (filepath): Promise.t(array(Highlighting.t)) => {
    readFile(. filepath)
    ->Promise.Js.fromBsPromise
    ->Promise.Js.toResult
    ->Promise.map(
        fun
        | Ok(content) => {
            open! Parser.SExpression;
            let expressions =
              content->Node.Buffer.toString->Parser.SExpression.parse;
            // TODO: we should do something about these parse errors
            let _parseErrors: array((int, string)) =
              expressions->Array.keepMap(
                fun
                | Error(error) => Some(error)
                | Ok(_) => None,
              );
            expressions
            ->Array.keepMap(
                fun
                | Error(_) => None // filter errors out
                | Ok(L(xs)) =>
                  Some(Highlighting.parseIndirectHighlightings(xs))
                | Ok(_) => Some([||]),
              )
            ->Array.concatMany;
          }
        // TODO: we should do something about these parse errors
        | Error(_err) => [||],
      );
  };

  // from Decoration to Tasks
  let handle =
    fun
    | Decoration.AddDirectly(highlightings) => [
        WithState(
          state => {
            let decorations =
              highlightings
              ->Array.map(highlighting => {
                  Decoration.decorateHighlighting(state.editor, highlighting)
                })
              ->Array.concatMany;
            state.decorations = Array.concat(state.decorations, decorations);
          },
        ),
      ]
    | AddIndirectly(filepath) => [
        WithState(
          state => {
            Js.Array.push(filepath, state.indirectHighlightingFileNames)
            ->ignore
          },
        ),
      ]
    | StopAddingIndirectly => [
        WithState(
          state => {
            state.indirectHighlightingFileNames
            ->Array.map(readAndParse)
            ->Promise.allArray
            ->Promise.map(highlightings => {
                let decorations =
                  highlightings
                  ->Array.concatMany
                  ->Array.map(highlighting => {
                      Decoration.decorateHighlighting(
                        state.editor,
                        highlighting,
                      )
                    })
                  ->Array.concatMany;
                state.decorations =
                  Array.concat(state.decorations, decorations);
                state.indirectHighlightingFileNames = [||];
                [];
              })
            ->ignore
          },
        ),
      ]
    | RemoveAll => [
        WithState(
          state => {
            state.decorations
            ->Array.map(fst)
            ->Array.forEach(Editor.Decoration.destroy);
            state.decorations = [||];
          },
        ),
      ]
    | Refresh => [
        WithState(
          state => {
            // highlightings
            state.decorations
            ->Array.forEach(((decoration, range)) => {
                Editor.Decoration.decorate(
                  state.editor,
                  decoration,
                  [|range|],
                )
              });
            // goal decorations
            state.goals
            ->Array.forEach(goal =>
                goal->Goal.refreshDecoration(state.editor)
              );
          },
        ),
      ];
};
