module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module Decoration = Decoration.Impl(Editor);
  open! Task;
  open Belt;
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
            let readFile = N.Util.promisify(N.Fs.readFile);
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
                    let annotations: array(Highlighting.t) =
                      expressions
                      ->Array.keepMap(
                          fun
                          | Error(_) => None // filter errors out
                          | Ok(L(xs)) =>
                            Some(Highlighting.parseIndirectHighlightings(xs))
                          | Ok(_) => Some([||]),
                        )
                      ->Array.concatMany;
                    // [Decoration(AddDirectly(annotations))];

                    let decorations =
                      annotations
                      ->Array.map(annotation => {
                          Decoration.decorateHighlighting(
                            state.editor,
                            annotation,
                          )
                        })
                      ->Array.concatMany;

                    state.decorations =
                      Array.concat(state.decorations, decorations);
                  }
                // TODO: we should do something about these parse errors
                | Error(_err) => (),
              )
            ->ignore;
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
