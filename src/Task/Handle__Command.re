open Command;
open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  // from Editor Command to Tasks
  let handle =
    fun
    | Load => [
        Task.WithState(
          state => Editor.save(state.editor)->Promise.map(_ => []),
        ),
        SendRequest(Load),
      ]
    | Quit => [Terminate]
    | NextGoal => [
        Task.WithState(
          state => {
            let nextGoal = ref(None);
            let cursor = Editor.getCursorPosition(state.editor);

            let getPositions = (): array(Editor.Point.t) => {
              state.goals
              ->Array.map(goal => goal.range)
              ->Array.map(range =>
                  Editor.Point.translate(Editor.Range.start(range), 0, 3)
                );
            };
            let positions = getPositions();

            // find the first Goal after the cursor
            positions->Array.forEach(position =>
              if (Editor.Point.compare(cursor, position) === Editor.LT
                  && nextGoal^ === None) {
                nextGoal := Some(position);
              }
            );

            // if there's no Goal after the cursor, then loop back and return the first Goal
            if (nextGoal^ === None) {
              nextGoal := positions[0];
            };

            switch (nextGoal^) {
            | None => ()
            | Some(point) => Editor.setCursorPosition(state.editor, point)
            };
            Promise.resolved([]);
          },
        ),
      ]
    | PreviousGoal => [
        Task.WithState(
          state => {
            let previousGoal = ref(None);
            let cursor = Editor.getCursorPosition(state.editor);

            let getPositions = (): array(Editor.Point.t) => {
              state.goals
              ->Array.map(goal => goal.range)
              ->Array.map(range =>
                  Editor.Point.translate(Editor.Range.start(range), 0, 3)
                );
            };
            let positions = getPositions();

            // find the last Goal before the cursor
            positions->Array.forEach(position =>
              if (Editor.Point.compare(cursor, position) === Editor.GT) {
                previousGoal := Some(position);
              }
            );

            // loop back if this is already the first Goal
            if (previousGoal^ === None) {
              previousGoal := positions[Array.length(positions) - 1];
            };

            switch (previousGoal^) {
            | None => ()
            | Some(point) => Editor.setCursorPosition(state.editor, point)
            };
            Promise.resolved([]);
          },
        ),
      ]
    | ViewResponse(response) => [ViewRes(response)];
};