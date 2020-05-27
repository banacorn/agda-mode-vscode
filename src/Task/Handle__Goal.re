open Belt;

module Impl = (Editor: Sig.Editor) => {
  module Task = Task.Impl(Editor);
  module State = State.Impl(Editor);
  module Goal = Goal.Impl(Editor);

  open! Task;

  // return an array of Positions of Goals
  let getPositions = (state: State.t): array(Editor.Point.t) => {
    state.goals
    ->Array.map(goal => goal.range)
    ->Array.map(range =>
        Editor.Point.translate(Editor.Range.start(range), 0, 3)
      );
  };

  // from Goal-related action to Tasks
  let handle =
    fun
    | Instantiate(indices) => [
        WithState(
          state => {
            // destroy all existing goals
            state.goals->Array.forEach(Goal.destroy);
            // instantiate new ones
            Goal.makeMany(state.editor, indices)
            ->Promise.map(goals => {
                state.goals = goals;
                [];
              });
          },
        ),
      ]
    | Next => [
        Task.WithState(
          state => {
            let nextGoal = ref(None);
            let cursor = Editor.getCursorPosition(state.editor);

            let positions = getPositions(state);

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
    | Previous => [
        Task.WithState(
          state => {
            let previousGoal = ref(None);
            let cursor = Editor.getCursorPosition(state.editor);

            let positions = getPositions(state);

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
      ];
};