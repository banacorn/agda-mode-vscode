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
            let getNextGoalPosition = (): option(Editor.Point.t) => {
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
              // assign the next goal position
              positions->Array.forEach(position =>
                if (Editor.Point.compare(cursor, position) === Editor.LT
                    && nextGoal^ === None) {
                  nextGoal := Some(position);
                }
              );

              /* if no goal ahead of cursor, then loop back */
              if (nextGoal^ === None) {
                nextGoal := positions[0];
              };

              nextGoal^;
            };

            switch (getNextGoalPosition()) {
            | None => ()
            | Some(point) => Editor.setCursorPosition(state.editor, point)
            };
            Promise.resolved([]);
          },
        ),
      ]
    | PreviousGoal => []
    | ViewResponse(response) => [ViewRes(response)];
};