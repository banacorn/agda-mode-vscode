module type Module = {
  type t
  let make: unit => t
  let instantiateGoalsFromLoad: (t, VSCode.TextEditor.t, array<int>, Map.t<int, int>) => unit

  let updatePositions: (t, VSCode.TextDocumentChangeEvent.t) => unit
}

module Module: Module = {
  module Goal = {
    type index = int
    type t = {
      index: index,
      start: int,
      end: int,
      decorationBackground: Editor.Decoration.t,
      decorationIndex: Editor.Decoration.t,
    }

    let toString = goal => {
      "#" ++
      string_of_int(goal.index) ++
      " [" ++
      string_of_int(goal.start) ++
      "-" ++
      string_of_int(goal.end) ++ ")"
    }
  }

  type t = {
    mutable goals: Map.t<Goal.index, Goal.t>, // goal index => goal
    mutable positions: AVLTree.t<Goal.index>, // start position => goal index
  }

  let make = () => {
    {
      goals: Map.make(),
      positions: AVLTree.make(),
    }
  }

  let instantiateGoalsFromLoad = (self, editor, indices, positions) => {
    positions
    ->Map.entries
    ->Iterator.toArray
    ->Array.forEachWithIndex(((start, end), i) => {
      switch indices[i] {
      | None => ()
      | Some(index) =>
        let (decorationBackground, decorationIndex) = Highlighting.decorateHole(
          editor,
          (start, end),
          index,
        )
        let goal = {
          Goal.index,
          start,
          end,
          decorationBackground,
          decorationIndex,
        }
        self.goals->Map.set(index, goal)
        self.positions->AVLTree.insert(start, index)
      }
    })
  }

  type action =
    | Destroy(Goal.index)
    | UpdatePosition(Goal.index, int) // delta

  let updatePositions = (self, event) => {
    let changes =
      event
      ->VSCode.TextDocumentChangeEvent.contentChanges
      ->Array.map(Tokens.Change.fromTextDocumentContentChangeEvent)
      ->Array.toReversed
      ->List.fromArray

    // there are 4 cases to consider when a change overlaps with a hole:

    // 1. the hole is completely after the change, skip the change and move on
    //      removal  ┣━━━━━┫
    //      hole              ┣━━━━━┫

    // 2. the hole is completely before the change, skip the hole and move on
    //      removal                    ┣━━━━━┫
    //      hole              ┣━━━━━┫

    // 3. the hole is completely destroyed, we regard this as the intention to remove the hole
    //      removal        ┣━━━━━━━━━━━┫
    //      hole              ┣━━━━━┫

    // 4. the hole is partially damaged, we should restore the hole afterwards
    //      removal        ┣━━━━━┫
    //      hole              ┣━━━━━┫
    //
    //      removal           ┣━━━━━┫
    //      hole           ┣━━━━━━━━━━━┫
    //
    //      removal              ┣━━━━━┫
    //      hole              ┣━━━━━┫
    let rec go = (delta, goals: list<Goal.t>, changes: list<Tokens.Change.t>) => {
      switch (goals, changes) {
      | (list{}, _) => list{} // no goals left
      | (list{goal, ...goals}, list{}) =>
        if delta != 0 {
          list{UpdatePosition(goal.index, delta), ...go(delta, goals, changes)}
        } else {
          go(delta, goals, changes)
        }
      | (list{goal, ...goals}, list{change, ...changes}) =>
        let removalStart = change.offset
        let removalEnd = change.offset + change.removed

        if removalEnd <= goal.start {
          // the hole is completely after the change
          let delta = delta + change.inserted - change.removed
          go(delta, list{goal, ...goals}, changes)
        } else if removalStart >= goal.end {
          // the hole is completely before the change
          go(delta, goals, list{change, ...changes})
        } else if goal.start >= removalStart && goal.end <= removalEnd {
          // the hole is completely destroyed
          let delta = delta + change.inserted - change.removed
          list{Destroy(goal.index), ...go(delta, goals, changes)}
        } else {
          // the hole is partially damaged
          let delta = delta + change.inserted - change.removed
          list{Destroy(goal.index), ...go(delta, goals, changes)}
        }
      }
    }

    // update the positions when there are changes
    if changes->List.length != 0 {
      // list of goals ordered by their start position
      let goals =
        self.positions
        ->AVLTree.toArray
        ->Array.filterMap(index => self.goals->Map.get(index))
        ->List.fromArray

      go(0, goals, changes)->List.forEach(action => {
        switch action {
        | Destroy(index) => Js.log2("Destroying goal: ", index)
        // self.goals->Map.delete(index)
        // self.positions->AVLTree.remove(index)
        | UpdatePosition(index, delta) =>
          Js.log(
            "Updating position for: #" ++
            string_of_int(index) ++
            " with delta: " ++
            string_of_int(delta),
          )
        }
        // self.goals->AVLTree.insert(goal.start, goal)
      })
    }
  }
}
include Module
