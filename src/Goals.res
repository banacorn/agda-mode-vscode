module type Module = {
  type t
  let make: unit => t
  let instantiateGoalsFromLoad: (
    t,
    VSCode.TextEditor.t,
    array<int>,
    Map.t<int, int>,
  ) => promise<unit>

  let updatePositions: (t, VSCode.TextEditor.t, VSCode.TextDocumentChangeEvent.t) => unit

  // for testing
  let getGoals: t => Map.t<int, (int, int)>
}

module Module: Module = {
  module Goal = {
    type index = int
    type t = {
      index: index,
      mutable start: int,
      mutable end: int,
      decorationBackground: Editor.Decoration.t,
      decorationIndex: Editor.Decoration.t,
    }

    let decorate = (editor: VSCode.TextEditor.t, start: int, end: int, index: int) => {
      let document = VSCode.TextEditor.document(editor)
      let backgroundRange = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, start),
        VSCode.TextDocument.positionAt(document, end),
      )

      let background = Editor.Decoration.highlightBackground(
        editor,
        "editor.selectionHighlightBackground",
        [backgroundRange],
      )
      let indexText = string_of_int(index)
      let indexRange = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, start),
        VSCode.TextDocument.positionAt(document, end - 2),
      )

      let index = Editor.Decoration.overlayText(
        editor,
        "editorLightBulb.foreground",
        indexText,
        indexRange,
      )

      (background, index)
    }

    let make = (editor: VSCode.TextEditor.t, start: int, end: int, index: index) => {
      let (decorationBackground, decorationIndex) = decorate(editor, start, end, index)
      {
        index,
        start,
        end,
        decorationBackground,
        decorationIndex,
      }
    }

    let destroy = goal => {
      goal.decorationBackground->Editor.Decoration.destroy
      goal.decorationIndex->Editor.Decoration.destroy
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

  // Destory and clear all goals
  let clear = self => {
    self.goals->Map.values->Iterator.toArray->Array.forEach(Goal.destroy)
    self.goals = Map.make()
    self.positions = AVLTree.make()
  }

  // Batch replace all goals of question marks to holes
  let expandQuestionMarkGoals = async (self, document) => {
    let rewrites =
      self.goals
      ->Map.values
      ->Iterator.toArray
      ->Array.filterMap(goal => {
        let range = VSCode.Range.make(
          VSCode.TextDocument.positionAt(document, goal.start),
          VSCode.TextDocument.positionAt(document, goal.end),
        )
        let content = VSCode.TextDocument.getText(document, Some(range))
        if content == "?" {
          Some((range, "{!   !}"))
        } else {
          None
        }
      })
    // execute the batch replacement of question marks to holes
    let _ = await Editor.Text.batchReplace(document, rewrites)
  }

  let updateGoalPosition = (
    self,
    editor,
    goal: Goal.t,
    deltaStart: int,
    deltaEnd: int,
    redecorate: bool,
  ) => {
    // update the position of the goal
    let newStart = goal.start + deltaStart
    let newEnd = goal.end + deltaEnd

    // remove the old goal from the positions tree
    self.positions->AVLTree.remove(goal.start)->ignore

    // update the goal's start and end positions

    let updatedGoal = if redecorate {
      Goal.destroy(goal)
      Goal.make(editor, newStart, newEnd, goal.index)
    } else {
      {...goal, start: newStart, end: newEnd}
    }

    Js.log(
      "Update " ++
      Goal.toString(goal) ++
      " => " ++
      Goal.toString(updatedGoal) ++
      " (deltaStart: " ++
      string_of_int(deltaStart) ++
      ", deltaEnd: " ++
      string_of_int(deltaEnd) ++ ")",
    )
    // add the updated goal back to the positions tree
    self.positions->AVLTree.insert(newStart, updatedGoal.index)

    // update the goal in the goals map
    self.goals->Map.set(updatedGoal.index, updatedGoal)
  }

  // Instantiate goals after the Load command is executed.
  // Also destoys all existing goals
  let instantiateGoalsFromLoad = async (self, editor, indices, positions) => {
    clear(self)

    positions
    ->Map.entries
    ->Iterator.toArray
    ->Array.forEachWithIndex(((start, end), i) => {
      switch indices[i] {
      | None => ()
      | Some(index) =>
        let goal = Goal.make(editor, start, end, index)
        self.goals->Map.set(index, goal)
        self.positions->AVLTree.insert(start, index)
      }
    })

    await expandQuestionMarkGoals(self, editor->VSCode.TextEditor.document)
  }

  type action =
    | Destroy(Goal.t)
    | UpdatePosition(Goal.t, int, int, bool) // goal, delta of start, delta of end, should the hole be redecorated?

  let updatePositions = (self, editor, event) => {
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
          list{UpdatePosition(goal, delta, delta, false), ...go(delta, goals, changes)}
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
          if goal.end - goal.start == 1 && change.removed == 1 && change.inserted == 7 {
            // the hole is being expanded from a question mark to a hole
            let delta = delta + change.inserted - change.removed
            let redecorate = true
            list{UpdatePosition(goal, delta - 6, delta, redecorate), ...go(delta, goals, changes)}
          } else {
            // the hole is completely destroyed
            let delta = delta + change.inserted - change.removed
            list{Destroy(goal), ...go(delta, goals, changes)}
          }
        } else {
          // the hole is partially damaged
          let delta = delta + change.inserted - change.removed
          list{Destroy(goal), ...go(delta, goals, changes)}
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
        | Destroy(goal) =>
          self.goals->Map.delete(goal.index)->ignore
          self.positions->AVLTree.remove(goal.start)->ignore
          Goal.destroy(goal) // destroy the goal's decorations
        | UpdatePosition(goal, deltaStart, deltaEnd, redecorate) =>
          updateGoalPosition(self, editor, goal, deltaStart, deltaEnd, redecorate)
        }
      })
    }

    Js.log(
      "Goals after update: " ++
      self.goals
      ->Map.values
      ->Iterator.toArray
      ->Array.map(Goal.toString)
      ->Array.join(", "),
    )
  }

  let getGoals = self =>
    self.goals
    ->Map.entries
    ->Iterator.toArray
    ->Array.map(((index, goal)) => (index, (goal.start, goal.end)))
    ->Map.fromArray
}
include Module
