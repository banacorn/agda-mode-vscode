module type Module = {
  type t
  let make: unit => t
  let instantiateGoalsFromLoad: (
    t,
    VSCode.TextEditor.t,
    array<int>,
    Map.t<int, int>,
  ) => promise<unit>

  let scanAllGoals: (t, VSCode.TextEditor.t, array<Tokens.Change.t>) => promise<unit>

  let isBusy: t => bool
  let waitUntilNotBusy: t => promise<unit>

  // for testing
  let serialize: t => array<string>
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

    // let destroy = goal => {
    //   goal.decorationBackground->Editor.Decoration.destroy
    //   goal.decorationIndex->Editor.Decoration.destroy
    // }

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
    mutable isBusy: option<(promise<unit>, unit => unit)>, // semaphore for busy state
  }

  let make = () => {
    {
      goals: Map.make(),
      positions: AVLTree.make(),
      isBusy: None,
    }
  }

  let isBusy = self => self.isBusy->Option.isSome
  let setBusy = self =>
    switch self.isBusy {
    | Some(_) => () // already busy, do nothing
    | None =>
      let (promise, resolve, _) = Util.Promise_.pending()
      self.isBusy = Some(promise, resolve)
    }
  let setNotBusy = self =>
    switch self.isBusy {
    | None => () // not busy, do nothing
    | Some((_, resolve)) =>
      self.isBusy = None // clear the busy state
      resolve() // resolve the promise
    }

  let waitUntilNotBusy = self =>
    switch self.isBusy {
    | None => Promise.resolve()
    | Some((promise, _)) => promise
    }

  let serialize = self =>
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.toSorted((x, y) => Int.compare(x.index, y.index))
    ->Array.map(Goal.toString)

  let destroyGoal = (self, goal: Goal.t) => {
    // destroy the goal's decorations
    goal.decorationBackground->Editor.Decoration.destroy
    goal.decorationIndex->Editor.Decoration.destroy
    // remove the goal from the goals map
    self.goals->Map.delete(goal.index)->ignore
    // remove the goal from the positions tree
    self.positions->AVLTree.remove(goal.start)->ignore
  }

  // Destory and clear all goals
  let clear = self => {
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.forEach(goal => {
      goal.decorationBackground->Editor.Decoration.destroy
      goal.decorationIndex->Editor.Decoration.destroy
    })
    self.goals = Map.make()
    self.positions = AVLTree.make()
  }

  // // Batch replace all goals of question marks to holes
  // let expandQuestionMarkGoals = async (self, document) => {
  //   let rewrites =
  //     self.goals
  //     ->Map.values
  //     ->Iterator.toArray
  //     ->Array.filterMap(goal => {
  //       let range = VSCode.Range.make(
  //         VSCode.TextDocument.positionAt(document, goal.start),
  //         VSCode.TextDocument.positionAt(document, goal.end),
  //       )
  //       let content = VSCode.TextDocument.getText(document, Some(range))
  //       if content == "?" {
  //         Some((range, "{!   !}"))
  //       } else {
  //         None
  //       }
  //     })
  //   // execute the batch replacement of question marks to holes
  //   let _ = await Editor.Text.batchReplace(document, rewrites)
  // }

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
      destroyGoal(self, goal)
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

  // Damage done to a hole. The letters correspond to the boundaries of the hole like this:
  //    {!   !}
  //    AB   CD
  // So for example, if "!   !}" is damaged, we represent it as "BCD"
  // type damage =
  //   | A(int) // "{" deleted, how many characters before "{" were deleted
  //   | AB
  //   | ABC
  //   | B
  //   | BC
  //   | BCD
  //   | C
  //   | CD
  //   | D

  type action =
    | Rewrite(VSCode.Range.t, string) // range, text to replace
    | Destroy(Goal.t)
    // | Restore(Goal.t, damage)
    | UpdatePosition(Goal.t, int, int, bool) // goal, delta of start, delta of end, should the hole be redecorated?

  let scanAllGoals = async (self, editor, changes) => {
    Js.log(
      " ======= Update positions ======= " ++ if self->isBusy {
        "BUSY!"
      } else {
        ""
      },
    )
    let document = VSCode.TextEditor.document(editor)
    let changes = changes->List.fromArray

    // there are 4 cases to consider when a change overlaps with a hole:

    // 1. the hole is after the change, skip the change and move on
    //      removal  ┣━━━━━┫
    //      hole              ┣━━━━━┫

    // 2. the hole is before the change, skip the hole and move on
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

    // Given a goal, see if the document text of the range of the goal still constitutes a hole.
    // If it's still a hole like "{!   !}", we do nothing or update its position (in case that there's a change before it).
    // It the hole is a question mark like "?", we expand it to a hole "{!   !}".
    // If the hole is completely destroyed, we remove it.
    // If the hole is partially damaged, we restore it to a hole with the same content as before.
    let scanGoal = (delta, goal: Goal.t, deltaStart, deltaEnd, destroyed: bool) => {
      let range = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, goal.start + delta + deltaStart),
        VSCode.TextDocument.positionAt(document, goal.end + delta + deltaEnd),
      )
      let holeText = Editor.Text.get(document, range)
      let holeTextLength = goal.end - goal.start + deltaEnd - deltaStart
      // the hole may be replaced with some text like "  {!       !} ", in that case, we'll need to update the positions
      let leftBoundary = String.indexOfOpt(holeText, "{!")
      let rightBoundary = String.lastIndexOfOpt(holeText, "!}")

      let holeIsIntact = rightBoundary->Option.isSome && leftBoundary->Option.isSome

      // a goal will completely destroyed during the expansion from a question mark to a hole
      // to prevent this action from being interpreted as a goal removal, we check if the goal is indeed a question mark expansion by checking if it was a question mark before the change
      let isQuestionMarkExpansion = destroyed && goal.start + 1 == goal.end

      let (resizeStart, resizeEnd) = switch (leftBoundary, rightBoundary) {
      | (Some(leftIndex), Some(rightIndex)) =>
        if leftIndex == 0 && rightIndex + 2 == String.length(holeText) {
          (0, 0) // no resizing needed
        } else {
          (-leftIndex, rightIndex + 2 - String.length(holeText))
        }
      | _ => (0, 0)
      }

      Js.log(
        "  delta: " ++
        string_of_int(delta) ++
        ", deltaStart: " ++
        string_of_int(deltaStart) ++
        ", deltaEnd: " ++
        string_of_int(deltaEnd) ++
        ", destroyed: " ++
        string_of_bool(destroyed) ++
        ", intact: " ++
        string_of_bool(holeIsIntact) ++
        ", resizing: " ++
        string_of_int(resizeStart) ++
        "/" ++
        string_of_int(resizeEnd) ++
        ", goal: " ++
        Goal.toString(goal) ++
        "  \t\"" ++
        holeText ++ "\"",
      )

      if holeText == "?" {
        [Rewrite(range, "{!   !}")]
      } // expand question mark to hole
      else if destroyed && !isQuestionMarkExpansion {
        Js.log("DESTROY: " ++ Goal.toString(goal))
        [Destroy(goal)]
      } else if holeIsIntact {
        [
          UpdatePosition(
            goal,
            delta + deltaStart + resizeStart,
            delta + deltaEnd + resizeEnd,
            false,
          ),
        ]
      } else {
        Js.log("\nDAMAGED: " ++ Goal.toString(goal) ++ "\n")

        if holeText->String.startsWith("{!") {
          switch holeText->String.charAt(holeTextLength - 1) {
          | "!" => [
              UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
              Rewrite(range, holeText ++ "}"),
            ]

          | "}" => [
              UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
              Rewrite(range, holeText->String.substring(~start=0, ~end=holeTextLength - 1) ++ "!}"),
            ]

          | _ => [UpdatePosition(goal, delta + deltaStart, delta + deltaEnd, false)]
          }
        } else if holeText->String.endsWith("!}") {
          switch holeText->String.charAt(0) {
          | "{" => [
              UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
              Rewrite(range, "{!" ++ holeText->String.substringToEnd(~start=1)),
            ]

          | "!" =>
            Js.log("HOLE TEXT: \"" ++ holeText ++ "\"")

            [
              UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
              Rewrite(range, "{" ++ holeText),
            ]

          | _ => [UpdatePosition(goal, delta + deltaStart, delta + deltaEnd, false)]
          }
        } else {
          [UpdatePosition(goal, delta + deltaStart, delta + deltaEnd, false)]
        }
      }
    }

    let rec go = (delta, goals: list<Goal.t>, changes: list<Tokens.Change.t>) => {
      switch (goals, changes) {
      | (list{}, _) => [] // no goals left
      | (list{goal, ...goals}, list{}) =>
        let actions = scanGoal(delta, goal, 0, 0, false)
        [...actions, ...go(delta, goals, changes)]
      | (list{goal, ...goals}, list{change, ...changes}) =>
        let removalStart = change.offset
        let removalEnd = change.offset + change.removed
        if removalEnd <= goal.start {
          // the hole is completely after the change
          let delta = delta + change.inserted - change.removed
          go(delta, list{goal, ...goals}, changes)
        } else if removalStart >= goal.end {
          // the hole is completely before the change, skip the hole
          go(delta, goals, list{change, ...changes})
        } else if goal.start >= removalStart && goal.end <= removalEnd {
          let deltaStart = removalStart - goal.start
          let deltaEnd = deltaStart + change.inserted - change.removed
          let actions = scanGoal(delta, goal, deltaStart, deltaEnd, true)
          let delta' = delta + change.inserted - change.removed
          [...actions, ...go(delta', goals, changes)]
        } else {
          Js.log("partially damaged goal: " ++ Goal.toString(goal))
          let deltaStart = if removalStart <= goal.start {
            removalStart - goal.start
          } else {
            0
          }
          let deltaEnd = if removalEnd >= goal.end {
            removalStart - goal.end + change.inserted
          } else {
            change.inserted - change.removed
          }

          let actions = scanGoal(delta, goal, deltaStart, deltaEnd, false)
          let delta' = delta + change.inserted - change.removed

          [...actions, ...go(delta', goals, changes)]
        }
      }
    }

    // update the positions when there are changes
    // list of goals ordered by their start position
    let goals =
      self.positions
      ->AVLTree.toArray
      ->Array.filterMap(index => self.goals->Map.get(index))
      ->List.fromArray

    let rewrites = go(0, goals, changes)->Array.filterMap(action => {
      switch action {
      | Destroy(goal) =>
        destroyGoal(self, goal)
        None // no rewrite needed
      | Rewrite(range, text) => Some((range, text))
      | UpdatePosition(goal, deltaStart, deltaEnd, redecorate) =>
        updateGoalPosition(self, editor, goal, deltaStart, deltaEnd, redecorate)
        None
      }
    })

    Js.log(
      "Goals after update: " ++
      self
      ->serialize
      ->Util.Pretty.array,
    )

    Js.log(
      "rewrites: " ++
      rewrites
      ->Array.map(((range, text)) =>
        Editor.Range.toString(range) ++
        " " ++
        Int.toString(VSCode.TextDocument.offsetAt(document, VSCode.Range.start(range))) ++
        " => " ++
        text
      )
      ->Util.Pretty.array,
    )

    if Array.length(rewrites) != 0 {
      // set busy
      setBusy(self)
      let _ = await Editor.Text.batchReplace(document, rewrites)
    } else {
      setNotBusy(self)
    }
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

    Js.log("triggered by instantiateGoalsFromLoad")
    await scanAllGoals(self, editor, [])
    // await expandQuestionMarkGoals(self, editor->VSCode.TextEditor.document)
  }
}
include Module
