module type Module = {
  type t
  type index = int

  let make: unit => t
  let size: t => int
  let resetGoalIndices: (t, VSCode.TextEditor.t, array<index>) => promise<unit>
  let addGoalPositions: (t, array<(int, int)>) => unit
  let parseGoalPositionsFromRefine: string => array<(int, int)>
  let destroyGoalByIndex: (t, index) => unit

  type action2 = UpdatePosition(int, int) | Rewrite(VSCode.Range.t, string) | Destroy

  let actionsFromChanges: (
    (int, int) => string,
    array<(int, int)>,
    array<Tokens.Change.t>,
  ) => array<action2>

  // scan all goals and update their positions after document changes
  let scanAllGoals: (t, VSCode.TextEditor.t, array<Tokens.Change.t>) => promise<unit>

  let getGoalByIndex: (t, index) => option<Goal2.t>

  let modify: (t, VSCode.TextDocument.t, index, string => string) => promise<unit>
  let removeBoundaryAndDestroy: (t, VSCode.TextDocument.t, index) => promise<bool>
  // get the goal at the cursor position
  let getGoalAtCursor: (t, VSCode.TextEditor.t) => option<Goal2.t>

  // jumping between goals
  let jmupToTheNextGoal: (t, VSCode.TextEditor.t) => unit
  let jmupToThePreviousGoal: (t, VSCode.TextEditor.t) => unit

  // semaphore for busy state
  let isBusy: t => bool
  let waitUntilNotBusy: t => promise<unit>

  // keep track of the last case split goal
  let markAsCaseSplited: (t, Goal2.t) => unit
  let getRecentlyCaseSplited: t => option<Goal2.t>

  // for testing
  let serialize: t => array<string>
}

module Module: Module = {
  type index = int

  // Internal representation of a goal in the editor
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

    let toString = goal => {
      "#" ++
      string_of_int(goal.index) ++
      " [" ++
      string_of_int(goal.start) ++
      "-" ++
      string_of_int(goal.end) ++ ")"
    }

    let makeInnerRange = (goal, document) =>
      VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, goal.start + 2),
        VSCode.TextDocument.positionAt(document, goal.end - 2),
      )

    let makeOuterRange = (goal, document) =>
      VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, goal.start),
        VSCode.TextDocument.positionAt(document, goal.end),
      )
  }

  // flag for controlling the restoration of damaged goals
  let restoreGoals = false

  type t = {
    mutable goals: Map.t<index, Goal.t>, // goal index => goal
    mutable goalsWithoutIndices: Map.t<int, int>, // mapping of start position => end position, goals without indices
    mutable positions: AVLTree.t<index>, // start position => goal index
    mutable isBusy: option<Resource.t<unit>>, // semaphore for busy state
    mutable recentlyCaseSplited: option<Goal2.t>, // keep track of the last case split goal, because it won't be available during the case split
    mutable goalPositionsFromRefine: array<int>, // positions of the goals from the refine result
  }

  let make = () => {
    {
      goals: Map.make(),
      goalsWithoutIndices: Map.make(),
      positions: AVLTree.make(),
      isBusy: None,
      recentlyCaseSplited: None,
      goalPositionsFromRefine: [],
    }
  }

  let size = self => Map.size(self.goals)

  let destroyGoal = (self, goal: Goal.t) => {
    // destroy the goal's decorations
    goal.decorationBackground->Editor.Decoration.destroy
    goal.decorationIndex->Editor.Decoration.destroy
    // remove the goal from the goals map
    self.goals->Map.delete(goal.index)->ignore
    // remove the goal from the positions tree
    self.positions->AVLTree.remove(goal.start)->ignore
  }

  let destroyGoalByIndex = (self, index) =>
    switch self.goals->Map.get(index) {
    | None => () // goal not found, do nothing
    | Some(goal) => destroyGoal(self, goal)
    }

  let isBusy = self => self.isBusy->Option.isSome
  let setBusy = self =>
    switch self.isBusy {
    | Some(_) => () // already busy, do nothing
    | None => self.isBusy = Some(Resource.make())
    }
  let setNotBusy = self =>
    switch self.isBusy {
    | None => () // not busy, do nothing
    | Some(resource) =>
      self.isBusy = None // clear the busy state
      resource->Resource.set() // resolve the promise
    }

  let waitUntilNotBusy = self =>
    switch self.isBusy {
    | None => Promise.resolve()
    | Some(resource) => resource->Resource.get
    }

  let serialize = self =>
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.toSorted((x, y) => Int.compare(x.index, y.index))
    ->Array.map(Goal.toString)

  let getInternalGoalByIndex = (self, index) => self.goals->Map.get(index)

  let read = (goal, document) => {
    let innerRange = Goal.makeInnerRange(goal, document)
    Editor.Text.get(document, innerRange)->String.trim
  }

  let getGoalByIndex = (self, index): option<Goal2.t> =>
    self.goals
    ->Map.get(index)
    ->Option.map(goal => {
      {
        Goal2.index: goal.index,
        indexString: Int.toString(goal.index),
        start: goal.start,
        end: goal.end,
      }
    })

  let modify = async (self, document, index, f) =>
    switch getInternalGoalByIndex(self, index) {
    | Some(goal) =>
      let innerRange = Goal.makeInnerRange(goal, document)
      let goalContent = read(goal, document)
      let _ = await Editor.Text.replace(document, innerRange, " " ++ f(goalContent) ++ " ")
    | None => ()
    }

  let removeBoundaryAndDestroy = async (self, document, index) =>
    switch getInternalGoalByIndex(self, index) {
    | None => true
    | Some(goal) =>
      let outerRange = Goal.makeOuterRange(goal, document)

      let content = read(goal, document)
      if await Editor.Text.replace(document, outerRange, content) {
        self->destroyGoal(goal)
        true
      } else {
        false
      }
    }

  let getGoalAtCursor = (self, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let cursorOffset = VSCode.TextDocument.offsetAt(document, Editor.Cursor.get(editor))

    self.positions
    ->AVLTree.lowerBound(cursorOffset)
    ->Option.flatMap(index => {
      switch self.goals->Map.get(index) {
      | None => None // no goal found
      | Some(goal) =>
        if cursorOffset >= goal.start && cursorOffset < goal.end {
          Some({
            Goal2.index: goal.index,
            indexString: Int.toString(goal.index),
            start: goal.start,
            end: goal.end,
          }) // return the index of the goal
        } else {
          None // no goal found at the cursor position
        }
      }
    })
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

    // add the updated goal back to the positions tree
    self.positions->AVLTree.insert(newStart, updatedGoal.index)

    // update the goal in the goals map
    self.goals->Map.set(updatedGoal.index, updatedGoal)
  }

  type action2 = UpdatePosition(int, int) | Rewrite(VSCode.Range.t, string) | Destroy

  let actionsFromChanges = (getText, goals, changes) => {
    []
  }

  type action =
    | Rewrite(VSCode.Range.t, string) // range, text to replace
    | Destroy(Goal.t)
    // | Restore(Goal.t, damage)
    | UpdatePosition(Goal.t, int, int, bool) // goal, delta of start, delta of end, should the goal be redecorated?

  let scanAllGoals = async (self, editor, changes) => {
    let document = VSCode.TextEditor.document(editor)
    let changes = changes->List.fromArray

    // there are 4 cases to consider when a change overlaps with a goal:

    // 1. the goal is after the change, skip the change and move on
    //      removal  ┣━━━━━┫
    //      goal              ┣━━━━━┫

    // 2. the goal is before the change, skip the goal and move on
    //      removal                    ┣━━━━━┫
    //      goal              ┣━━━━━┫

    // 3. the goal is completely destroyed, we regard this as the intention to remove the goal
    //      removal        ┣━━━━━━━━━━━┫
    //      goal              ┣━━━━━┫

    // 4. the goal is partially damaged, we should restore the goal afterwards
    //      removal        ┣━━━━━┫
    //      goal              ┣━━━━━┫
    //
    //      removal           ┣━━━━━┫
    //      goal           ┣━━━━━━━━━━━┫
    //
    //      removal              ┣━━━━━┫
    //      goal              ┣━━━━━┫

    // Given a goal, see if the document text of the range of the goal still constitutes a goal.
    // If it's still a goal like "{!   !}", we do nothing or update its position (in case that there's a change before it).
    // It the goal is a question mark like "?", we expand it to a goal "{!   !}".
    // If the goal is completely destroyed, we remove it.
    // If the goal is partially damaged, we restore it to a goal with the same content as before.
    let scanGoal = (delta, goal: Goal.t, deltaStart, deltaEnd, destroyed: bool) => {
      let range = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, goal.start + delta + deltaStart),
        VSCode.TextDocument.positionAt(document, goal.end + delta + deltaEnd),
      )
      let goalText = Editor.Text.get(document, range)
      let goalTextLength = goal.end - goal.start + deltaEnd - deltaStart
      // the goal may be replaced with some text like "  {!       !} ", in that case, we'll need to update the positions
      let leftBoundary = String.indexOfOpt(goalText, "{!")
      let rightBoundary = String.lastIndexOfOpt(goalText, "!}")
      let goalIsIntact = rightBoundary->Option.isSome && leftBoundary->Option.isSome

      // a goal will completely destroyed during the expansion from a question mark to a goal
      // to prevent this action from being interpreted as a goal removal, we check if the goal is indeed a question mark expansion by checking if it was a question mark before the change
      let isQuestionMarkExpansion = destroyed && goal.start + 1 == goal.end

      let (resizeStart, resizeEnd) = switch (leftBoundary, rightBoundary) {
      | (Some(leftIndex), Some(rightIndex)) =>
        if leftIndex == 0 && rightIndex + 2 == String.length(goalText) {
          (0, 0) // no resizing needed
        } else {
          (-leftIndex, rightIndex + 2 - String.length(goalText))
        }
      | _ => (0, 0)
      }

      if goalText == "?" {
        [Rewrite(range, "{!   !}")]
      } // expand question mark to goal
      else if destroyed && !isQuestionMarkExpansion {
        [Destroy(goal)]
      } else if goalIsIntact {
        [
          UpdatePosition(
            goal,
            delta + deltaStart + resizeStart,
            delta + deltaEnd + resizeEnd,
            isQuestionMarkExpansion, // redecorate if it was a question mark expansion
          ),
        ]
      } else if restoreGoals && goalText->String.startsWith("{!") {
        switch goalText->String.charAt(goalTextLength - 1) {
        | "!" => [
            UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
            Rewrite(range, goalText ++ "}"),
          ]
        | "}" => [
            UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
            Rewrite(range, goalText->String.substring(~start=0, ~end=goalTextLength - 1) ++ "!}"),
          ]
        | _ => [UpdatePosition(goal, delta + deltaStart, delta + deltaEnd, false)]
        }
      } else if restoreGoals && goalText->String.endsWith("!}") {
        switch goalText->String.charAt(0) {
        | "{" => [
            UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
            Rewrite(range, "{!" ++ goalText->String.substringToEnd(~start=1)),
          ]

        | "!" => [
            UpdatePosition(goal, delta + deltaStart, delta + deltaEnd + 1, false),
            Rewrite(range, "{" ++ goalText),
          ]

        | _ => [UpdatePosition(goal, delta + deltaStart, delta + deltaEnd, false)]
        }
      } else {
        [UpdatePosition(goal, delta + deltaStart, delta + deltaEnd, false)]
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
        if removalEnd < goal.start {
          // the goal is completely after the change (separated by a least a character)
          //      removal  ┣━━━━━┫
          //      goal              ┣━━━━━┫
          let delta = delta + change.inserted - change.removed
          go(delta, list{goal, ...goals}, changes)
        } else if removalStart >= goal.end {
          // the goal is completely before the change, skip the goal
          //      removal                    ┣━━━━━┫
          //      goal              ┣━━━━━┫
          go(delta, goals, list{change, ...changes})
        } else if goal.start >= removalStart && goal.end <= removalEnd {
          //      removal        ┣━━━━━━━━━━━┫
          //      goal              ┣━━━━━┫
          let deltaStart = removalStart - goal.start
          let deltaEnd = deltaStart + change.inserted - change.removed
          let actions = scanGoal(delta, goal, deltaStart, deltaEnd, true)
          let delta' = delta + change.inserted - change.removed
          [...actions, ...go(delta', goals, changes)]
        } else {
          // if removalStart < goal.start {
          //   //      removal        ┣━━━━━┫
          //   //      goal              ┣━━━━━┫

          //   let deltaStart = removalStart - goal.start
          //   let deltaEnd = change.inserted - change.removed
          //   Js.log("restore 1")

          //   let actions = scanGoal(delta, goal, deltaStart, deltaEnd, false)
          //   let delta' = delta + change.inserted - change.removed

          //   [...actions, ...go(delta', list{goal, ...goals}, changes)]
          // } else if removalEnd <= goal.end {
          //   //      removal           ┣━━━━━┫
          //   //      goal           ┣━━━━━━━━━━━┫
          //   let deltaStart = 0
          //   let deltaEnd = change.inserted - change.removed
          //   Js.log("restore 2")

          //   let actions = scanGoal(delta, goal, deltaStart, deltaEnd, false)
          //   let delta' = delta + change.inserted - change.removed

          //   [...actions, ...go(delta', list{goal, ...goals}, changes)]
          // } else {
          //   //      removal              ┣━━━━━┫
          //   //      goal              ┣━━━━━┫
          //   let deltaStart = 0
          //   let deltaEnd = removalStart - goal.end + change.inserted // ?
          //   Js.log("restore 3")

          //   let actions = scanGoal(delta, goal, deltaStart, deltaEnd, false)
          //   let delta' = delta + change.inserted - change.removed

          //   [...actions, ...go(delta', goals, changes)]
          // }

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

    if Array.length(rewrites) != 0 {
      // set busy
      setBusy(self)
      let _ = await Editor.Text.batchReplace(document, rewrites)
    } else {
      setNotBusy(self)
    }
  }

  // Set indices of goals from `Responses.InteractionPoints` from commands like Load or Refine
  // The indices are ordered by the start position of the goals.
  let resetGoalIndices = async (self, editor, indices) => {
    clear(self)

    self.goalsWithoutIndices
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

    self.goalsWithoutIndices = Map.make() // clear the goals without indices

    await scanAllGoals(self, editor, [])
  }

  // Add goal positions without indices, e.g. from the Load or Refine command
  let addGoalPositions = (self, positions) => {
    positions->Array.forEach(((start, end)) => {
      self.goalsWithoutIndices->Map.set(start, end)
    })
  }

  // New holes may be introduced by a refine command, however, we don't have highlighting information
  // for the result of the refine command.
  // So we need to parse the holes from the refine result and decorate them ourself.
  // This function calculates the offsets of the question marks
  let parseGoalPositionsFromRefine = raw => {
    let goalQuestionMark = %re("/([\s\(\{\_\;\.\\\"@]|^)(\?)([\s\)\}\_\;\.\\\"@]|$)/gm")
    // the chunks may contain:
    //   1. the question mark itself              (\?)
    //   2. the part before the question mark     ([\s\(\{\_\;\.\\\"@]|^)
    //   3. the part after the question mark      ([\s\)\}\_\;\.\\\"@]|$)
    //   4. other strings not matching the regex
    let chunks = raw->String.splitByRegExp(goalQuestionMark)

    chunks
    ->Array.reduce(([], 0), ((offsets, i), chunk) =>
      switch chunk {
      | None => (offsets, i)
      | Some(chunk) =>
        if chunk == "?" {
          let offset = i
          ([...offsets, (offset, offset + 1)], offset + 1)
        } else {
          // not a question mark, just append it to the string
          (offsets, i + String.length(chunk))
        }
      }
    )
    ->fst
  }

  // Instantiate goals after the Refine command is executed.
  // However, we won't have the positions and indices of the goals together at once like in `setGoalIndices`:
  //  * positions: determined by the question marks in the refine result
  //  * indices: supplied from `InteractionPoints` later

  // let supplyGoalPositionsFromRefine = (self, refinement) => {
  //   self.goalPositionsFromRefine = parseHolePositionsFromRefine(refinement)
  // }

  // let instantiateGoalsFromRefine = async (self, editor, indices) => {
  //   clear(self)

  //   self.goalPositionsFromRefine->Array.forEachWithIndex((start, i) => {
  //     switch indices[i] {
  //     | None => ()
  //     | Some(index) =>
  //       let goal = Goal.make(editor, start, start + 1, index)
  //       self.goals->Map.set(index, goal)
  //       self.positions->AVLTree.insert(start, index)
  //     }
  //   })
  //   self.goalPositionsFromRefine = [] // clear the positions after instantiation

  //   await scanAllGoals(self, editor, [])
  // }

  let jmupToGoal = (editor, goal: Goal.t) => {
    let document = VSCode.TextEditor.document(editor)
    let spaceInsideBoundaries = goal.end - goal.start - 4
    let offset = if spaceInsideBoundaries == 0 {
      // {!!}
      goal.start + 2
    } else {
      // {! !}
      goal.start + 3
    }

    let position = VSCode.TextDocument.positionAt(document, offset)
    Editor.Cursor.set(editor, position)
  }

  let jmupToTheNextGoal = (self, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let cursorOffset = VSCode.TextDocument.offsetAt(document, Editor.Cursor.get(editor))

    // find the first Goal after the cursor
    let goal = switch self.positions->AVLTree.upperBound(cursorOffset) {
    | None => self.goals->Map.get(0) // if no goal is found, maybe we are at the end of the document, try to return the first goal
    | Some(index) => self.goals->Map.get(index)
    }

    switch goal {
    | None => () // no goal found, do nothing
    | Some(goal) => jmupToGoal(editor, goal)
    }
  }

  let jmupToThePreviousGoal = (self, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let cursorOffset = VSCode.TextDocument.offsetAt(document, Editor.Cursor.get(editor))

    // find the first Goal after the cursor
    let goal = switch self.positions->AVLTree.lowerBound(cursorOffset) {
    | None => self.goals->Map.get(Map.size(self.goals) - 1) // if no goal is found, maybe we are at the beginning of the document, try to return the last goal
    | Some(index) =>
      // if we are already in a goal, finding the lower bound will return the goal itself, we need to return the predecessor instead
      switch self.goals->Map.get(index) {
      | None => None // no goal found, do nothing
      | Some(goal) =>
        if cursorOffset >= goal.start && cursorOffset < goal.end {
          let previousIndex = if index == 0 {
            Map.size(self.goals) - 1 // wrap around to the last goal
          } else {
            index - 1 // get the previous goal
          }
          self.goals->Map.get(previousIndex) // return the previous goal
        } else {
          Some(goal) // return the current goal
        }
      }
    }

    switch goal {
    | None => () // no goal found, do nothing
    | Some(goal) => jmupToGoal(editor, goal)
    }
  }

  let markAsCaseSplited = (self, goal) => {
    self.recentlyCaseSplited = Some(goal)
  }

  let getRecentlyCaseSplited = self =>
    self.recentlyCaseSplited->Option.map(goal => {
      {
        Goal2.index: goal.index,
        indexString: Int.toString(goal.index),
        start: goal.start,
        end: goal.end,
      }
    })
}
include Module
