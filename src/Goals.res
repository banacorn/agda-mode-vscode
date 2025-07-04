module type Module = {
  type t
  type index = int

  let make: unit => t
  let redecorate: t => unit
  let destroy: t => unit
  let size: t => int
  let resetGoalIndices: (t, VSCode.TextEditor.t, array<index>) => promise<unit>
  let resetGoalIndicesNew: (t, VSCode.TextEditor.t, array<index>) => promise<unit>
  let addGoalPositions: (t, array<(int, int)>) => unit
  let getGoalPositionByIndex: (t, index) => option<(int, int)>
  let parseGoalPositionsFromRefine: string => array<(int, int)>
  let removeGoalByIndex: (t, index) => unit

  // scan all goals and update their positions after document changes
  let scanAllGoals: (t, VSCode.TextEditor.t, array<Tokens.Change.t>) => promise<unit>

  let getGoalByIndex: (t, index) => option<Goal.t>

  let modify: (t, VSCode.TextDocument.t, index, string => string) => promise<unit>
  let removeBoundaryAndDestroy: (t, VSCode.TextDocument.t, index) => promise<bool>
  // get the goal at the cursor position
  let getGoalAtCursor: (t, VSCode.TextEditor.t) => option<Goal.t>
  let setCursorByIndex: (t, VSCode.TextEditor.t, int) => unit

  // jumping between goals
  let jmupToTheNextGoal: (t, VSCode.TextEditor.t) => unit
  let jmupToThePreviousGoal: (t, VSCode.TextEditor.t) => unit

  // semaphore for busy state
  let isBusy: t => bool
  let waitUntilNotBusy: t => promise<unit>

  // keep track of the last case split goal
  let markAsCaseSplited: (t, Goal.t) => unit
  let getRecentlyCaseSplited: t => option<Goal.t>

  // for testing
  let serialize: t => array<string>
  let toString: t => string
}

module Module: Module = {
  type index = int

  // Internal representation of a goal in the editor
  // TODO: merge this with State.t below
  module InternalGoal = {
    type index = int
    type t = {
      index: index,
      start: int,
      end: int,
      mutable decoration: option<(Editor.Decoration.t, Editor.Decoration.t)>, // (background, index)
    }

    let decorate = (start: int, end: int, index: int) =>
      VSCode.Window.activeTextEditor->Option.map(editor => {
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
          VSCode.TextDocument.positionAt(document, end - 2),
          VSCode.TextDocument.positionAt(document, end - 2),
        )

        let index = Editor.Decoration.overlayText(
          editor,
          "editorLightBulb.foreground",
          indexText,
          indexRange,
        )

        (background, index)
      })

    let undecorate = (goal: t) => {
      // destroy the goal's decorations
      goal.decoration->Option.forEach(((background, index)) => {
        background->Editor.Decoration.destroy
        index->Editor.Decoration.destroy
      })
    }

    let make = (start: int, end: int, index: index) => {
      let isQuestionMark = start + 1 == end
      {
        index,
        start,
        end,
        decoration: if isQuestionMark {
          None // no decoration for question mark goals
        } else {
          decorate(start, end, index)
        },
      }
    }

    let toString = goal =>
      switch VSCode.Window.activeTextEditor {
      | Some(editor) =>
        let document = VSCode.TextEditor.document(editor)
        let startPoint = VSCode.TextDocument.positionAt(document, goal.start)
        let endPoint = VSCode.TextDocument.positionAt(document, goal.end)
        let startLine = VSCode.Position.line(startPoint) + 1
        let startColumn = VSCode.Position.character(startPoint) + 1
        let endLine = VSCode.Position.line(endPoint) + 1
        let endColumn = VSCode.Position.character(endPoint) + 1
        if startLine == endLine {
          `#${string_of_int(goal.index)} [${string_of_int(startLine)}:${string_of_int(
              startColumn,
            )}-${string_of_int(endColumn)})`
        } else {
          `#${string_of_int(goal.index)} [${string_of_int(startLine)}:${string_of_int(
              startColumn,
            )}-${string_of_int(endLine)}:${string_of_int(endColumn)})`
        }
      | None =>
        `#${string_of_int(goal.index)} offset [${string_of_int(goal.start)}-${string_of_int(
            goal.end,
          )})`
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

  type t = {
    mutable goals: Map.t<index, InternalGoal.t>, // goal index => goal
    mutable goalsWithoutIndices: Map.t<int, int>, // mapping of start position => end position, goals without indices
    mutable positions: AVLTree.t<index>, // start position => goal index
    mutable isBusy: option<Resource.t<unit>>, // semaphore for busy state
    mutable recentlyCaseSplited: option<Goal.t>, // keep track of the last case split goal, because it won't be available during the case split
  }

  let make = () => {
    {
      goals: Map.make(),
      goalsWithoutIndices: Map.make(),
      positions: AVLTree.make(),
      isBusy: None,
      recentlyCaseSplited: None,
    }
  }

  let insertGoal = (self, start, end, index) => {
    let goal = InternalGoal.make(start, end, index)
    self.goals->Map.set(index, goal)
    self.positions->AVLTree.insert(start, index)
  }

  let removeGoal = (self, goal: InternalGoal.t) => {
    // destroy the goal's decorations
    InternalGoal.undecorate(goal)
    // remove the goal from the goals map
    self.goals->Map.delete(goal.index)->ignore
    // remove the goal from the positions tree
    self.positions->AVLTree.remove(goal.start)->ignore
  }

  let redecorate = self =>
    VSCode.Window.activeTextEditor->Option.forEach(editor => {
      self.goals
      ->Map.values
      ->Iterator.toArray
      ->Array.forEach(goal => {
        // re-decorate the editor with the existing decorations
        goal.decoration->Option.forEach(
          ((background, index)) => {
            let document = VSCode.TextEditor.document(editor)
            let backgroundRange = VSCode.Range.make(
              VSCode.TextDocument.positionAt(document, goal.start),
              VSCode.TextDocument.positionAt(document, goal.end),
            )

            let indexRange = VSCode.Range.make(
              VSCode.TextDocument.positionAt(document, goal.end - 2),
              VSCode.TextDocument.positionAt(document, goal.end - 2),
            )
            Editor.Decoration.decorate(editor, background, [backgroundRange])
            Editor.Decoration.decorate(editor, index, [indexRange])
          },
        )
      })
    })

  let destroy = self => {
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.forEach(goal => removeGoal(self, goal))
    self.goals = Map.make()
    self.positions = AVLTree.make()
  }

  let size = self => Map.size(self.goals)

  let removeGoalByIndex = (self, index) =>
    switch self.goals->Map.get(index) {
    | None => () // goal not found, do nothing
    | Some(goal) => removeGoal(self, goal)
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
    ->Array.map(InternalGoal.toString)

  let toString = self =>
    self
    ->serialize
    ->Array.join("\n")

  let getInternalGoalByIndex = (self, index) => self.goals->Map.get(index)

  let read = (goal, document) => {
    let innerRange = InternalGoal.makeInnerRange(goal, document)
    Editor.Text.get(document, innerRange)->String.trim
  }

  let getGoalByIndex = (self, index): option<Goal.t> =>
    self.goals
    ->Map.get(index)
    ->Option.map(goal => {
      {
        Goal.index: goal.index,
        indexString: Int.toString(goal.index),
        start: goal.start,
        end: goal.end,
      }
    })

  let modify = async (self, document, index, f) =>
    switch getInternalGoalByIndex(self, index) {
    | Some(goal) =>
      let innerRange = InternalGoal.makeInnerRange(goal, document)
      let goalContent = read(goal, document)
      let _ = await Editor.Text.replace(document, innerRange, " " ++ f(goalContent) ++ " ")
    | None => ()
    }

  let removeBoundaryAndDestroy = async (self, document, index) =>
    switch getInternalGoalByIndex(self, index) {
    | None => true
    | Some(goal) =>
      let outerRange = InternalGoal.makeOuterRange(goal, document)

      let content = read(goal, document)
      if await Editor.Text.replace(document, outerRange, content) {
        self->removeGoal(goal)
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
        if cursorOffset >= goal.start && cursorOffset <= goal.end {
          Some({
            Goal.index: goal.index,
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

  let setCursorByIndex = (self, editor, index) =>
    switch getInternalGoalByIndex(self, index) {
    | None => () // goal not found, do nothing
    | Some(goal) =>
      let document = VSCode.TextEditor.document(editor)
      let position = VSCode.TextDocument.positionAt(document, goal.start + 3)
      Editor.Cursor.set(editor, position)
      // scroll to that part of the document
      let range = InternalGoal.makeOuterRange(goal, document)
      editor->VSCode.TextEditor.revealRange(range, None)
    }

  // Destory and clear all goals
  let clear = self => {
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.forEach(InternalGoal.undecorate)
    self.goals = Map.make()
    self.positions = AVLTree.make()
  }

  let updateGoalPosition = (self, goal: InternalGoal.t, deltaStart: int, deltaEnd: int) => {
    // update the position of the goal
    let newStart = goal.start + deltaStart
    let newEnd = goal.end + deltaEnd

    // remove the old goal from the positions tree
    self.positions->AVLTree.remove(goal.start)->ignore

    // decorate the goal in case that it has been expanded from a question mark to a hole
    let wasQuestionMark = goal.start + 1 == goal.end
    let isQuestionMark = newStart + 1 == newEnd
    let isQuestionMarkExpansion = wasQuestionMark && !isQuestionMark

    if isQuestionMarkExpansion {
      removeGoal(self, goal)
      insertGoal(self, newStart, newEnd, goal.index)
    } else {
      let updatedGoal = {...goal, start: newStart, end: newEnd}

      // add the updated goal back to the positions tree
      self.positions->AVLTree.insert(newStart, updatedGoal.index)

      // update the goal in the goals map
      self.goals->Map.set(updatedGoal.index, updatedGoal)
    }
  }

  let updateGoalPositionByIndex = (self, index: index, deltaStart: int, deltaEnd: int) =>
    switch getInternalGoalByIndex(self, index) {
    | None => () // goal not found, do nothing
    | Some(goal) => updateGoalPosition(self, goal, deltaStart, deltaEnd)
    }

  module States = {
    // TODO: merge this with InternalGoal.t above
    module State = {
      type boundary =
        | Damaged
        | Intact(int) // delta
        | Unknown

      let boundaryToString = x =>
        switch x {
        | Damaged => "Damaged"
        | Intact(x) => "Intact(" ++ string_of_int(x) ++ ")"
        | Unknown => "Unknown"
        }

      type t =
        | IsQuestionMark(int) // offset of the question mark
        | IsHole(boundary, boundary)

      let toString = x =>
        switch x {
        | IsQuestionMark(x) => "QM " ++ string_of_int(x)
        | IsHole(left, right) => "Hole " ++ boundaryToString(left) ++ " " ++ boundaryToString(right)
        }
    }
    type t = Map.t<index, State.t>

    let make = () => Map.make()

    let toArray = (map: t) =>
      map
      ->Map.entries
      ->Iterator.toArray

    let _toString = (map: t) =>
      map
      ->toArray
      ->Array.map(((index, state)) => {
        "#" ++ Int.toString(index) ++ ": " ++ State.toString(state)
      })
      ->Array.join("\n")

    let addLeftBoundary = (map: t, index, delta): t =>
      switch Map.get(map, index) {
      | None =>
        Map.set(map, index, IsHole(Intact(delta), Unknown))
        map
      | Some(IsQuestionMark(_)) => map
      | Some(IsHole(Damaged, _)) => map
      | Some(IsHole(Intact(_), _)) => map
      | Some(IsHole(Unknown, right)) =>
        Map.set(map, index, IsHole(Intact(delta), right))
        map
      }

    let markLeftBoundaryDamaged = (map: t, index): t =>
      switch Map.get(map, index) {
      | None =>
        Map.set(map, index, IsHole(Damaged, Unknown))
        map
      | Some(IsQuestionMark(_)) => map
      | Some(IsHole(_, right)) =>
        Map.set(map, index, IsHole(Damaged, right))
        map
      }

    let addRightBoundary = (map: t, index, delta): t =>
      switch Map.get(map, index) {
      | None =>
        Map.set(map, index, IsHole(Unknown, Intact(delta)))
        map
      | Some(IsQuestionMark(_)) => map
      | Some(IsHole(_, Damaged)) => map
      | Some(IsHole(_, Intact(_))) => map
      | Some(IsHole(left, Unknown)) =>
        Map.set(map, index, IsHole(left, Intact(delta)))
        map
      }

    let markRightBoundaryDamaged = (map: t, index): t =>
      switch Map.get(map, index) {
      | None =>
        Map.set(map, index, IsHole(Unknown, Damaged))
        map
      | Some(IsQuestionMark(_)) => map
      | Some(IsHole(left, _)) =>
        Map.set(map, index, IsHole(left, Damaged))
        map
      }

    let addQuestionMark = (map: t, index, offset): t =>
      switch Map.get(map, index) {
      | None =>
        Map.set(map, index, IsQuestionMark(offset))
        map
      | Some(IsHole(_, _)) => map
      | Some(IsQuestionMark(_)) => map
      }
  }

  // Token parts of a goal
  type part =
    | LeftBoundary // {!
    | RightBoundary // !}
    | QuestionMark // ?

  let _partToString = x =>
    switch x {
    | LeftBoundary => "{!"
    | RightBoundary => "!}"
    | QuestionMark => "?"
    }

  let goalToParts = (goal: InternalGoal.t): array<(index, int, part)> => {
    if goal.start + 1 == goal.end {
      [(goal.index, goal.start, QuestionMark)]
    } else {
      [(goal.index, goal.start, LeftBoundary), (goal.index, goal.end - 2, RightBoundary)]
    }
  }

  // there are 6 cases to consider when a change overlaps with a goal part:
  type case =
    // 1. the part is after the change, skip the change and move on
    //      removal  ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case1
    // 2. the part is damaged, we'll also need to see if there's another change that damages this part
    //      removal        ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case2
    // 3. the part is damaged, we'll also need to see if there's another part damaged by this change
    //      removal        ┣━━━━━━━━━━━┫
    //      part              ┣━━━━━┫
    | Case3
    // 4. the part is damaged, we'll also need to see if there's another change that damages this part
    //      removal           ┣━━━━━┫
    //      part           ┣━━━━━━━━━━━┫
    | Case4
    // 5. the part is damaged, we'll also need to see if there's another part damaged by this change
    //      removal              ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case5
    // 6. the part is before the change, skip the part and move on
    //      removal                    ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case6

  let caseAnalysis = (removalStart: int, removalEnd: int, start: int, end: int): case => {
    if removalStart == start && removalEnd == start {
      Case1
    } else if removalStart < start {
      if removalEnd <= start {
        Case1 // Case 1
      } else if removalEnd <= end {
        Case2 // Case 2
      } else {
        Case3 // Case 3
      }
    } else if removalEnd <= end && removalStart < end {
      Case4 // Case 4
    } else if removalStart < end {
      Case5 // Case 5
    } else {
      Case6 // Case 6
    }
  }

  let scanAllGoals = async (self, editor, changes) => {
    let document = VSCode.TextEditor.document(editor)

    let changes = changes->List.fromArray

    let rec go = (
      accMap: States.t,
      accDeltaBeforePart: int,
      accDeltaAfterPart: int,
      parts: list<(index, int, part)>,
      changes: list<Tokens.Change.t>,
    ): States.t => {
      switch (parts, changes) {
      | (list{}, _) => accMap
      | (list{(index, start, part), ...parts}, list{}) =>
        switch part {
        | LeftBoundary => accMap->States.addLeftBoundary(index, accDeltaBeforePart)
        | RightBoundary => accMap->States.addRightBoundary(index, accDeltaBeforePart)
        | QuestionMark => accMap->States.addQuestionMark(index, start + accDeltaBeforePart)
        }->go(accDeltaAfterPart, accDeltaAfterPart, parts, changes)
      | (list{(index, start, part), ...parts}, list{change, ...changes}) =>
        let removalStart = change.offset
        let removalEnd = change.offset + change.removed
        let delta = change.inserted - change.removed

        let end = switch part {
        | LeftBoundary => start + 2 // {!
        | RightBoundary => start + 2 // !}
        | QuestionMark => start + 1 // ?
        }

        switch caseAnalysis(removalStart, removalEnd, start, end) {
        | Case1 =>
          // 1. the part is after the change, skip the change and move on
          //      removal  ┣━━━━━┫
          //      part              ┣━━━━━┫
          accMap->go(
            accDeltaBeforePart + delta,
            accDeltaAfterPart + delta,
            list{(index, start, part), ...parts},
            changes,
          )
        | Case2 =>
          // 2. the part is damaged
          //      removal        ┣━━━━━┫
          //      part              ┣━━━━━┫
          switch part {
          | LeftBoundary => accMap->States.markLeftBoundaryDamaged(index)
          | RightBoundary => accMap->States.markRightBoundaryDamaged(index)
          | QuestionMark => accMap
          }->go(
            accDeltaBeforePart,
            accDeltaAfterPart + delta,
            list{(index, start, part), ...parts},
            changes,
          )
        | Case3 =>
          // 3. the part is damaged
          //      removal        ┣━━━━━━━━━━━┫
          //      part              ┣━━━━━┫
          //
          switch part {
          | LeftBoundary => accMap->States.markLeftBoundaryDamaged(index)
          | RightBoundary => accMap->States.markRightBoundaryDamaged(index)
          | QuestionMark => accMap
          }->go(accDeltaBeforePart, accDeltaAfterPart, parts, list{change, ...changes})
        | Case4 =>
          // 4. the part is damaged
          //      removal           ┣━━━━━┫
          //      part           ┣━━━━━━━━━━━┫
          switch part {
          | LeftBoundary =>
            accMap
            ->States.markLeftBoundaryDamaged(index)
            ->go(
              accDeltaBeforePart,
              accDeltaAfterPart + delta,
              list{(index, start, part), ...parts},
              changes,
            )
          | RightBoundary =>
            accMap
            ->States.markRightBoundaryDamaged(index)
            ->go(
              accDeltaBeforePart,
              accDeltaAfterPart + delta,
              list{(index, start, part), ...parts},
              changes,
            )
          | QuestionMark =>
            accMap
            ->States.addLeftBoundary(index, accDeltaBeforePart)
            ->States.addRightBoundary(index, accDeltaBeforePart + 6)
            ->go(accDeltaAfterPart + delta, accDeltaAfterPart + delta, parts, changes)
          }
        | Case5 =>
          // 5. the part is damaged
          //      removal              ┣━━━━━┫
          //      part              ┣━━━━━┫
          switch part {
          | LeftBoundary => accMap->States.markLeftBoundaryDamaged(index)
          | RightBoundary => accMap->States.markRightBoundaryDamaged(index)
          | QuestionMark => accMap
          }->go(accDeltaBeforePart, accDeltaAfterPart, parts, list{change, ...changes})
        | Case6 =>
          // 6. the part is before the change, skip the part and move on
          //      removal                    ┣━━━━━┫
          //      part              ┣━━━━━┫
          switch part {
          | LeftBoundary => accMap->States.addLeftBoundary(index, accDeltaBeforePart)
          | RightBoundary => accMap->States.addRightBoundary(index, accDeltaBeforePart)
          | QuestionMark => accMap->States.addQuestionMark(index, start + accDeltaBeforePart)
          }->go(accDeltaAfterPart, accDeltaAfterPart, parts, list{change, ...changes})
        }
      }
    }

    // update the positions when there are changes
    // list of goal parts ordered by their start position
    let parts =
      self.positions
      ->AVLTree.toArray
      ->Array.filterMap(index => self.goals->Map.get(index)->Option.map(goalToParts))
      ->Array.flat
      ->List.fromArray

    let map = go(States.make(), 0, 0, parts, changes)

    let rewrites =
      map
      ->States.toArray
      ->Array.filterMap(((index, state)) => {
        switch state {
        | IsQuestionMark(offset) =>
          Some(
            VSCode.Range.make(
              VSCode.TextDocument.positionAt(document, offset),
              VSCode.TextDocument.positionAt(document, offset + 1),
            ),
            "{!   !}",
          )
        | IsHole(Intact(a), Intact(b)) =>
          updateGoalPositionByIndex(self, index, a, b)
          None
        | IsHole(_) =>
          removeGoalByIndex(self, index)
          None
        }
      })

    if Array.length(rewrites) != 0 {
      let originalCursorPosition = Editor.Cursor.get(editor)
      // set busy
      setBusy(self)
      let _ = await Editor.Text.batchReplace(document, rewrites)

      // place the cursor inside a hole if it was there before the rewrite
      let cursorWasWithinRewrites =
        rewrites->Array.some(((range, _)) => VSCode.Range.contains(range, originalCursorPosition))
      if cursorWasWithinRewrites {
        switch getGoalAtCursor(self, editor) {
        | None => () // no goal at cursor, do nothing
        | Some(goal) =>
          // set the cursor to the goal
          setCursorByIndex(self, editor, goal.index)
        }
      }
    } else {
      setNotBusy(self)
    }
  }

  // Set indices of goals from `Responses.InteractionPoints` from commands like Load or Refine
  // The indices are ordered by the start position of the goals.
  let resetGoalIndices = async (self, editor, indices) => {
    clear(self)

    let positionsArray =
      self.goalsWithoutIndices
      ->Map.entries
      ->Iterator.toArray

    positionsArray->Array.forEachWithIndex(((start, end), i) =>
      switch indices[i] {
      | None => ()
      | Some(index) => insertGoal(self, start, end, index)
      }
    )

    self.goalsWithoutIndices = Map.make() // clear the goals without indices
    await scanAllGoals(self, editor, [])
  }

  // New version that properly handles both existing goals and new positions from refine operations
  let resetGoalIndicesNew = async (self, editor, indices: array<int>) => {
    // Collect existing goals indexed by positions
    let existingPositions =
      self.goals
      ->Map.values
      ->Iterator.toArray
      ->Array.map(goal => (goal.start, (goal.end, Some(goal))))
      ->Map.fromArray

    // Add new goal positions from refine operations to the map of existing goals indexed by positions
    self.goalsWithoutIndices->Map.forEachWithKey((end, start) => {
      existingPositions->Map.set(start, (end, None)) // None indicates no goal index yet
    })

    // Combine all positions and sort by start offset
    let allPositions =
      existingPositions
      ->Map.entries
      ->Iterator.toArray
      ->Array.toSorted(((start1, _), (start2, _)) => Int.compare(start1, start2))

    // Clear existing goals
    clear(self)

    // Assign indices to all positions in order
    allPositions->Array.forEachWithIndex(((start, (end, _)), i) => {
      switch indices[i] {
      | None => () // should not happen
      | Some(index) => insertGoal(self, start, end, index)
      }
    })

    self.goalsWithoutIndices = Map.make() // clear the goals without indices
    await scanAllGoals(self, editor, [])
  }

  // Add goal positions without indices, e.g. from the Load or Refine command
  let addGoalPositions = (self, positions) =>
    positions->Array.forEach(((start, end)) => {
      self.goalsWithoutIndices->Map.set(start, end)
    })

  let getGoalPositionByIndex = (self, index) => {
    switch self.goals->Map.get(index) {
    | None => None // goal not found
    | Some(goal) => Some((goal.start, goal.end))
    }
  }

  // New holes may be introduced by a refine command, however, we don't have highlighting information
  // for the result of the refine command.
  // So we need to parse the holes from the refine result and decorate them ourself.
  // This function calculates the offsets of the question marks
  let parseGoalPositionsFromRefine = raw => {
    // result of refinement from Agda uses LF "\n" as line endings
    // we need to convert it to the system line endings
    // so that the positions of the question marks can be calculated correctly
    let raw = raw->Parser.splitToLines->Array.join(NodeJs.Os.eol)

    // Find all standalone question marks without problematic regex splitting
    // Include all whitespace characters like the original regex [\s\(\{\_\;\.\\\"@]
    let delimiters = [" ", "\t", "\n", "\r", "(", ")", "{", "}", "_", ";", ".", "\\", "\"", "@"]
    let isDelimiter = char => delimiters->Array.includes(char)

    let rec findQuestionMarks = (text, offset) => {
      switch String.indexOf(text, "?") {
      | -1 => [] // No more question marks
      | index =>
        let actualPosition = offset + index
        let beforeChar = if actualPosition > 0 {
          Some(String.charAt(raw, actualPosition - 1))
        } else {
          None
        }
        let afterChar = if actualPosition < String.length(raw) - 1 {
          Some(String.charAt(raw, actualPosition + 1))
        } else {
          None
        }

        // Check if this ? is standalone (surrounded by delimiters or boundaries)
        let beforeOk = switch beforeChar {
        | None => true // Start of string
        | Some(char) => isDelimiter(char)
        }

        let afterOk = switch afterChar {
        | None => true // End of string
        | Some(char) => isDelimiter(char)
        }

        let remainingText = String.sliceToEnd(text, ~start=index + 1)
        let nextResults = findQuestionMarks(remainingText, offset + index + 1)

        if beforeOk && afterOk {
          [(actualPosition, actualPosition + 1), ...nextResults]
        } else {
          nextResults
        }
      }
    }

    findQuestionMarks(raw, 0)
  }

  let jmupToGoal = (editor, goal: InternalGoal.t) => {
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
        Goal.index: goal.index,
        indexString: Int.toString(goal.index),
        start: goal.start,
        end: goal.end,
      }
    })
}
include Module
