/*
This module manages Agda goals in the VSCode editor. It handles the tricky
problem of keeping track of goal positions when users edit the document. 

The main challenge: Agda goals like {!   !} and ? have fragile boundaries that break
if you type near them. This system detects when edits damage goals and either fixes 
them or removes them cleanly.

Everything uses absolute character offsets because they're simpler to work with
than line/column coordinates. When the document changes, the code scans through all 
goal parts and figures out how each change affects them.

The core algorithm processes document changes atomically - it takes all the changes
that happened since the last scan and processes them together. This prevents race
conditions and keeps goals consistent.
*/
module type Module = {
  type t
  type index = int

  let make: unit => t
  let redecorate: t => unit
  let destroy: t => unit
  let size: t => int
  let resetGoalIndicesOnLoad: (t, VSCode.TextEditor.t, array<index>) => promise<unit>
  let resetGoalIndicesOnRefineOrGive: (t, VSCode.TextEditor.t, array<index>) => promise<unit>
  let addGoalPositions: (t, array<(int, int)>) => unit
  let getGoalPositionByIndex: (t, index) => option<(int, int)>
  let parseGoalPositionsFromRefine: string => array<(int, int)>
  let removeGoalByIndex: (t, index) => unit

  // scan all goals and update their positions after document changes
  let scanAllGoals: (t, VSCode.TextEditor.t, array<TokenChange.t>) => promise<unit>

  let getGoalByIndex: (t, index) => option<Goal.t>

  let modify: (t, VSCode.TextDocument.t, index, string => string) => promise<unit>
  let removeBoundaryAndDestroy: (t, VSCode.TextDocument.t, index) => promise<bool>
  // get the goal at the cursor position
  let getGoalAtCursor: (t, VSCode.TextEditor.t) => option<Goal.t>
  let setCursorByIndex: (t, VSCode.TextEditor.t, int) => unit

  // jumping between goals
  let jumpToTheNextGoal: (t, VSCode.TextEditor.t) => unit
  let jumpToThePreviousGoal: (t, VSCode.TextEditor.t) => unit

  // semaphore for busy state
  let isBusy: t => bool
  let waitUntilNotBusy: t => promise<unit>

  // keep track of the last case split goal
  let markAsCaseSplited: (t, Goal.t) => unit
  let getRecentlyCaseSplited: t => option<Goal.t>

  // for testing
  let serializeGoals: t => array<string>
  let toString: t => string
}

module Module: Module = {
  type index = int

  /*
  These boundary functions handle the geometry of range overlaps and containment.
  They're essential for maintaining goal integrity during document edits.

  Document changes can damage goal boundaries in subtle ways. A single character 
  insertion can corrupt a "{!" or "!}" boundary, making the goal unrecognizable. 
  These utilities provide precise overlap detection to prevent such corruption.

  The overlap detection uses !(end1 <= start2 || end2 <= start1) which says two 
  ranges overlap if neither is completely before the other. This avoids the bugs 
  that come from trying to write overlap logic directly.

  Some functions check inclusive bounds (containsCursor) while others use exclusive 
  bounds (containsCursorExclusive). The difference matters for user interaction - 
  clicking exactly at a boundary might count as "inside" for some purposes but not others.
 */
  module Boundary = {
    // Tests if point range is fully contained within container range
    let _contains = (containerStart: int, containerEnd: int, pointStart: int, pointEnd: int) =>
      containerStart <= pointStart && pointEnd <= containerEnd

    // Two ranges overlap if neither is completely before the other
    let overlaps = (start1: int, end1: int, start2: int, end2: int) =>
      !(end1 <= start2 || end2 <= start1)

    // Calculates the actual overlap range between two ranges, if any
    let _getOverlapRange = (start1: int, end1: int, start2: int, end2: int) => {
      if overlaps(start1, end1, start2, end2) {
        let overlapStart = start1 > start2 ? start1 : start2
        let overlapEnd = end1 < end2 ? end1 : end2
        Some((overlapStart, overlapEnd))
      } else {
        None
      }
    }

    // Cursor position checking with inclusive end bound - cursor AT boundary counts as inside
    let containsCursor = (start: int, end: int, cursorOffset: int) =>
      start <= cursorOffset && cursorOffset <= end

    // Cursor position checking with exclusive end bound - cursor AT end boundary is outside
    let containsCursorExclusive = (start: int, end: int, cursorOffset: int) =>
      start <= cursorOffset && cursorOffset < end

    // For Case4 logic: checks if removal operation affects the goal's end
    let isContainedOrOverlapsEnd = (
      _partStart: int,
      partEnd: int,
      removalStart: int,
      removalEnd: int,
    ) => removalEnd <= partEnd && removalStart < partEnd
  }

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

    let makeDecorationRanges = (document, start: int, end: int) => {
      let backgroundRange = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, start),
        VSCode.TextDocument.positionAt(document, end),
      )

      let indexRange = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, end - 2),
        VSCode.TextDocument.positionAt(document, end - 2),
      )

      (backgroundRange, indexRange)
    }

    let createAndApplyDecoration = (editor, start: int, end: int, index: int) => {
      let document = VSCode.TextEditor.document(editor)
      let (backgroundRange, indexRange) = makeDecorationRanges(document, start, end)

      let background = Editor.Decoration.createBackground("editor.selectionHighlightBackground")
      Editor.Decoration.apply(editor, background, [backgroundRange])

      let indexText = string_of_int(index)
      let index = Editor.Decoration.createTextOverlay("editorLightBulb.foreground", indexText)
      Editor.Decoration.apply(editor, index, [indexRange])

      (background, index)
    }

    let decorate = (start: int, end: int, index: int) =>
      VSCode.Window.activeTextEditor->Option.map(editor => {
        let (background, index) = createAndApplyDecoration(editor, start, end, index)

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

    let toString = goal => {
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
            let (backgroundRange, indexRange) = InternalGoal.makeDecorationRanges(
              document,
              goal.start,
              goal.end,
            )
            Editor.Decoration.apply(editor, background, [backgroundRange])
            Editor.Decoration.apply(editor, index, [indexRange])
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

  let serializeGoals = self =>
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.toSorted((x, y) => Int.compare(x.index, y.index))
    ->Array.map(InternalGoal.toString)

  let serializeGoalsWithoutIndices = self =>
    self.goalsWithoutIndices
    ->Map.entries
    ->Iterator.toArray
    ->Array.map(((start, end)) => `(${string_of_int(start)}, ${string_of_int(end)})`)

  let serializePositions = self => {
    self.positions
    ->AVLTree.toArray
    ->Array.map(((start, index)) => `(${string_of_int(start)}, ${string_of_int(index)})`)
  }

  let toString = self => {
    let goals = serializeGoals(self)->Array.join("\n")
    let goalsWithoutIndices = serializeGoalsWithoutIndices(self)->Array.join(", ")
    let positions = serializePositions(self)->Array.join(", ")

    "Goals:\n" ++
    goals ++
    "\nGoals without indices: [" ++
    goalsWithoutIndices ++
    "]" ++
    "\nPositions: [" ++
    positions ++
    "]" ++
    "\nBusy: " ++ (self.isBusy->Option.isSome ? "true" : "false")
  }

  let removeGoalByIndex = (self, index) => {
    switch self.goals->Map.get(index) {
    | None => ()
    | Some(goal) => removeGoal(self, goal)
    }
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
        if Boundary.containsCursor(goal.start, goal.end, cursorOffset) {
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

  let setCursor = (editor, goal: InternalGoal.t) => {
    let document = VSCode.TextEditor.document(editor)
    // determine where to set the cursor
    let spaceInsideBoundaries = goal.end - goal.start - 4
    let offset = if spaceInsideBoundaries == 0 {
      // {!!}
      goal.start + 2
    } else {
      // {! !}
      goal.start + 3
    }
    let position = VSCode.TextDocument.positionAt(document, offset)
    // set the cursor to the start of the goal
    Editor.Cursor.set(editor, position)
    // scroll to that part of the document
    let range = InternalGoal.makeOuterRange(goal, document)
    editor->VSCode.TextEditor.revealRange(range, None)
  }

  let setCursorByIndex = (self, editor, index) =>
    switch getInternalGoalByIndex(self, index) {
    | None => () // goal not found, do nothing
    | Some(goal) => setCursor(editor, goal)
    }

  // Destory and clear all goals
  let clear = self => {
    self.goals
    ->Map.values
    ->Iterator.toArray
    ->Array.forEach(InternalGoal.undecorate)
    self.goals = Map.make()
    self.positions = AVLTree.make()
    // NOTE: NOT clearing goalsWithoutIndices here - that's done in resetGoalIndicesOnLoad
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

  /*
  Case analysis handles the core problem: when a document change happens, how does 
  it affect each goal part? There are exactly 6 ways a change can relate to a goal 
  part geometrically.

  This drives the entire goal maintenance system. Each case determines whether the 
  goal part gets damaged, updated, or left alone. The cases also control how the 
  algorithm advances through the lists of changes and parts.

  Case4 is special - it's where question marks can expand into holes. This happens 
  when the user types inside a ? goal, turning it into {!   !}.
 */
  type case =
    // Change is completely before the part - accumulate position delta and continue
    //      removal  ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case1
    // Change clips the left edge of the part - mark boundary damaged
    //      removal        ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case2
    // Change completely engulfs the part - mark damaged, keep change for next part
    //      removal        ┣━━━━━━━━━━━┫
    //      part              ┣━━━━━┫
    | Case3
    // Change is inside the part or overlaps its end - handle question mark expansion
    //      removal           ┣━━━━━┫
    //      part           ┣━━━━━━━━━━━┫
    | Case4
    // Change clips the right edge of the part - mark damaged, keep change for next part
    //      removal              ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case5
    // Change is completely after the part - apply delta and move to next part
    //      removal                    ┣━━━━━┫
    //      part              ┣━━━━━┫
    | Case6

  let caseAnalysis = (removalStart: int, removalEnd: int, start: int, end: int): case => {
    if removalStart == start && removalEnd == start {
      Case1
    } else if removalStart < start {
      if !Boundary.overlaps(removalStart, removalEnd, start, end) {
        Case1 // Case 1
      } else if removalEnd <= end {
        Case2 // Case 2
      } else {
        Case3 // Case 3
      }
    } else if Boundary.isContainedOrOverlapsEnd(start, end, removalStart, removalEnd) {
      Case4 // Case 4
    } else if removalStart < end {
      Case5 // Case 5
    } else {
      Case6 // Case 6
    }
  }

  /*
  This is the main function that gets called every time the document changes.
  It's responsible for keeping all goals valid and correctly positioned.

  The algorithm works in two phases:
  1. Scan through all goal parts and changes to build a state map
  2. Apply the state map to update goal positions or remove damaged goals

  Everything is processed atomically - all changes since the last scan are 
  handled together. This prevents race conditions and ensures consistency.

  The scanning uses two delta accumulators: accDeltaBeforePart tracks the 
  cumulative offset for parts we haven't processed yet, while accDeltaAfterPart 
  includes the current change. This ensures correct position calculations as 
  the algorithm moves left to right through the document.
 */
  let scanAllGoals = async (self, editor, changes) => {
    let document = VSCode.TextEditor.document(editor)

    let changes = changes->List.fromArray

    let rec go = (
      accMap: States.t,
      accDeltaBeforePart: int,
      accDeltaAfterPart: int,
      parts: list<(index, int, part)>,
      changes: list<TokenChange.t>,
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
      ->Array.filterMap(((_, index)) => self.goals->Map.get(index)->Option.map(goalToParts))
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

  // Set indices of goals from `Responses.InteractionPoints` on Load
  // The indices are ordered by the start position of the goals.
  let resetGoalIndicesOnLoad = async (self, editor, indices) => {
    clear(self)

    let positionsArray = self.goalsWithoutIndices->Map.entries->Iterator.toArray
    positionsArray->Array.forEachWithIndex(((start, end), i) => {
      switch indices[i] {
      | None => ()
      | Some(index) => insertGoal(self, start, end, index)
      }
    })

    self.goalsWithoutIndices = Map.make() // clear the goals without indices
    await scanAllGoals(self, editor, [])
  }

  // Set indices of goals from `Responses.InteractionPoints` on Refine or Give
  //
  // This function provides defensive handling against phantom goal positions that can occur
  // when Agda sends faulty token positions during refine/give operations. Unlike resetGoalIndicesOnLoad
  // which blindly trusts all positions from goalsWithoutIndices, this function preserves existing
  // valid goals and only assigns indices to positions that actually have corresponding indices
  // from Agda's response. This prevents orphaned positions from becoming unwanted {!   !} goals.
  let resetGoalIndicesOnRefineOrGive = async (self, editor, indices: array<int>) => {
    // Helper function to collect goal positions from existing goals
    let collectExistingGoalPositions = () =>
      self.goals
      ->Map.values
      ->Iterator.toArray
      ->Array.map(goal => (goal.start, (goal.end, Some(goal))))
      ->Map.fromArray

    // Helper function to merge new goal positions with existing ones
    let mergeGoalPositions = existingPositions => {
      self.goalsWithoutIndices->Map.forEachWithKey((end, start) => {
        existingPositions->Map.set(start, (end, None)) // None indicates no goal index yet
      })
      existingPositions
    }

    // Helper function to sort positions by start offset
    let sortPositionsByOffset = positions =>
      positions
      ->Map.entries
      ->Iterator.toArray
      ->Array.toSorted(((start1, _), (start2, _)) => Int.compare(start1, start2))

    // Helper function to assign indices to sorted positions
    let assignIndices = sortedPositions => {
      sortedPositions->Array.forEachWithIndex(((start, (end, _)), i) => {
        switch indices[i] {
        | None => () // should not happen
        | Some(index) => insertGoal(self, start, end, index)
        }
      })
    }

    // Main refactoring logic
    let existingPositions = collectExistingGoalPositions()
    let mergedPositions = mergeGoalPositions(existingPositions)
    let sortedPositions = sortPositionsByOffset(mergedPositions)

    // Clear existing goals before reassigning
    clear(self)

    // Assign new indices to all positions
    assignIndices(sortedPositions)

    // Clear processed goals without indices
    self.goalsWithoutIndices = Map.make()

    // Update goal positions after document changes
    await scanAllGoals(self, editor, [])
  }

  // Add goal positions without indices, e.g. from the Load or Refine command  
  let addGoalPositions = (self, positions) => {
    positions->Array.forEach(((start, end)) => {
      self.goalsWithoutIndices->Map.set(start, end)
    })
  }

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

  let jumpToTheNextGoal = (self, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let cursorOffset = VSCode.TextDocument.offsetAt(document, Editor.Cursor.get(editor))

    // find the first Goal after the cursor
    let goal = switch self.positions->AVLTree.upperBound(cursorOffset) {
    | None => self.goals->Map.get(0) // if no goal is found, maybe we are at the end of the document, try to return the first goal
    | Some(index) => self.goals->Map.get(index)
    }

    switch goal {
    | None => () // no goal found, do nothing
    | Some(goal) => setCursor(editor, goal)
    }
  }

  let jumpToThePreviousGoal = (self, editor) => {
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
        if Boundary.containsCursorExclusive(goal.start, goal.end, cursorOffset) {
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
    | Some(goal) => setCursor(editor, goal)
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
