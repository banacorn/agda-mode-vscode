module type Module = {
  type t
  type index = int

  // External representation of a goal in the editor
  type goalInfo = {
    index: string,
    content: string,
    start: int,
    end: int,
  }
  let makeHaskellRangeFromGoalInfo: (goalInfo, VSCode.TextDocument.t, string, string) => string

  let make: unit => t
  let instantiateGoalsFromLoad: (
    t,
    VSCode.TextEditor.t,
    array<index>,
    Map.t<int, int>,
  ) => promise<unit>

  // scan all goals and update their positions after document changes
  let scanAllGoals: (t, VSCode.TextEditor.t, array<Tokens.Change.t>) => promise<unit>

  let getGoalInfoByIndex: (t, index) => option<goalInfo>

  // let read: (t, VSCode.TextDocument.t, index) => option<string>
  let modify: (t, VSCode.TextDocument.t, index, string => string) => promise<unit>
  // let removeBoundaryAndDestroy: (t, VSCode.TextDocument.t, index) => promise<unit>
  // helper function for building Haskell Agda ranges
  // let buildHaskellRange: (Goal.t, VSCode.TextDocument.t, string, string) => string
  // get the goal at the cursor position
  let getGoalIndexAndContentAtCursor: (t, VSCode.TextEditor.t) => option<(index, string)>

  // jumping between goals
  let jmupToTheNextGoal: (t, VSCode.TextEditor.t) => unit
  let jmupToThePreviousGoal: (t, VSCode.TextEditor.t) => unit

  // semaphore for busy state
  let isBusy: t => bool
  let waitUntilNotBusy: t => promise<unit>

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
      content: string,
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
      let document = VSCode.TextEditor.document(editor)

      let innerRange = VSCode.Range.make(
        VSCode.TextDocument.positionAt(document, start + 2),
        VSCode.TextDocument.positionAt(document, end - 2),
      )
      let content = Editor.Text.get(document, innerRange)->String.trim

      {
        index,
        start,
        end,
        content,
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
  type goalInfo = {
    index: string,
    content: string,
    start: int,
    end: int,
  }

  let makeHaskellRangeFromGoalInfo = (goalInfo, document, version, filepath) => {
    let startPoint = VSCode.TextDocument.positionAt(document, goalInfo.start)
    let endPoint = VSCode.TextDocument.positionAt(document, goalInfo.end)

    let startIndex = string_of_int(goalInfo.start + 3)
    let startRow = string_of_int(VSCode.Position.line(startPoint) + 1)
    let startColumn = string_of_int(VSCode.Position.character(startPoint) + 3)
    let startPart = `${startIndex} ${startRow} ${startColumn}`
    let endIndex' = string_of_int(goalInfo.end - 3)
    let endRow = string_of_int(VSCode.Position.line(endPoint) + 1)
    let endColumn = string_of_int(VSCode.Position.character(endPoint) - 1)
    let endPart = `${endIndex'} ${endRow} ${endColumn}`

    if Util.Version.gte(version, "2.8.0") {
      `(intervalsToRange (Just (mkAbsolute "${filepath}")) [Interval () (Pn () ${startPart}) (Pn () ${endPart})])` // after 2.8.0
    } else if Util.Version.gte(version, "2.5.1") {
      `(intervalsToRange (Just (mkAbsolute "${filepath}")) [Interval (Pn () ${startPart}) (Pn () ${endPart})])` // after 2.5.1, before (not including) 2.8.0
    } else {
      `(Range [Interval (Pn (Just (mkAbsolute "${filepath}")) ${startPart}) (Pn (Just (mkAbsolute "${filepath}")) ${endPart})])` // before (not including) 2.5.1
    }
  }

  type t = {
    mutable goals: Map.t<index, Goal.t>, // goal index => goal
    mutable positions: AVLTree.t<index>, // start position => goal index
    mutable isBusy: option<Resource.t<unit>>, // semaphore for busy state
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

  let getGoalByIndex = (self, index) => self.goals->Map.get(index)
  let getGoalInfoByIndex = (self, index): option<goalInfo> =>
    self.goals
    ->Map.get(index)
    ->Option.map(goal => {
      index: Int.toString(goal.index),
      content: goal.content,
      start: goal.start,
      end: goal.end,
    })

  let read = (goal, document) => {
    let innerRange = Goal.makeInnerRange(goal, document)
    Editor.Text.get(document, innerRange)->String.trim
  }

  let modify = async (self, document, index, f) =>
    switch getGoalByIndex(self, index) {
    | Some(goal) =>
      let innerRange = Goal.makeInnerRange(goal, document)
      let goalContent = read(goal, document)
      let _ = await Editor.Text.replace(document, innerRange, " " ++ f(goalContent) ++ " ")
    | None => ()
    }

  // let removeBoundaryAndDestroy = async (self, document, index) => {
  //   let innerRange = getInnerRange(self, document, index)
  //   let outerRange = Map.get(self.goals, index)->Option.map(goal => {
  //     VSCode.Range.make(
  //       VSCode.TextDocument.positionAt(document, goal.start),
  //       VSCode.TextDocument.positionAt(document, goal.end),
  //     )
  //   })

  //   let content = read(self, document, index)
  //   if await Editor.Text.replace(state.document, outerRange, content) {
  //     Goal.destroyDecoration(goal)
  //   } else {
  //     await State__View.Panel.display(
  //       state,
  //       Error("Goal-related Error"),
  //       [Item.plainText("Unable to remove the boundary of goal #" ++ string_of_int(goal.index))],
  //     )
  //   }
  // }

  let getGoalIndexAndContentAtCursor = (self, editor) => {
    let document = VSCode.TextEditor.document(editor)
    let cursorOffset = VSCode.TextDocument.offsetAt(document, Editor.Cursor.get(editor))
    self.positions
    ->AVLTree.lowerBound(cursorOffset)
    ->Option.flatMap(index => {
      switch self.goals->Map.get(index) {
      | None => None // no goal found
      | Some(goal) =>
        if cursorOffset >= goal.start && cursorOffset < goal.end {
          let goalContent =
            Editor.Text.get(
              document,
              VSCode.Range.make(
                VSCode.TextDocument.positionAt(document, goal.start + 2),
                VSCode.TextDocument.positionAt(document, goal.end - 2),
              ),
            )->String.trim
          Some(goal.index, goalContent) // return the index of the goal
        } else {
          None // no goal found at the cursor position
        }
      }
    })
  }

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

  // Damage done to a goal. The letters correspond to the boundaries of the goal like this:
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
      } else if goalText->String.startsWith("{!") {
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
      } else if goalText->String.endsWith("!}") {
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
          let delta = delta + change.inserted - change.removed
          go(delta, list{goal, ...goals}, changes)
        } else if removalStart >= goal.end {
          // the goal is completely before the change, skip the goal
          go(delta, goals, list{change, ...changes})
        } else if goal.start >= removalStart && goal.end <= removalEnd {
          let deltaStart = removalStart - goal.start
          let deltaEnd = deltaStart + change.inserted - change.removed
          let actions = scanGoal(delta, goal, deltaStart, deltaEnd, true)
          let delta' = delta + change.inserted - change.removed
          [...actions, ...go(delta', goals, changes)]
        } else {
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

    await scanAllGoals(self, editor, [])
  }

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
}
include Module
