// External representation of a goal in the editor
module type Module = {
  type t = {
    index: int,
    indexString: string,
    start: int,
    end: int,
  }

  let read: (t, VSCode.TextDocument.t) => string

  // helper function for building Haskell Agda ranges
  let makeHaskellRange: (t, VSCode.TextDocument.t, string, string) => string
  let indentationWidth: (t, VSCode.TextDocument.t) => (int, string, VSCode.Range.t)
}

module Module: Module = {
  type t = {
    index: int,
    indexString: string, // for serialization
    start: int,
    end: int,
  }

  let makeHaskellRange = (goalInfo, document, version, filepath) => {
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

  let read = (goal, document) => {
    let innerRange = VSCode.Range.make(
      VSCode.TextDocument.positionAt(document, goal.start + 2),
      VSCode.TextDocument.positionAt(document, goal.end - 2),
    )
    Editor.Text.get(document, innerRange)->String.trim
  }

  // returns the width of indentation of the first line of a goal
  // along with the text and the range before the goal
  let indentationWidth = (goal, document) => {
    let goalStart = document->VSCode.TextDocument.positionAt(goal.start)
    let lineNo = VSCode.Position.line(goalStart)
    let range = VSCode.Range.make(VSCode.Position.make(lineNo, 0), goalStart)
    let textBeforeGoal = Editor.Text.get(document, range)
    // tally the number of blank characters
    // ' ', '\012', '\n', '\r', and '\t'
    let indentedBy = s => {
      let n = ref(0)
      for i in 0 to String.length(s) - 1 {
        switch String.charAt(s, i) {
        | " "
        | ""
        | "
"
        | "	" =>
          if i == n.contents {
            n := n.contents + 1
          }
        | _ => ()
        }
      }
      n.contents
    }
    (indentedBy(textBeforeGoal), textBeforeGoal, range)
  }
}
include Module
