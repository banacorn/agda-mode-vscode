open! BsMocha.Mocha
open! Belt

module Assert = BsMocha.Assert

describe("when running Emacs__Parser2.parseGoalType", () => {
  it("should parse goal only", () => {
    let raw = `Goal: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = [Item.Labeled("Goal", "special", RichText.string("ℕ"), None, None)]
    Assert.deep_equal(actual, expected)
  })

  it("should parse goal + have", () => {
    let raw = `Goal: ℕ
Have: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = [
      Item.Labeled("Goal", "special", RichText.string("ℕ"), None, None),
      Item.Labeled("Have", "special", RichText.string("ℕ"), None, None),
    ]
    Assert.deep_equal(actual, expected)
  })

  it("should parse goal + have + context", () => {
    let raw = `Goal: ℕ
Have: ℕ
————————————————————————————————————————————————————————————
y : ℕ
x : ℕ`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = [
      Item.Labeled("Goal", "special", RichText.string("ℕ"), None, None),
      Item.Labeled("Have", "special", RichText.string("ℕ"), None, None),
      Item.Unlabeled(
        RichText.concatMany([RichText.string("y"), RichText.string(" : "), RichText.string("ℕ")]),
        None,
        None,
      ),
      Item.Unlabeled(
        RichText.concatMany([RichText.string("x"), RichText.string(" : "), RichText.string("ℕ")]),
        None,
        None,
      ),
    ]
    Assert.deep_equal(actual, expected)
  })
})

describe("when running Emacs__Parser2.parseAllGoalsWarnings", () => {
  it("should parse goals only", () => {
    let raw = `
?0 : ℕ
?1 : ℕ
Sort _0  [ at /Users/banacorn/agda/examples/A.agda:11,5-20 ]
`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals*", raw)
    let expected = Js.Dict.fromArray([
      ("interactionMetas", ["?0 : ℕ", "?1 : ℕ"]),
      ("hiddenMetas", ["Sort _0  [ at /Users/banacorn/agda/examples/A.agda:11,5-20 ]"]),
    ])
    Assert.deep_equal(actual, expected)
  })

  it("should parse goals + errors", () => {
    let raw = `?0 : _2

———— Errors ————————————————————————————————————————————————
Unsolved constraints`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals, Errors*", raw)
    let expected = Js.Dict.fromArray([
      ("interactionMetas", ["?0 : _2"]),
      ("errors", ["Unsolved constraints"]),
    ])
    Assert.deep_equal(actual, expected)
  })

  it("should parse goals that span multiple lines", () => {
    let raw = `?0
  : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals, Errors*", raw)
    let expected = Js.Dict.fromArray([
      (
        "interactionMetas",
        [
          `?0
  : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool`,
        ],
      ),
    ])
    Assert.deep_equal(actual, expected)
  })

  // ?0
  // : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool
})

describe("when running Emacs__Parser2.parseError", () => {
  it("should parse an error only", () => {
    let raw = `/Users/banacorn/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`
    let actual = Emacs__Parser2.parseError(raw)
    let expected = [
      Item.Labeled(
        "Error",
        "error",
        RichText.concatMany([
          RichText.string(""),
          RichText.srcLoc(
            Common.AgdaRange.Range(
              Some("/Users/banacorn/agda/examples/A.agda"),
              [
                {
                  start: {col: 1, line: 15, pos: 0},
                  end_: {col: 2, line: 15, pos: 0},
                },
              ],
            ),
          ),
          RichText.string(
            "\nThe right-hand side can only be omitted if there is an absurd\npattern, () or {}, in the left-hand side.\nwhen checking that the clause a has type _8",
          ),
        ]),
        None,
        None,
      ),
    ]
    Assert.deep_equal(actual, expected)
  })
  it("should parse an error + warnings", () => {
    let raw = `———— Error —————————————————————————————————————————————————
/Users/banacorn/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8

———— Warning(s) ————————————————————————————————————————————
/Users/banacorn/agda/examples/A.agda:17,1-8
The following names are declared but not accompanied by a
definition: boo
/Users/banacorn/agda/examples/A.agda:9,1-10
Unreachable clause
when checking the definition of _+_`
    let actual = Emacs__Parser2.parseError(raw)
    let expected = [
      Item.Labeled(
        "Error",
        "error",
        RichText.concatMany([
          RichText.string(""),
          RichText.srcLoc(
            Common.AgdaRange.Range(
              Some("/Users/banacorn/agda/examples/A.agda"),
              [
                {
                  start: {col: 1, line: 15, pos: 0},
                  end_: {col: 2, line: 15, pos: 0},
                },
              ],
            ),
          ),
          RichText.string(
            "\nThe right-hand side can only be omitted if there is an absurd\npattern, () or {}, in the left-hand side.\nwhen checking that the clause a has type _8",
          ),
        ]),
        None,
        None,
      ),
      Item.Labeled(
        "Warning",
        "warning",
        RichText.concatMany([
          RichText.string(""),
          RichText.srcLoc(
            Common.AgdaRange.Range(
              Some("/Users/banacorn/agda/examples/A.agda"),
              [
                {
                  start: {col: 1, line: 9, pos: 0},
                  end_: {col: 10, line: 9, pos: 0},
                },
              ],
            ),
          ),
          RichText.string("\nUnreachable clause\nwhen checking the definition of _+_"),
        ]),
        None,
        None,
      ),
      Item.Labeled(
        "Warning",
        "warning",
        RichText.concatMany([
          RichText.string(""),
          RichText.srcLoc(
            Common.AgdaRange.Range(
              Some("/Users/banacorn/agda/examples/A.agda"),
              [
                {
                  start: {col: 1, line: 17, pos: 0},
                  end_: {col: 8, line: 17, pos: 0},
                },
              ],
            ),
          ),
          RichText.string(
            "\nThe following names are declared but not accompanied by a\ndefinition: boo",
          ),
        ]),
        None,
        None,
      ),
    ]
    Assert.deep_equal(actual, expected)
  })
})
