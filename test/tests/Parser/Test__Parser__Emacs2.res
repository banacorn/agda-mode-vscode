open Mocha

let tempNormalize = xs => {
  Js.Dict.map((. value) => {
    value->Array.map(x => x->Util.String.lines->Util.String.unlines)
  }, xs)
}

describe("when running Emacs__Parser2.parseGoalType", () => {
  it("should parse goal only", () => {
    let raw = `Goal: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = Dict.fromArray([("goal", ["Goal: ℕ"])])
    Assert.deepEqual(actual, expected)
  })

  it("should parse goal + have", () => {
    let raw = `Goal: ℕ
Have: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = Dict.fromArray([("goal", ["Goal: ℕ"]), ("have", ["Have: ℕ"])])
    Assert.deepEqual(actual, expected)
  })

  it("should parse goal + have + context", () => {
    let raw = `Goal: ℕ
Have: ℕ
————————————————————————————————————————————————————————————
y : ℕ
x : ℕ`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = Dict.fromArray([
      ("goal", ["Goal: ℕ"]),
      ("have", ["Have: ℕ"]),
      ("interactionMetas", ["y : ℕ", "x : ℕ"]),
    ])
    Assert.deepEqual(actual, expected)
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
    let expected = Dict.fromArray([
      ("interactionMetas", ["?0 : ℕ", "?1 : ℕ"]),
      ("hiddenMetas", ["Sort _0  [ at /Users/banacorn/agda/examples/A.agda:11,5-20 ]"]),
    ])
    Assert.deepEqual(actual, expected)
  })

  it("should parse goals + errors", () => {
    let raw = `?0 : _2

———— Errors ————————————————————————————————————————————————
Unsolved constraints`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals, Errors*", raw)
    let expected = Dict.fromArray([
      ("interactionMetas", ["?0 : _2"]),
      ("errors", ["Unsolved constraints"]),
    ])
    Assert.deepEqual(actual, expected)
  })

  it("should parse goals that span multiple lines", () => {
    let raw = `?0
  : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals, Errors*", raw)
    let expected = Dict.fromArray([
      (
        "interactionMetas",
        [
          `?0
  : BoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBoolBool`,
        ],
      ),
    ])->tempNormalize
    Assert.deepEqual(actual, expected)
  })
})

describe("when running Emacs__Parser2.parseError", () => {
  it("should parse an error only", () => {
    let raw = `/Users/banacorn/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`
    let actual = Emacs__Parser2.parseError(raw)
    let expected = Dict.fromArray([
      (
        "errors",
        [
          `/Users/banacorn/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`,
        ],
      ),
    ])->tempNormalize
    Assert.deepEqual(actual, expected)
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
    let expected = Dict.fromArray([
      (
        "errors",
        [
          `/Users/banacorn/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`,
        ],
      ),
      (
        "warnings",
        [
          `/Users/banacorn/agda/examples/A.agda:9,1-10
Unreachable clause
when checking the definition of _+_`,
          `/Users/banacorn/agda/examples/A.agda:17,1-8
The following names are declared but not accompanied by a
definition: boo`,
        ],
      ),
    ])->tempNormalize
    Assert.deepEqual(actual, expected)
  })
})
