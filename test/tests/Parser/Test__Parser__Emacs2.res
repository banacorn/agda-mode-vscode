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
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse goal + have", () => {
    let raw = `Goal: ℕ
Have: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = Dict.fromArray([("goal", ["Goal: ℕ"]), ("have", ["Have: ℕ"])])
    Assert.deepStrictEqual(actual, expected)
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
    Assert.deepStrictEqual(actual, expected)
  })
})

describe("when running Emacs__Parser2.parseGoalType and Emacs__Parser2.render together", () => {
  // #337: Agda line-wraps a goal type that's too long for its own
  // fixed-width display. `parseGoalType` keeps each wrapped physical line
  // as a separate array entry; `render` then rejoins them with
  // `Util.String.unlines` (a literal "\n") before handing the string to
  // `Agda.Expr.parse`, which passes it straight through into a single
  // `RichText` `Text` node with no whitespace collapsing. The wrap is a
  // display artifact, not a semantic line break, so it should not survive
  // as a literal "\n" in the rendered item.
  //
  // `raw` below is not hand-written: it's the real `*Goal type etc.*`
  // response captured from Agda 2.8.0 (via `agda --interaction`, and
  // independently verified through this extension's own
  // `Request.GoalTypeAndContext` -> `sendRequestAndCollectResponses`
  // pipeline) for the goal
  //   test : Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat →
  //          Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat
  // -- not a synthetic stand-in.
  it("should collapse an Agda-wrapped goal type into a single line", () => {
    let raw = `Goal: Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat
————————————————————————————————————————————————————————————`
    let actual = raw->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    let expected = [
      Item.Labeled(
        "Goal",
        "special",
        RichText.string(
          "Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat",
        ),
        None,
        None,
      ),
    ]
    Assert.deepStrictEqual(actual, expected)
  })

  // Real response for `Cmd_goal_type_context_infer` (`test.GoalTypeContextAndInferredType`)
  // on the same goal, given `longFn` (of the same 21-ary `Nat` type) as the
  // expression to infer -- both Goal and Have wrap identically.
  it("should collapse an Agda-wrapped \"Have\" type into a single line", () => {
    let raw = `Goal: Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat
Have: Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat →
      Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat
————————————————————————————————————————————————————————————`
    let actual = raw->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    let chain = "Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat → Nat"
    let expected = [
      Item.Labeled("Goal", "special", RichText.string(chain), None, None),
      Item.Labeled("Have", "special", RichText.string(chain), None, None),
    ]
    Assert.deepStrictEqual(actual, expected)
  })

  // Real response for the goal `"a  b" ≡ "a  b"` -- Agda's own output
  // preserves the intentional double space inside the string literal, on
  // one line, with no wrapping. This guards the #337 fix itself: it must
  // only collapse whitespace runs that contain the newline Agda's
  // *display* wrap introduces, and must never touch same-line whitespace
  // that's part of the actual content.
  it("should not touch an intentional double space inside a string literal", () => {
    let raw = `Goal: "a  b" ≡ "a  b"
————————————————————————————————————————————————————————————`
    let actual = raw->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    let expected = [
      Item.Labeled("Goal", "special", RichText.string(`"a  b" ≡ "a  b"`), None, None),
    ]
    Assert.deepStrictEqual(actual, expected)
  })
})

describe("when running Emacs__Parser2.parseAllGoalsWarnings", () => {
  it("should parse goals only", () => {
    let raw = `
?0 : ℕ
?1 : ℕ
Sort _0  [ at /path/to/agda/examples/A.agda:11,5-20 ]
`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals*", raw)
    let expected = Dict.fromArray([
      ("interactionMetas", ["?0 : ℕ", "?1 : ℕ"]),
      ("hiddenMetas", ["Sort _0  [ at /path/to/agda/examples/A.agda:11,5-20 ]"]),
    ])
    Assert.deepStrictEqual(actual, expected)
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
    Assert.deepStrictEqual(actual, expected)
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
    Assert.deepStrictEqual(actual, expected)
  })
})

describe("when running Emacs__Parser2.parseError", () => {
  it("should parse an error only", () => {
    let raw = `/path/to/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`
    let actual = Emacs__Parser2.parseError(raw)
    let expected = Dict.fromArray([
      (
        "errors",
        [
          `/path/to/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`,
        ],
      ),
    ])->tempNormalize
    Assert.deepStrictEqual(actual, expected)
  })
  it("should parse an error + warnings", () => {
    let raw = `———— Error —————————————————————————————————————————————————
/path/to/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8

———— Warning(s) ————————————————————————————————————————————
/path/to/agda/examples/A.agda:17,1-8
The following names are declared but not accompanied by a
definition: boo
/path/to/agda/examples/A.agda:9,1-10
Unreachable clause
when checking the definition of _+_`
    let actual = Emacs__Parser2.parseError(raw)
    let expected = Dict.fromArray([
      (
        "errors",
        [
          `/path/to/agda/examples/A.agda:15,1-2
The right-hand side can only be omitted if there is an absurd
pattern, () or {}, in the left-hand side.
when checking that the clause a has type _8`,
        ],
      ),
      (
        "warnings",
        [
          `/path/to/agda/examples/A.agda:9,1-10
Unreachable clause
when checking the definition of _+_`,
          `/path/to/agda/examples/A.agda:17,1-8
The following names are declared but not accompanied by a
definition: boo`,
        ],
      ),
    ])->tempNormalize
    Assert.deepStrictEqual(actual, expected)
  })
})
