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

  // #371: Agda 2.9 (master) labels the delimiter before a non-empty context
  // with "Context" (agda/agda@bd82cf22). Without a context, the delimiter
  // is still the plain 60-dash line.
  it("should parse goal + labeled context delimiter + context", () => {
    let raw = `Goal: Type
———— Context ———————————————————————————————————————————————
A : Type`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = Dict.fromArray([("goal", ["Goal: Type"]), ("interactionMetas", ["A : Type"])])
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse goal + have + labeled context delimiter + context", () => {
    let raw = `Goal: ℕ
Have: ℕ
———— Context ———————————————————————————————————————————————
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
  // #371: the labeled and the plain delimiter must render identically.
  it("should render a labeled context delimiter like the plain one", () => {
    let labeled = `Goal: Type
———— Context ———————————————————————————————————————————————
A : Type`
    let plain = `Goal: Type
————————————————————————————————————————————————————————————
A : Type`
    let actual = labeled->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    let expected = plain->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    Assert.deepStrictEqual(actual, expected)
    Assert.deepStrictEqual(Array.length(actual), 2)
  })

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

  // #371: Agda labels the wanted boundary of a cubical goal with
  // `———— Boundary (wanted) ————…`, followed by one equation per line.
  it("should render a labeled divider as a header followed by one row per entry", () => {
    let raw = `Goal: Type₁
———— Boundary (wanted) —————————————————————————————————————
i = i0 ⊢ Type
i = i1 ⊢ Type`
    let actual = raw->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    let expected = [
      Item.Labeled("Goal", "special", RichText.string("Type₁"), None, None),
      Item.Header("Boundary (wanted)"),
      Item.Unlabeled(RichText.string("i = i0 ⊢ Type"), None, None),
      Item.Unlabeled(RichText.string("i = i1 ⊢ Type"), None, None),
    ]
    Assert.deepStrictEqual(actual, expected)
  })

  // #371: `Cmd_goal_type_context_check` puts `Elaborates to:` after the goal type.
  it("should render Elaborates to as its own labeled item", () => {
    let raw = `Goal: ℕ
Elaborates to: zero`
    let actual = raw->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    let expected = [
      Item.Labeled("Goal", "special", RichText.string("ℕ"), None, None),
      Item.Labeled("Elaborates to", "special", RichText.string("zero"), None, None),
    ]
    Assert.deepStrictEqual(actual, expected)
  })

  // #371: Agda emits the context before the constraints.
  it("should render the context before Constraints", () => {
    let context = `Goal: Nat
———— Context ———————————————————————————————————————————————
n : Nat`
    let constraints = `Goal: Nat
———— Constraints ———————————————————————————————————————————
_5 := refl (blocked on problem 5)`
    let raw = `Goal: Nat
———— Context ———————————————————————————————————————————————
n : Nat
———— Constraints ———————————————————————————————————————————
_5 := refl (blocked on problem 5)`
    let actual = raw->Emacs__Parser2.parseGoalType->Emacs__Parser2.render
    // the goal, the context, then the Constraints header and its rows
    let expected = Array.concat(
      context->Emacs__Parser2.parseGoalType->Emacs__Parser2.render,
      constraints
      ->Emacs__Parser2.parseGoalType
      ->Emacs__Parser2.render
      ->Array.sliceToEnd(~start=1),
    )
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

  // #371: since Agda 2.7, the divider before the errors is
  // `———— Error ————…` instead of `———— Errors ————…`.
  it("should parse goals + errors with the Error divider", () => {
    let raw = `?0 : Nat
———— Error —————————————————————————————————————————————————
/path/to/agda/examples/A.agda:8.1-9.16: error: [TerminationIssue]
Termination checking failed for the following function:
  loop`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals, Errors*", raw)
    let expected = Dict.fromArray([
      ("interactionMetas", ["?0 : Nat"]),
      (
        "errors",
        [
          `/path/to/agda/examples/A.agda:8.1-9.16: error: [TerminationIssue]
Termination checking failed for the following function:
  loop`,
        ],
      ),
    ])->tempNormalize
    Assert.deepStrictEqual(actual, expected)
  })

  // #371: with no goals, Agda emits the errors first, each after its divider.
  it("should parse errors + warnings", () => {
    let raw = `———— Error —————————————————————————————————————————————————
/path/to/agda/examples/A.agda:10.1-11.16: error: [TerminationIssue]
Termination checking failed for the following function:
  loop
———— Warnings ——————————————————————————————————————————————
/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
MATRIX_WARNING
when scope checking A`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Errors, Warnings*", raw)
    let expected = Dict.fromArray([
      (
        "errors",
        [
          `/path/to/agda/examples/A.agda:10.1-11.16: error: [TerminationIssue]
Termination checking failed for the following function:
  loop`,
        ],
      ),
      (
        "warnings",
        [
          `/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
MATRIX_WARNING
when scope checking A`,
        ],
      ),
    ])->tempNormalize
    Assert.deepStrictEqual(actual, expected)
  })

  // #371: since Agda 2.7, each warning starts with its range followed by
  // its severity and name, like `A.agda:7.10-11: warning: -W[no]UserWarning`.
  it("should parse each warning with its severity after the range", () => {
    let raw = `/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
FIRST_WARNING
when scope checking A
/path/to/agda/examples/A.agda:9.10-11: warning: -W[no]UserWarning
SECOND_WARNING
when scope checking B`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Warnings*", raw)
    let expected = Dict.fromArray([
      (
        "warnings",
        [
          `/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
FIRST_WARNING
when scope checking A`,
          `/path/to/agda/examples/A.agda:9.10-11: warning: -W[no]UserWarning
SECOND_WARNING
when scope checking B`,
        ],
      ),
    ])->tempNormalize
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

describe("when running Emacs__Parser2.parseAllGoalsWarnings and Emacs__Parser2.render together", () => {
  // #371: Agda emits the goals before the warnings.
  it("should render goals before warnings", () => {
    let warning = `/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
MATRIX_WARNING
when scope checking A`
    let raw = `?0 : Nat
———— Warnings ——————————————————————————————————————————————
${warning}`
    let actual =
      Emacs__Parser2.parseAllGoalsWarnings("*All Goals, Warnings*", raw)->Emacs__Parser2.render
    let expected = Array.concat(
      Emacs__Parser2.parseAllGoalsWarnings("*All Goals*", "?0 : Nat")->Emacs__Parser2.render,
      Emacs__Parser2.parseAllGoalsWarnings("*All Warnings*", warning)->Emacs__Parser2.render,
    )
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
  // #371: since Agda 2.7, the divider before the warnings of an error is
  // `———— Warnings ————…` instead of `———— Warning(s) ————…`.
  it("should parse an error + warnings with the Warnings divider", () => {
    let raw = `———— Error —————————————————————————————————————————————————
/path/to/agda/examples/A.agda:10.1-11.16: error: [TerminationIssue]
Termination checking failed for the following function:
  loop
———— Warnings ——————————————————————————————————————————————
/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
MATRIX_WARNING
when scope checking A`
    let actual = Emacs__Parser2.parseError(raw)
    let expected = Dict.fromArray([
      (
        "errors",
        [
          `/path/to/agda/examples/A.agda:10.1-11.16: error: [TerminationIssue]
Termination checking failed for the following function:
  loop`,
        ],
      ),
      (
        "warnings",
        [
          `/path/to/agda/examples/A.agda:7.10-11: warning: -W[no]UserWarning
MATRIX_WARNING
when scope checking A`,
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
        // #371: in the order Agda emits them
        "warnings",
        [
          `/path/to/agda/examples/A.agda:17,1-8
The following names are declared but not accompanied by a
definition: boo`,
          `/path/to/agda/examples/A.agda:9,1-10
Unreachable clause
when checking the definition of _+_`,
        ],
      ),
    ])->tempNormalize
    Assert.deepStrictEqual(actual, expected)
  })
})
