open Mocha

describe("when running Agda.Expr.parse", () => {
  it("should parse a plain string", () => {
    let raw = `ℕ`
    let expected = Some([Agda.Term.Plain("ℕ")])
    let actual = Agda.Expr.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })
  it("should parse a question mark", () => {
    let raw = `?3`
    let expected = Some([Agda.Term.QuestionMark(3)])
    let actual = Agda.Expr.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse a underscore", () => {
    let raw = ` _4hi `
    let expected = Some([Agda.Term.Underscore("_4hi")])
    let actual = Agda.Expr.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })
})

describe("when running Agda.OutputConstraint.parse", () => {
  it("should parse OfType", () => {
    let raw = `x : ℕ`
    let expected = Some(Agda.OutputConstraint.OfType(RichText.string("x"), RichText.string("ℕ")))
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse OfType with a wrapped type", () => {
    // Context/meta entries ("x : T") are the most common OutputConstraint,
    // and T is exactly what Agda line-wraps when it's long -- the wrap is
    // a display artifact, not a semantic line break (#337).
    let raw = "x : A ->\n  B -> C"
    let expected = Some(
      Agda.OutputConstraint.OfType(RichText.string("x"), RichText.string("A -> B -> C")),
    )
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse JustType", () => {
    let raw = `Type ℕ`
    let expected = Some(Agda.OutputConstraint.JustType(RichText.string("ℕ")))
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse JustType with a wrapped type", () => {
    let raw = "Type A ->\n  B -> C"
    let expected = Some(Agda.OutputConstraint.JustType(RichText.string("A -> B -> C")))
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse JustSort on Windows", () => {
    // Agda line-wraps for its own fixed-width display; that wrap is a
    // display artifact, not a semantic line break, so it should collapse
    // to a single space rather than surviving as a literal "\r\n" (#337).
    let raw = "Sort ℕ\r\n  ℕ"
    let expected = Some(
      Agda.OutputConstraint.JustSort(
        RichText.string("ℕ ℕ"),
      ),
    )
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })

  it("should parse JustSort on Unix", () => {
    let raw = "Sort ℕ\n  ℕ"
    let expected = Some(
      Agda.OutputConstraint.JustSort(
        RichText.string("ℕ ℕ"),
      ),
    )
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deepStrictEqual(actual, expected)
  })
})
