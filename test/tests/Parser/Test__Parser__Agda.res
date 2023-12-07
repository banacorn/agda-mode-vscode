open! BsMocha.Mocha
open! Belt

module Assert = BsMocha.Assert

describe("when running Agda.Expr.parse", () => {
  it("should should parse a plain string", () => {
    let raw = `ℕ`
    let expected = Some([Agda.Term.Plain("ℕ")])
    let actual = Agda.Expr.parse(raw)
    Assert.deep_equal(actual, expected)
  })
  it("should should parse a question mark", () => {
    let raw = `?3`
    let expected = Some([Agda.Term.QuestionMark(3)])
    let actual = Agda.Expr.parse(raw)
    Assert.deep_equal(actual, expected)
  })

  it("should should parse a underscore", () => {
    let raw = ` _4hi `
    let expected = Some([Agda.Term.Underscore("_4hi")])
    let actual = Agda.Expr.parse(raw)
    Assert.deep_equal(actual, expected)
  })
})

describe("when running Agda.OutputConstraint.parse", () => {
  it("should should parse OfType", () => {
    let raw = `x : ℕ`
    let expected = Some(Agda.OutputConstraint.OfType(RichText.string("x"), RichText.string("ℕ")))
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deep_equal(actual, expected)
  })

  it("should should parse JustType", () => {
    let raw = `Type ℕ`
    let expected = Some(Agda.OutputConstraint.JustType(RichText.string("ℕ")))
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deep_equal(actual, expected)
  })

  it("should should parse JustSort", () => {
    let raw = `Sort ℕ
  ℕ`
    let expected = Some(
      Agda.OutputConstraint.JustSort(
        RichText.string(`ℕ
  ℕ`),
      ),
    )
    let actual = Agda.OutputConstraint.parse(raw)
    Assert.deep_equal(actual, expected)
  })
})
