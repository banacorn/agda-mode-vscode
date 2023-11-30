open! BsMocha.Mocha
open! Belt

module Assert = BsMocha.Assert

describe("when running Emacs__Parser2.parseGoalType", () => {
  it("should should parse goal only", () => {
    let raw = `Goal: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = [Item.Labeled("Goal", "special", RichText.string("ℕ"), None, None)]
    Assert.deep_equal(actual, expected)
  })

  it("should should parse goal + have", () => {
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

  it("should should parse goal + have + context", () => {
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

describe_only("when running Emacs__Parser2.parseAllGoalsWarnings", () => {
  it("should should parse metas", () => {
    let raw = `
?0 : ℕ
?1 : ℕ
`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals*", raw)
    let expected = [
      Item.Unlabeled(
        RichText.concatMany([RichText.hole(0), RichText.string(" : "), RichText.string("ℕ")]),
        None,
        None,
      ),
      Item.Unlabeled(
        RichText.concatMany([RichText.hole(1), RichText.string(" : "), RichText.string("ℕ")]),
        None,
        None,
      ),
    ]
    Assert.deep_equal(actual, expected)
  })

  it("should should parse goal only", () => {
    let raw = `
?0 : ℕ
?1 : ℕ
`
    let actual = Emacs__Parser2.parseAllGoalsWarnings("*All Goals*", raw)
    let expected = [
      Item.Unlabeled(
        RichText.concatMany([RichText.hole(0), RichText.string(" : "), RichText.string("ℕ")]),
        None,
        None,
      ),
      Item.Unlabeled(
        RichText.concatMany([RichText.hole(1), RichText.string(" : "), RichText.string("ℕ")]),
        None,
        None,
      ),
    ]
    Assert.deep_equal(actual, expected)
  })
})
