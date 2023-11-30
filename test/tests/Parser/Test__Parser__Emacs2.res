open! BsMocha.Mocha
open! Belt

module Assert = BsMocha.Assert

describe_only("when running Emacs__Parser2.parseGoalType", () => {
  it("should should parse goal type", () => {
    let raw = `Goal: ℕ
————————————————————————————————————————————————————————————`
    let actual = Emacs__Parser2.parseGoalType(raw)
    let expected = [Item.Labeled("GOAL", "style", RichText.string("ℕ"), Some("ℕ"), None)]
    Assert.deep_equal(actual, expected)
  })
})
