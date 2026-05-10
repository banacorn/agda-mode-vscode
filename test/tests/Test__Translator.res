open Mocha

describe("Input Method Translator", () => {
  describe("filtered keymap", () => {
    it("should filter line-break candidates from prefix nodes", () => {
      let translation = Translator.translate("par", None)

      Assert.equal(None, translation.symbol)
      Assert.equal(true, translation.further)
      Assert.equal(false, translation.candidateSymbols->Array.includes("\n"))
      Assert.equal(true, translation.keySuggestions->Array.includes("a"))
      Assert.equal(true, translation.keySuggestions->Array.includes("t"))
    })

    it("should keep line-break candidates on leaf nodes", () => {
      let translation = Translator.translate("newline", None)

      Assert.equal(Some("\n"), translation.symbol)
      Assert.equal(false, translation.further)
      Assert.equal(true, translation.candidateSymbols->Array.includes("\n"))
    })

    it("should keep longer continuations below filtered prefix nodes", () => {
      Assert.equal(Some("∂"), Translator.translate("partial", None).symbol)
      Assert.equal(Some("∥"), Translator.translate("parallel", None).symbol)
    })

    it("should filter reverse lookup sequences with the same rule", () => {
      switch Translator.lookup("\n") {
      | Some(sequences) =>
        Assert.equal(true, sequences->Array.includes("newline"))
        Assert.equal(false, sequences->Array.includes("par"))
      | None => Assert.fail("expected lookup sequences for newline")
      }
    })
  })
})
