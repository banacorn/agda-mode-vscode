open Mocha

// Issue #417: choosing a candidate must replace the old symbol at its range in the document
// before the edit, and the next choice must use the ranges the symbols have after it.
// Without an editor, as in the prompt, ranges are columns of line 0.
describe("Input Method — candidate replacement ranges", () => {
  // the ranges and symbols of the rewrite made by choosing `symbol`, completed as clean
  let choose = (im, symbol) =>
    IM.run(im, None, Candidate(ChooseSymbol(symbol)))->Array.filterMap(output =>
      switch output {
      | IM.Output.Rewrite(replacements, complete) =>
        complete(true)
        Some(
          replacements->Array.map(((range, text)) => (
            (
              range->VSCode.Range.start->VSCode.Position.character,
              range->VSCode.Range.end_->VSCode.Position.character,
            ),
            text,
          )),
        )
      | _ => None
      }
    )

  // activates on `intervals`, chooses `first` and then `second`, and returns both rewrites
  let chooseTwice = (intervals, first, second) => {
    let im = IM.make(Chan.make())
    let _ = IM.run(im, None, Activate(intervals))
    let rewrites = (choose(im, first), choose(im, second))
    let _ = IM.run(im, None, Deactivate)
    rewrites
  }

  it("should replace only the old symbol when the new one is longer", () => {
    Assert.deepStrictEqual(
      chooseTwice([(2, 3)], "🅐", "ⓐ"),
      ([[((2, 3), "🅐")]], [[((2, 4), "ⓐ")]]),
    )
  })

  it("should replace the whole old symbol when the new one is shorter", () => {
    Assert.deepStrictEqual(
      chooseTwice([(2, 4)], "ⓐ", "🅐"),
      ([[((2, 4), "ⓐ")]], [[((2, 3), "🅐")]]),
    )
  })

  it("should replace the old symbol when the new one has the same length", () => {
    Assert.deepStrictEqual(
      chooseTwice([(2, 4)], "🅐", "🄰"),
      ([[((2, 4), "🅐")]], [[((2, 4), "🄰")]]),
    )
  })

  it("should replace the old symbols in the coordinates before the edit when the new ones are longer, at two cursors", () => {
    Assert.deepStrictEqual(
      chooseTwice([(2, 3), (6, 7)], "🅐", "ⓐ"),
      ([[((2, 3), "🅐"), ((6, 7), "🅐")]], [[((2, 4), "ⓐ"), ((7, 9), "ⓐ")]]),
    )
  })

  it("should replace the old symbols in the coordinates before the edit when the new ones are shorter, at two cursors", () => {
    Assert.deepStrictEqual(
      chooseTwice([(2, 4), (7, 9)], "ⓐ", "🅐"),
      ([[((2, 4), "ⓐ"), ((7, 9), "ⓐ")]], [[((2, 3), "🅐"), ((6, 7), "🅐")]]),
    )
  })
})
