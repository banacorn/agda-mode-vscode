open Mocha
open Test__EditorLayout__Util

// Notation: `|` is left/right, `/` is top/bottom, and the number is the share
// of the parent's slot in percent. The input is the layout VS Code has made
// after adding the panel `P` next to the active group.
describe("EditorLayout.resizePair", () => {
  let check = (layout, active, expected) =>
    it(`${show(layout)}, ${active} active`, () =>
      Assert.deepStrictEqual(
        EditorLayout.resizePair(layout, ~active, ~panel="P")->show,
        expected,
      )
    )

  describe("a pair that is the whole layout", () => {
    check(col([leaf(50., "A"), leaf(50., "P")]), "A", "[A(70) / P(30)]")
  })

  describe("a nested pair in a row", () => {
    check(
      row([leaf(50., "A"), nested(50., col([leaf(50., "B"), leaf(50., "P")]))]),
      "B",
      "[A(50) | [B(70) / P(30)](50)]",
    )
    check(
      row([nested(50., col([leaf(50., "A"), leaf(50., "P")])), leaf(50., "B")]),
      "A",
      "[[A(70) / P(30)](50) | B(50)]",
    )
  })

  describe("siblings in a column", () => {
    check(
      col([leaf(50., "A"), leaf(25., "B"), leaf(25., "P")]),
      "B",
      "[A(50) / B(35) / P(15)]",
    )
    check(
      col([leaf(25., "A"), leaf(25., "P"), leaf(50., "B")]),
      "A",
      "[A(35) / P(15) / B(50)]",
    )
    check(
      col([leaf(60., "A"), leaf(20., "B"), leaf(20., "P")]),
      "B",
      "[A(60) / B(28) / P(12)]",
    )
  })

  describe("a deeper layout", () => {
    check(
      row([
        leaf(50., "A"),
        nested(
          50.,
          col([
            leaf(50., "B"),
            nested(
              50.,
              row([nested(50., col([leaf(50., "C"), leaf(50., "P")])), leaf(50., "D")]),
            ),
          ]),
        ),
      ]),
      "C",
      "[A(50) | [B(50) / [[C(70) / P(30)](50) | D(50)](50)](50)]",
    )
  })

  describe("sizes", () => {
    // sizes arrive in pixels and are turned into proportions per sibling list
    check(
      row([leaf(426., "A"), nested(426., col([leaf(213., "B"), leaf(213., "P")]))]),
      "B",
      "[A(50) | [B(70) / P(30)](50)]",
    )
    // groups outside the pair keep their shares
    check(
      row([leaf(70., "A"), nested(30., col([leaf(50., "B"), leaf(50., "P")]))]),
      "B",
      "[A(70) | [B(70) / P(30)](30)]",
    )
  })
})
