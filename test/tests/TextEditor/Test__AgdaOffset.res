// open BsMocha;
open! BsMocha.Mocha
module Assert = BsMocha.Assert
module P = BsMocha.Promise

let issue7 = "module Issue7 where\n-- 𝕁\na = {!   !}" // String literal updated to v10.1.4

let openEditorWithContent = content =>
  VSCode.Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )->Promise.flatMap(textDocument =>
    VSCode.Window.showTextDocumentWithShowOptions(textDocument, None)
  )

describe("Conversion between Agda Offsets and Editor Offsets", () => {
  describe("Agda.OffsetConverter.characterWidth", () => {
    it(
      "should calculate the width of some grapheme cluster",
      () => {
        let expected = 1
        let actual = Agda.OffsetConverter.characterWidth("𝐀")
        Assert.deep_equal(actual, expected)
      },
    )
    it(
      "should calculate the width of some ordinary ASCII character",
      () => {
        let expected = 1
        let actual = Agda.OffsetConverter.characterWidth("a")
        Assert.deep_equal(actual, expected)
      },
    )
  })

  describe("Agda.OffsetConverter.computeUTF16SurrogatePairIndices", () =>
    it(
      "should work",
      () => {
        Assert.deep_equal(
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(
            "𝐀𝐀𝐀𝐀\n𝐀𝐀𝐀𝐀", // String literal updated to v10.1.4
          ),
          [0, 2, 4, 6, 9, 11, 13, 15],
        )
        Assert.deep_equal(
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(
            "𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\na", // String literal updated to v10.1.4
          ),
          [0, 3, 7, 10, 14, 17, 19, 21],
        )
        // Issue #7
        Assert.deep_equal(Agda.OffsetConverter.computeUTF16SurrogatePairIndices(issue7), [23])
      },
    )
  )

  describe("Common.Indices.make", () =>
    it(
      "should work",
      () => {
        open Agda.Indices
        Assert.deep_equal(
          "𝐀𝐀𝐀𝐀\n𝐀𝐀𝐀𝐀" // String literal updated to v10.1.4
          ->Agda.OffsetConverter.computeUTF16SurrogatePairIndices
          ->make
          ->expose
          ->fst,
          [(0, 0), (1, 1), (2, 2), (3, 3), (4, 5), (6, 6), (7, 7), (8, 8)],
        )
        Assert.deep_equal(
          "𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\na" // String literal updated to v10.1.4
          ->Agda.OffsetConverter.computeUTF16SurrogatePairIndices
          ->make
          ->expose
          ->fst,
          [(0, 0), (1, 2), (3, 5), (6, 7), (8, 10), (11, 12), (13, 13), (14, 14)],
        )
      },
    )
  )

  describe("Common.Indices.convert", () => {
    it(
      "should work",
      () => {
        open Agda.Indices
        let a = make(
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(
            "𝐀𝐀𝐀𝐀\n𝐀𝐀𝐀𝐀", // String literal updated to v10.1.4
          ),
        )
        Assert.deep_equal(convert(a, 0), 0)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 1), 2)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 2), 4)
        Assert.deep_equal(a->expose->snd, 2)
        Assert.deep_equal(convert(a, 3), 6)
        Assert.deep_equal(a->expose->snd, 3)
        Assert.deep_equal(convert(a, 0), 0)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 4), 8)
        Assert.deep_equal(convert(a, 5), 9)
        Assert.deep_equal(convert(a, 6), 11)
        Assert.deep_equal(convert(a, 7), 13)
        Assert.deep_equal(convert(a, 8), 15)
        Assert.deep_equal(convert(a, 9), 17)
        Assert.deep_equal(a->expose->snd, 8)
      },
    )

    it(
      "issue #7",
      () => {
        open Agda.Indices
        let a = make(Agda.OffsetConverter.computeUTF16SurrogatePairIndices(issue7))
        Assert.deep_equal(convert(a, 0), 0)
        Assert.deep_equal(convert(a, 6), 6)
        Assert.deep_equal(convert(a, 14), 14)
        Assert.deep_equal(convert(a, 19), 19)
        Assert.deep_equal(convert(a, 20), 20)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 24), 25)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 27), 28)
        Assert.deep_equal(convert(a, 28), 29)
        Assert.deep_equal(convert(a, 29), 30)
        Assert.deep_equal(convert(a, 36), 37)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 0), 0)
        Assert.deep_equal(a->expose->snd, 0) // the cursor should be reset to 0
        Assert.deep_equal(convert(a, 6), 6)
        Assert.deep_equal(convert(a, 7), 7)
        Assert.deep_equal(convert(a, 14), 14)
        Assert.deep_equal(convert(a, 19), 19)
        Assert.deep_equal(convert(a, 20), 20)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 24), 25)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 0), 0)
        Assert.deep_equal(a->expose->snd, 0) // the cursor should be reset to 0
      },
    )
  })

  describe("Editor.toUTF8Offset", () => {
    P.it(
      "should do it right",
      () =>
        openEditorWithContent("𝐀a𝐁bb𝐂c\\na")
        ->Promise.map(
          textEditor => {
            let f = n => textEditor->VSCode.TextEditor.document->Editor.toUTF8Offset(n)
            Assert.equal(f(0), 0)
            Assert.equal(f(1), 1) // cuts grapheme in half, toUTF8Offset is a partial function
            Assert.equal(f(2), 1)
            Assert.equal(f(3), 2)
            Assert.equal(f(5), 3)
            Assert.equal(f(6), 4)
            Assert.equal(f(7), 5)
            Assert.equal(f(9), 6)
            Assert.equal(f(10), 7)
            Assert.equal(f(11), 8)
            Assert.equal(f(12), 9)
          },
        )
        ->Promise.Js.toBsPromise,
    )

    P.it(
      "should be a left inverse of Editor.fromUTF8Offset",
      () =>
        // toUTF8Offset . fromUTF8Offset = id
        openEditorWithContent("𝐀a𝐁bb𝐂c\\na")
        ->Promise.map(
          textEditor => {
            let f = n => textEditor->VSCode.TextEditor.document->Editor.toUTF8Offset(n)
            let g = n =>
              Agda.OffsetConverter.computeUTF16SurrogatePairIndices("𝐀a𝐁bb𝐂c\\na")
              ->Agda.Indices.make
              ->Agda.Indices.convert(n)
            Assert.equal(f(g(0)), 0)
            Assert.equal(f(g(1)), 1)
            Assert.equal(f(g(2)), 2)
            Assert.equal(f(g(3)), 3)
            Assert.equal(f(g(4)), 4)
            Assert.equal(f(g(5)), 5)
            Assert.equal(f(g(6)), 6)
            Assert.equal(f(g(7)), 7)
            Assert.equal(f(g(8)), 8)
            Assert.equal(f(g(9)), 9)
          },
        )
        ->Promise.Js.toBsPromise,
    )

    P.it(
      "should be a right inverse of Editor.fromUTF8Offset ()",
      () =>
        // NOTE: toUTF8Offset is a partial function
        // fromUTF8Offset . toUTF8Offset = id
        openEditorWithContent("𝐀a𝐁bb𝐂c\\na")
        ->Promise.map(
          textEditor => {
            let f = n =>
              Agda.OffsetConverter.computeUTF16SurrogatePairIndices("𝐀a𝐁bb𝐂c\\na")
              ->Agda.Indices.make
              ->Agda.Indices.convert(n)
            let g = n => textEditor->VSCode.TextEditor.document->Editor.toUTF8Offset(n)
            Assert.equal(f(g(0)), 0)
            Assert.equal(f(g(2)), 2)
            Assert.equal(f(g(3)), 3)
            Assert.equal(f(g(5)), 5)
            Assert.equal(f(g(6)), 6)
            Assert.equal(f(g(7)), 7)
            Assert.equal(f(g(9)), 9)
          },
        )
        ->Promise.Js.toBsPromise,
    )
  })
})
