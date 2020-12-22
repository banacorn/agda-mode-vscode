// open BsMocha;
open! BsMocha.Mocha
module Assert = BsMocha.Assert
module P = BsMocha.Promise

open Common

let openEditorWithContent = content =>
  VSCode.Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )->Promise.flatMap(textDocument =>
    VSCode.Window.showTextDocumentWithShowOptions(textDocument, None)
  )

describe("Conversion between Agda Offsets and Editor Offsets", () => {
  describe("Common.Agda.OffsetConverter.characterWidth", () => {
    it("should calculate the width of some grapheme cluster", () => {
      let expected = 1
      let actual = Agda.OffsetConverter.characterWidth(j`𝐀`)
      Assert.deep_equal(actual, expected)
    })
    it("should calculate the width of some ordinary ASCII character", () => {
      let expected = 1
      let actual = Agda.OffsetConverter.characterWidth(j`a`)
      Assert.deep_equal(actual, expected)
    })
  })

  describe("Common.Agda.OffsetConverter.computeUTF16SurrogatePairIndices", () =>
    it("should work", () => {
      Assert.deep_equal(
        Agda.OffsetConverter.computeUTF16SurrogatePairIndices(j`𝐀𝐀𝐀𝐀\\n𝐀𝐀𝐀𝐀`),
        [0, 2, 4, 6, 9, 11, 13, 15],
      )
      Assert.deep_equal(
        Agda.OffsetConverter.computeUTF16SurrogatePairIndices(j`𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\\na`),
        [0, 3, 7, 10, 14, 17, 19, 21],
      )
    })
  )

  describe("Common.Indices.make", () =>
    it("should work", () => {
      open Indices
      Assert.deep_equal(
        j`𝐀𝐀𝐀𝐀\\n𝐀𝐀𝐀𝐀`
        ->Agda.OffsetConverter.computeUTF16SurrogatePairIndices
        ->make
        ->expose
        ->fst,
        [(0, 0), (1, 1), (2, 2), (3, 3), (4, 5), (6, 6), (7, 7), (8, 8)],
      )
      Assert.deep_equal(
        j`𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\\na`
        ->Agda.OffsetConverter.computeUTF16SurrogatePairIndices
        ->make
        ->expose
        ->fst,
        [(0, 0), (1, 2), (3, 5), (6, 7), (8, 10), (11, 12), (13, 13), (14, 14)],
      )
    })
  )

  describe("Common.Indices.convert", () =>
    it("should work", () => {
      open Indices
      let a = make(
        Agda.OffsetConverter.computeUTF16SurrogatePairIndices(j`𝐀𝐀𝐀𝐀\\n𝐀𝐀𝐀𝐀`),
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
    })
  )

  describe("Editor.toUTF8Offset", () => {
    P.it("should do it right", () =>
      openEditorWithContent(j`𝐀a𝐁bb𝐂c\\na`)
      ->Promise.map(textEditor => {
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
      })
      ->Promise.Js.toBsPromise
    )

    P.it("should be a left inverse of Editor.fromUTF8Offset", () =>
      // toUTF8Offset . fromUTF8Offset = id
      openEditorWithContent(j`𝐀a𝐁bb𝐂c\\na`)
      ->Promise.map(textEditor => {
        let f = n => textEditor->VSCode.TextEditor.document->Editor.toUTF8Offset(n)
        let g = n =>
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(j`𝐀a𝐁bb𝐂c\\na`)
          ->Common.Indices.make
          ->Common.Indices.convert(n)
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
      })
      ->Promise.Js.toBsPromise
    )

    P.it("should be a right inverse of Editor.fromUTF8Offset ()", () =>
      // NOTE: toUTF8Offset is a partial function
      // fromUTF8Offset . toUTF8Offset = id
      openEditorWithContent(j`𝐀a𝐁bb𝐂c\\na`)
      ->Promise.map(textEditor => {
        let f = n =>
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(j`𝐀a𝐁bb𝐂c\\na`)
          ->Common.Indices.make
          ->Common.Indices.convert(n)
        let g = n => textEditor->VSCode.TextEditor.document->Editor.toUTF8Offset(n)
        Assert.equal(f(g(0)), 0)
        Assert.equal(f(g(2)), 2)
        Assert.equal(f(g(3)), 3)
        Assert.equal(f(g(5)), 5)
        Assert.equal(f(g(6)), 6)
        Assert.equal(f(g(7)), 7)
        Assert.equal(f(g(9)), 9)
      })
      ->Promise.Js.toBsPromise
    )
  })
})
