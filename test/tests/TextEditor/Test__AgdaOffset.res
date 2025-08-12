open Mocha

let issue7 = "module Issue7 where\n-- 𝕁\na = {!   !}" // String literal updated to v10.1.4

let openEditorWithContent = async content => {
  let textDocument = await VSCode.Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  await VSCode.Window.showTextDocumentWithShowOptions(textDocument, None)
}

describe("Conversion between Agda Offsets and Editor Offsets", () => {
  describe("Agda.OffsetConverter.characterWidth", () => {
    it(
      "should calculate the width of some grapheme cluster",
      () => {
        let expected = 1
        let actual = Agda.OffsetConverter.characterWidth("𝐀")
        Assert.deepStrictEqual(actual, expected)
      },
    )
    it(
      "should calculate the width of some ordinary ASCII character",
      () => {
        let expected = 1
        let actual = Agda.OffsetConverter.characterWidth("a")
        Assert.deepStrictEqual(actual, expected)
      },
    )
  })

  describe("Agda.OffsetConverter.computeUTF16SurrogatePairIndices", () => {
    it(
      "should work",
      () => {
        Assert.deepStrictEqual(
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(
            "𝐀𝐀𝐀𝐀\n𝐀𝐀𝐀𝐀", // String literal updated to v10.1.4
          ),
          [0, 2, 4, 6, 9, 11, 13, 15],
        )
        Assert.deepStrictEqual(
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices(
            "𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\na", // String literal updated to v10.1.4
          ),
          [0, 3, 7, 10, 14, 17, 19, 21],
        )
        // Issue #7
        Assert.deepStrictEqual(Agda.OffsetConverter.computeUTF16SurrogatePairIndices(issue7), [23])
      },
    )
  })

  describe("Common.Indices.make", () =>
    it(
      "should work",
      () => {
        open Agda.Indices
        Assert.deepStrictEqual(
          "𝐀𝐀𝐀𝐀\n𝐀𝐀𝐀𝐀" // String literal updated to v10.1.4
          ->Agda.OffsetConverter.computeUTF16SurrogatePairIndices
          ->make
          ->expose
          ->fst,
          [(0, 0), (1, 1), (2, 2), (3, 3), (4, 5), (6, 6), (7, 7), (8, 8)],
        )
        Assert.deepStrictEqual(
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
        Assert.deepStrictEqual(convert(a, 0), 0)
        Assert.deepStrictEqual(a->expose->snd, 0)
        Assert.deepStrictEqual(convert(a, 1), 2)
        Assert.deepStrictEqual(a->expose->snd, 1)
        Assert.deepStrictEqual(convert(a, 2), 4)
        Assert.deepStrictEqual(a->expose->snd, 2)
        Assert.deepStrictEqual(convert(a, 3), 6)
        Assert.deepStrictEqual(a->expose->snd, 3)
        Assert.deepStrictEqual(convert(a, 0), 0)
        Assert.deepStrictEqual(a->expose->snd, 0)
        Assert.deepStrictEqual(convert(a, 4), 8)
        Assert.deepStrictEqual(convert(a, 5), 9)
        Assert.deepStrictEqual(convert(a, 6), 11)
        Assert.deepStrictEqual(convert(a, 7), 13)
        Assert.deepStrictEqual(convert(a, 8), 15)
        Assert.deepStrictEqual(convert(a, 9), 17)
        Assert.deepStrictEqual(a->expose->snd, 8)
      },
    )

    it(
      "issue #7",
      () => {
        open Agda.Indices
        let a = make(Agda.OffsetConverter.computeUTF16SurrogatePairIndices(issue7))
        Assert.deepStrictEqual(convert(a, 0), 0)
        Assert.deepStrictEqual(convert(a, 6), 6)
        Assert.deepStrictEqual(convert(a, 14), 14)
        Assert.deepStrictEqual(convert(a, 19), 19)
        Assert.deepStrictEqual(convert(a, 20), 20)
        Assert.deepStrictEqual(a->expose->snd, 0)
        Assert.deepStrictEqual(convert(a, 24), 25)
        Assert.deepStrictEqual(a->expose->snd, 1)
        Assert.deepStrictEqual(convert(a, 27), 28)
        Assert.deepStrictEqual(convert(a, 28), 29)
        Assert.deepStrictEqual(convert(a, 29), 30)
        Assert.deepStrictEqual(convert(a, 36), 37)
        Assert.deepStrictEqual(a->expose->snd, 1)
        Assert.deepStrictEqual(convert(a, 0), 0)
        Assert.deepStrictEqual(a->expose->snd, 0) // the cursor should be reset to 0
        Assert.deepStrictEqual(convert(a, 6), 6)
        Assert.deepStrictEqual(convert(a, 7), 7)
        Assert.deepStrictEqual(convert(a, 14), 14)
        Assert.deepStrictEqual(convert(a, 19), 19)
        Assert.deepStrictEqual(convert(a, 20), 20)
        Assert.deepStrictEqual(a->expose->snd, 0)
        Assert.deepStrictEqual(convert(a, 24), 25)
        Assert.deepStrictEqual(a->expose->snd, 1)
        Assert.deepStrictEqual(convert(a, 0), 0)
        Assert.deepStrictEqual(a->expose->snd, 0) // the cursor should be reset to 0
      },
    )
  })

  describe("Editor.toCodepointOffset", () => {
    Async.it(
      "should do it right",
      async () => {
        let textEditor = await openEditorWithContent("𝐀a𝐁bb𝐂c\\na")
        let f = n => textEditor->VSCode.TextEditor.document->Editor.toCodepointOffset(n)
        Assert.equal(f(0), 0)
        Assert.equal(f(1), 1) // cuts grapheme in half, toCodepointOffset is a partial function
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

    Async.it(
      "should be a left inverse of Editor.fromCodepointOffset",
      async () => {
        // toCodepointOffset . fromCodepointOffset = id
        let textEditor = await openEditorWithContent("𝐀a𝐁bb𝐂c\\na")
        let f = n => textEditor->VSCode.TextEditor.document->Editor.toCodepointOffset(n)
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

    Async.it(
      "should be a right inverse of Editor.fromCodepointOffset ()",
      async () => {
        // NOTE: toCodepointOffset is a partial function
        // fromCodepointOffset . toCodepointOffset = id
        let textEditor = await openEditorWithContent("𝐀a𝐁bb𝐂c\\na")
        let f = n =>
          Agda.OffsetConverter.computeUTF16SurrogatePairIndices("𝐀a𝐁bb𝐂c\\na")
          ->Agda.Indices.make
          ->Agda.Indices.convert(n)
        let g = n => textEditor->VSCode.TextEditor.document->Editor.toCodepointOffset(n)
        Assert.equal(f(g(0)), 0)
        Assert.equal(f(g(2)), 2)
        Assert.equal(f(g(3)), 3)
        Assert.equal(f(g(5)), 5)
        Assert.equal(f(g(6)), 6)
        Assert.equal(f(g(7)), 7)
        Assert.equal(f(g(9)), 9)
      },
    )
  })
})
