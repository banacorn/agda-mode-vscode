open! BsMocha.Mocha
module Assert = BsMocha.Assert
module P = BsMocha.Promise

open VSCode

let openEditorWithContent = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )->Promise.flatMap(textDocument => Window.showTextDocumentWithShowOptions(textDocument, None))

describe("Conversion of offsets between LF and CRLF line endings", () => {
  describe("Editor.computeCRLFIndices", () =>
    it(
      "should work",
      () => {
        Assert.deep_equal(Agda.OffsetConverter.computeCRLFIndices("1234\r\n78"), [4]) // String literal updated to v10.1.4
        Assert.deep_equal(Agda.OffsetConverter.computeCRLFIndices("12\r\n56\r\n90"), [2, 6]) // String literal updated to v10.1.4
      },
    )
  )

  describe("Editor.Indices.make", () =>
    it(
      "should work",
      () => {
        open Agda.Indices
        ()
        Assert.deep_equal(
          "12\r\n56\r\n90"->Agda.OffsetConverter.computeCRLFIndices->make->expose->fst, // String literal updated to v10.1.4
          [(0, 2), (3, 5)],
        )
      },
    )
  )

  describe("Editor.Indices.convert", () =>
    it(
      "should work",
      () => {
        open Agda.Indices
        let a = make(Agda.OffsetConverter.computeCRLFIndices("12\r\n56\r\n90")) // String literal updated to v10.1.4
        Assert.deep_equal(convert(a, 0), 0)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 1), 1)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 2), 2)
        Assert.deep_equal(a->expose->snd, 0)
        Assert.deep_equal(convert(a, 3), 4)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 4), 5)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 5), 6)
        Assert.deep_equal(a->expose->snd, 1)
        Assert.deep_equal(convert(a, 6), 8)
        Assert.deep_equal(a->expose->snd, 2)
        Assert.deep_equal(convert(a, 7), 9)
        Assert.deep_equal(a->expose->snd, 2)
      },
    )
  )
})
