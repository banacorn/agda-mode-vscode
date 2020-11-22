open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;

open VSCode;

let openEditorWithContent = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );

describe_only("Conversion of offsets between LF and CRLF line endings", () => {
  describe("Editor.computeCRLFIndices", () => {
    it("should work", () => {
      Assert.deep_equal(Editor.computeCRLFIndices({j|1234\r\n78|j}), [|4|]);
      Assert.deep_equal(
        Editor.computeCRLFIndices({j|12\r\n56\r\n90|j}),
        [|2, 6|],
      );
    })
  })
});
