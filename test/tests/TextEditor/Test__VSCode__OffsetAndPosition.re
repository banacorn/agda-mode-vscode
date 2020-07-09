// open BsMocha;
// let (it, it_skip) = BsMocha.Promise.(it, it_skip);
open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;

describe_only("Conversion between Offsets and Positions", () => {
  describe("Editor.graphemeWidth", () => {
    it("should calculate the width of some grapheme cluster", () => {
      let expected = 1;
      let actual = Editor.graphemeWidth({j|𝐀|j});
      Assert.deep_equal(actual, expected);
    });
    it("should calculate the width of some ordinary ASCII character", () => {
      let expected = 1;
      let actual = Editor.graphemeWidth({j|a|j});
      Assert.deep_equal(actual, expected);
    });
  });

  describe("VSCode.TextDocument.positionAt", () => {
    let openTextDocument =
      Workspace.openTextDocumentWithOptions(
        Some({
          "content": {j|𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇\na|j},
          "language": "agda",
        }),
      );
    P.it("should count it wrong", () => {
      openTextDocument
      ->Promise.map(textDocument => {
          let range =
            Range.make(
              textDocument->VSCode.TextDocument.positionAt(0),
              textDocument->VSCode.TextDocument.positionAt(4),
            );
          let actual = textDocument->TextDocument.getText(Some(range));
          Assert.not_equal(actual, {j|𝐀𝐁𝐂𝐃|j});
          Assert.equal(actual, {j|𝐀𝐁|j});
        })
      ->Promise.Js.toBsPromise
    });
  });
  describe("Editor.pointAtOffset", () => {
    let openTextEditor =
      Workspace.openTextDocumentWithOptions(
        Some({
          "content": {j|𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇\na|j},
          "language": "agda",
        }),
      )
      ->Promise.flatMap(textDocument =>
          Window.showTextDocumentWithShowOptions(textDocument, None)
        );
    P.it("should count it right", () => {
      openTextEditor
      ->Promise.map(textEditor => {
          let range =
            Range.make(
              textEditor->Editor.pointAtOffset(0),
              textEditor->Editor.pointAtOffset(4),
            );
          let actual = textEditor->Editor.getTextInRange(range);
          Js.log(
            textEditor
            ->VSCode.TextEditor.document
            ->VSCode.TextDocument.positionAt(4),
          );

          //   Js.log(textEditor->Editor.pointAtOffset(4));
          //   Assert.not_equal(actual, {j|𝐀𝐁|j});
          Assert.equal(actual, {j|𝐀𝐁𝐂𝐃|j});
        })
      ->Promise.Js.toBsPromise
    });
  });
});
