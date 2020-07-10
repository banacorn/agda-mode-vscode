// open BsMocha;
// let (it, it_skip) = BsMocha.Promise.(it, it_skip);
open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;

let openTextEditor = content =>
  Workspace.openTextDocumentWithOptions(
    Some({"content": content, "language": "agda"}),
  )
  ->Promise.flatMap(textDocument =>
      Window.showTextDocumentWithShowOptions(textDocument, None)
    );

let getTextToOffsetAt = (textEditor, offset) => {
  let (offset, _) = Editor.codeUnitEndingOffset(textEditor, offset);
  let range =
    Range.make(
      Position.make(0, 0),
      textEditor->TextEditor.document->TextDocument.positionAt(offset),
    );
  textEditor->TextEditor.document->TextDocument.getText(Some(range));
};

describe("Conversion between Offsets and Positions", () => {
  describe("Editor.characterWidth", () => {
    it("should calculate the width of some grapheme cluster", () => {
      let expected = 1;
      let actual = Editor.characterWidth({j|𝐀|j});
      Assert.deep_equal(actual, expected);
    });
    it("should calculate the width of some ordinary ASCII character", () => {
      let expected = 1;
      let actual = Editor.characterWidth({j|a|j});
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
  describe("Editor.codeUnitEndingOffset", () => {
    P.it("shouldn't cut into a grapheme", () => {
      openTextEditor({j|𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\na|j})
      ->Promise.map(textEditor => {
          Assert.equal(getTextToOffsetAt(textEditor, 0), {j||j});
          Assert.equal(getTextToOffsetAt(textEditor, 1), {j|𝐀|j});
          Assert.equal(getTextToOffsetAt(textEditor, 2), {j|𝐀|j});
          Assert.equal(getTextToOffsetAt(textEditor, 3), {j|𝐀a|j});
          Assert.equal(getTextToOffsetAt(textEditor, 4), {j|𝐀a𝐁|j});
          Assert.equal(getTextToOffsetAt(textEditor, 5), {j|𝐀a𝐁|j});
          Assert.equal(getTextToOffsetAt(textEditor, 6), {j|𝐀a𝐁b|j});
        })
      ->Promise.Js.toBsPromise
    })
  });
  describe("Editor.pointAtOffset", () => {
    let getTextToOffsetAt = (textEditor, offset) => {
      let range =
        Range.make(
          Position.make(0, 0),
          textEditor->Editor.pointAtOffset(offset),
        );
      textEditor->Editor.getTextInRange(range);
    };
    P.it("should extract the right portion of text", () => {
      openTextEditor({j|𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\na|j})
      ->Promise.map(textEditor => {
          Assert.equal(getTextToOffsetAt(textEditor, 0), {j||j});
          Assert.equal(getTextToOffsetAt(textEditor, 1), {j|𝐀|j});
          Assert.equal(getTextToOffsetAt(textEditor, 2), {j|𝐀a|j});
          Assert.equal(getTextToOffsetAt(textEditor, 3), {j|𝐀a𝐁|j});
          Assert.equal(getTextToOffsetAt(textEditor, 4), {j|𝐀a𝐁b|j});
        })
      ->Promise.Js.toBsPromise
    });
  });
});
