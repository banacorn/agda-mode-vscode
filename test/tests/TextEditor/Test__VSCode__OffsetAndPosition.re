// open BsMocha;
// let (it, it_skip) = BsMocha.Promise.(it, it_skip);
open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;
open VSCode;

let graphemeWidth: string => int = [%raw
  "function (string) {return [...string].length}"
];

describe_only("Conversion between Offsets and Positions", () => {
  describe("`graphemeWidth`", () => {
    it("should calculate the width of some grapheme cluster", () => {
      let expected = 1;
      let actual = graphemeWidth({j|𝐀|j});
      Assert.deep_equal(actual, expected);
    });
    it("should calculate the width of some ordinary ASCII character", () => {
      let expected = 1;
      let actual = graphemeWidth({j|a|j});
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
});
