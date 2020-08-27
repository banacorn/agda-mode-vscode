// open BsMocha;
// let (it, it_skip) = BsMocha.Promise.(it, it_skip);
open! BsMocha.Mocha;
module Assert = BsMocha.Assert;
module P = BsMocha.Promise;

module Impl = (Editor: Sig.Editor) => {
  describe("Conversion between Agda Offsets and Editor Offsets", () => {
    describe("Sig.characterWidth", () => {
      it("should calculate the width of some grapheme cluster", () => {
        let expected = 1;
        let actual = Sig.characterWidth({j|𝐀|j});
        Assert.deep_equal(actual, expected);
      });
      it("should calculate the width of some ordinary ASCII character", () => {
        let expected = 1;
        let actual = Sig.characterWidth({j|a|j});
        Assert.deep_equal(actual, expected);
      });
    });

    describe("SigImpl.normalizeUTF16Offset", () => {
      let getTextToOffsetAt = (textEditor, from, to_) => {
        let from =
          SigImpl.normalizeUTF16Offset(textEditor, {utf8: 0, utf16: 0}, from).
            utf16;
        let to_ =
          SigImpl.normalizeUTF16Offset(textEditor, {utf8: 0, utf16: 0}, to_).
            utf16;
        let range =
          SigImpl.Range.make(
            SigImpl.pointAtOffset(textEditor, from),
            SigImpl.pointAtOffset(textEditor, to_),
          );
        textEditor->SigImpl.getTextInRange(range);
      };
      P.it("shouldn't cut into a grapheme", () => {
        SigImpl.openEditorWithContent(
          {j|𝐀a𝐁bb𝐂c𝐃dd𝐄e𝐅𝐆𝐇\na|j},
        )
        ->Promise.map(textEditor => {
            Assert.equal(getTextToOffsetAt(textEditor, 0, 0), {j||j});
            Assert.equal(getTextToOffsetAt(textEditor, 0, 1), {j|𝐀|j});
            Assert.equal(getTextToOffsetAt(textEditor, 0, 2), {j|𝐀|j});
            Assert.equal(getTextToOffsetAt(textEditor, 0, 3), {j|𝐀a|j});
            Assert.equal(
              getTextToOffsetAt(textEditor, 0, 4),
              {j|𝐀a𝐁|j},
            );
            Assert.equal(
              getTextToOffsetAt(textEditor, 0, 5),
              {j|𝐀a𝐁|j},
            );
            Assert.equal(
              getTextToOffsetAt(textEditor, 0, 6),
              {j|𝐀a𝐁b|j},
            );
            Assert.equal(getTextToOffsetAt(textEditor, 1, 6), {j|a𝐁b|j});
            Assert.equal(getTextToOffsetAt(textEditor, 2, 6), {j|a𝐁b|j});
            Assert.equal(getTextToOffsetAt(textEditor, 3, 6), {j|𝐁b|j});
            Assert.equal(getTextToOffsetAt(textEditor, 4, 6), {j|b|j});
            Assert.equal(getTextToOffsetAt(textEditor, 5, 6), {j|b|j});
          })
        ->Promise.Js.toBsPromise
      });
    });

    describe("Editor.fromUTF8Offset", () => {
      P.it("should do it right", () => {
        Editor.openEditorWithContent({j|𝐀a𝐁bb𝐂c\na|j})
        ->Promise.map(textEditor => {
            let f = n => textEditor->Editor.fromUTF8Offset(None, n);
            Assert.equal(f(0), 0);
            Assert.equal(f(1), 2);
            Assert.equal(f(2), 3);
            Assert.equal(f(3), 5);
            Assert.equal(f(4), 6);
            Assert.equal(f(5), 7);
            Assert.equal(f(6), 9);
            Assert.equal(f(7), 10);
            Assert.equal(f(8), 11);
            Assert.equal(f(9), 14);
          })
        ->Promise.Js.toBsPromise
      });

      P.it("should extract the right portion of text", () => {
        let getTextToOffsetAt = (textEditor, offset) => {
          let offset = textEditor->Editor.fromUTF8Offset(None, offset);
          let range =
            Editor.Range.make(
              Editor.Point.make(0, 0),
              Editor.pointAtOffset(textEditor, offset),
            );
          textEditor->Editor.getTextInRange(range);
        };
        Editor.openEditorWithContent({j|𝐀a𝐁bb𝐂c\na|j})
        ->Promise.map(textEditor => {
            Assert.equal(getTextToOffsetAt(textEditor, 0), {j||j});
            Assert.equal(getTextToOffsetAt(textEditor, 1), {j|𝐀|j});
            Assert.equal(getTextToOffsetAt(textEditor, 2), {j|𝐀a|j});
            Assert.equal(getTextToOffsetAt(textEditor, 3), {j|𝐀a𝐁|j});
            Assert.equal(getTextToOffsetAt(textEditor, 4), {j|𝐀a𝐁b|j});
            Assert.equal(
              getTextToOffsetAt(textEditor, 5),
              {j|𝐀a𝐁bb|j},
            );
            Assert.equal(
              getTextToOffsetAt(textEditor, 6),
              {j|𝐀a𝐁bb𝐂|j},
            );
            Assert.equal(
              getTextToOffsetAt(textEditor, 7),
              {j|𝐀a𝐁bb𝐂c|j},
            );
            Assert.equal(
              getTextToOffsetAt(textEditor, 8),
              {j|𝐀a𝐁bb𝐂c\n|j},
            );
            Assert.equal(
              getTextToOffsetAt(textEditor, 9),
              {j|𝐀a𝐁bb𝐂c\na|j},
            );
          })
        ->Promise.Js.toBsPromise;
      });
    });
    describe("Editor.toUTF8Offset", () => {
      P.it("should do it right", () => {
        Editor.openEditorWithContent({j|𝐀a𝐁bb𝐂c\na|j})
        ->Promise.map(textEditor => {
            let f = n => textEditor->Editor.toUTF8Offset(n);
            Assert.equal(f(0), 0);
            Assert.equal(f(1), 1); // cuts grapheme in half, toUTF8Offset is a partial function
            Assert.equal(f(2), 1);
            Assert.equal(f(3), 2);
            Assert.equal(f(5), 3);
            Assert.equal(f(6), 4);
            Assert.equal(f(7), 5);
            Assert.equal(f(9), 6);
            Assert.equal(f(10), 7);
            Assert.equal(f(11), 8);
            Assert.equal(f(12), 9);
          })
        ->Promise.Js.toBsPromise
      });

      P.it("should be a left inverse of Editor.fromUTF8Offset", () => {
        // toUTF8Offset . fromUTF8Offset = id
        Editor.openEditorWithContent({j|𝐀a𝐁bb𝐂c\na|j})
        ->Promise.map(textEditor => {
            let f = n => textEditor->Editor.toUTF8Offset(n);
            let g = n => textEditor->Editor.fromUTF8Offset(None, n);
            Assert.equal(f(g(0)), 0);
            Assert.equal(f(g(1)), 1);
            Assert.equal(f(g(2)), 2);
            Assert.equal(f(g(3)), 3);
            Assert.equal(f(g(4)), 4);
            Assert.equal(f(g(5)), 5);
            Assert.equal(f(g(6)), 6);
            Assert.equal(f(g(7)), 7);
            Assert.equal(f(g(8)), 8);
            Assert.equal(f(g(9)), 9);
          })
        ->Promise.Js.toBsPromise
      });

      P.it("should be a right inverse of Editor.fromUTF8Offset ()", () => {
        // NOTE: toUTF8Offset is a partial function
        // fromUTF8Offset . toUTF8Offset = id
        Editor.openEditorWithContent({j|𝐀a𝐁bb𝐂c\na|j})
        ->Promise.map(textEditor => {
            let f = n => textEditor->Editor.fromUTF8Offset(None, n);
            let g = n => textEditor->Editor.toUTF8Offset(n);
            Assert.equal(f(g(0)), 0);
            Assert.equal(f(g(2)), 2);
            Assert.equal(f(g(3)), 3);
            Assert.equal(f(g(5)), 5);
            Assert.equal(f(g(6)), 6);
            Assert.equal(f(g(7)), 7);
            Assert.equal(f(g(9)), 9);
          })
        ->Promise.Js.toBsPromise
      });
    });
  });
};

include Impl(SigImpl);
