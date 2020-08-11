open Belt;
open BsMocha.Mocha;
open BsMocha;
open Js.Promise;

module Impl = (Editor: Sig.Editor) => {
  module Test__Util = Test__Util.Impl(Editor);
  open Test__Util;

  describe("when parsing file paths", () =>
    it("should recognize the file extensions", () => {
      open SourceFile.FileType;
      Assert.equal(parse("a.agda"), Agda);
      Assert.equal(parse("a.lagda"), LiterateTeX);
      Assert.equal(parse("a.lagda.tex"), LiterateTeX);
      Assert.equal(parse("a.lagda.md"), LiterateMarkdown);
      Assert.equal(parse("a.lagda.rst"), LiterateRST);
      Assert.equal(parse("a.lagda.org"), LiterateOrg);
    })
  );

  if (onUnix()) {
    describe("when parsing source files (Unix only)", () =>
      Golden.getGoldenFilepathsSync(
        "../../../../test/tests/Parser/SourceFile",
      )
      ->Array.forEach(filepath =>
          BsMocha.Promise.it("should golden test " ++ filepath, () =>
            Golden.readFile(filepath)
            |> then_(raw =>
                 raw
                 ->Golden.map(
                     SourceFile.parse(
                       [|0, 1, 2, 3, 4, 5, 6, 7, 8, 9|],
                       filepath,
                     ),
                   )
                 ->Golden.map(
                     Strings.serializeWith(SourceFile.Diff.toString),
                   )
                 ->Golden.compare
               )
          )
        )
    );
  };
};

include Impl(SigImpl);
