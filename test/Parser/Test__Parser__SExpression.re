open BsMocha.Mocha;
open Js.Promise;
open Test__Util;

open Belt;

// [Int] -> String -> [SExpression]
let parseSExpression = (breakpoints, input) => {
  open Parser.Incr.Event;

  let output = ref([||]);

  let parser =
    Parser.SExpression.makeIncr(
      fun
      | Yield(Rebase.Error(err)) =>
        Assert.fail(
          "Failed when parsing S-expression: " ++ Parser.Error.toString(err),
        )
      | Yield(Rebase.Ok(a)) => Js.Array.push(a, output^) |> ignore
      | Stop => (),
    );

  input
  ->Js.String.trim
  ->breakInput(breakpoints)
  ->Array.map(Parser.split)
  ->Array.concatMany
  ->Array.forEach(Parser.Incr.feed(parser));

  output^;
};

describe("when parsing S-expressions wholly", () =>
  Golden.getGoldenFilepathsSync("test/Parser/SExpression")
  ->Array.forEach(filepath =>
      BsMocha.Promise.it("should golden test " ++ filepath, () =>
        Golden.readFile(filepath)
        |> then_(raw =>
             raw
             ->Golden.map(parseSExpression([||]))
             ->Golden.map(serializeWith(Parser.SExpression.toString))
             ->Golden.compare
           )
      )
    )
);

describe("when parsing S-expressions incrementally", () =>
  Golden.getGoldenFilepathsSync("test/Parser/SExpression")
  ->Array.forEach(filepath =>
      BsMocha.Promise.it("should golden test " ++ filepath, () =>
        Golden.readFile(filepath)
        |> then_(raw =>
             raw
             ->Golden.map(
                 parseSExpression([|3, 23, 171, 217, 1234, 2342, 3453|]),
               )
             ->Golden.map(serializeWith(Parser.SExpression.toString))
             ->Golden.compare
           )
      )
    )
);