open Mocha
open! Test__Parser__SExpression
open Test__Util

// [SExpression] -> [Response.Prioritized.t]
let toPrioritizedResponses = exprs =>
  // keeping the successful parsing result
  // Assert.fail on the failed ones
  exprs
  ->Array.map(Response.Prioritized.parse)
  ->Array.map(x =>
    switch x {
    | Error(e) =>
      Assert.fail(Parser.Error.toString(e))
      []
    | Ok(v) => [v]
    }
  )
  ->Array.flat

describe("when parsing responses", () =>
  Golden.getGoldenFilepathsSync("../../../../test/tests/Parser/Response")->Array.forEach(filepath =>
    Async.it(
      "should golden test " ++ filepath,
      async () => {
        let raw = await Golden.readFile(filepath)
        raw
        ->Golden.map(parseSExpression([], ...))
        ->Golden.map(toPrioritizedResponses)
        ->Golden.map(Strings.unlinesWith(Response.Prioritized.toString, ...))
        ->Golden.compare
      },
    )
  )
)
