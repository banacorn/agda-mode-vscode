open Mocha
open Test__Util

describe("Tokens", () => {
  This.timeout(10000)
  describe("GotoDefinition.agda", () => {
    Async.it(
      "should produce 28 tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->Tokens.toArray
          ->Array.map(
            ((token, range)) => Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token),
          )
        Assert.deepEqual(Array.length(tokens), 28)
      },
    )

    Async.it(
      "should produce correct tokens",
      async () => {
        let ctx = await AgdaMode.makeAndLoad("GotoDefinition.agda")
        let tokens =
          ctx.state.tokens
          ->Tokens.toArray
          ->Array.map(
            ((token, range)) => Editor.Range.toString(range) ++ " " ++ Tokens.Token.toString(token),
          )

        let srcOfPrimitive = switch ctx.state.agdaVersion {
        | Some(version) =>
          if Util.Version.gte(version, "2.6.4") {
            "[src: 388]"
          } else {
            "[src: 320]"
          }
        | None => raise(Failure("No Agda version found"))
        }

        Assert.deepEqual(
          tokens,
          [
            "0:0-6 Token (0, 6) [Keyword]",
            "0:7-21 Token (7, 21) [Module] [src: 1]",
            "0:22-27 Token (22, 27) [Keyword]",
            "1:0-4 Token (28, 32) [Keyword]",
            "1:5-6 Token (33, 34) [Datatype] [src: 34]",
            "1:7-8 Token (35, 36) [Symbol]",
            "1:9-12 Token (37, 40) [Primitive] " ++ srcOfPrimitive,
            "1:13-18 Token (41, 46) [Keyword]",
            "2:2-3 Token (49, 50) [ConstructorInductive] [src: 50]",
            "2:4-5 Token (51, 52) [Symbol]",
            "2:6-7 Token (53, 54) [Datatype] [src: 34]",
            "3:2-3 Token (57, 58) [ConstructorInductive] [src: 58]",
            "3:4-5 Token (59, 60) [Symbol]",
            "3:6-7 Token (61, 62) [Datatype] [src: 34]",
            "3:8-9 Token (63, 64) [Symbol]",
            "3:10-11 Token (65, 66) [Datatype] [src: 34]",
            "5:0-3 Token (68, 71) [Function, Operator] [src: 69]",
            "5:4-5 Token (72, 73) [Symbol]",
            "5:6-7 Token (74, 75) [Datatype] [src: 34]",
            "5:8-9 Token (76, 77) [Symbol]",
            "5:10-11 Token (78, 79) [Datatype] [src: 34]",
            "5:12-13 Token (80, 81) [Symbol]",
            "5:14-15 Token (82, 83) [Datatype] [src: 34]",
            "6:0-1 Token (84, 85) [Bound] [src: 85]",
            "6:2-3 Token (86, 87) [Function, Operator] [src: 69]",
            "6:4-5 Token (88, 89) [Bound] [src: 89]",
            "6:6-7 Token (90, 91) [Symbol]",
            "6:8-15 Token (92, 99) [Hole]",
          ],
        )
      },
    )
  })
})
